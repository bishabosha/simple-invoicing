#! /usr/bin/env scala shebang
//> using scala 3.8.1
//> using toolkit 0.9.1
//> using dep org.apache.pdfbox:pdfbox:3.0.7
//> using dep "io.github.bishabosha::enhanced-string-interpolator:1.0.2"
//> using dep io.github.bishabosha::scala-object-notation:0.1.0
//> using dep ch.epfl.lamp::steps::0.2.0
//> using file Configs.scala

object Logger:
  def info(msg: String) =
    Console.err.println(msg.linesWithSeparators.map(l => s"[INFO] $l").mkString)
  def error(msg: String) =
    Console.err.println(
      msg.linesWithSeparators.map(l => s"[Error] $l").mkString
    )

def printUsageAndExit(): Nothing =
  Logger.error("args: [--inconsolata] <config-file>")
  sys.exit(0)

val (useInconsolata, configPath) =
  if args.contains("--inconsolata") then
    args.filterNot(_ == "--inconsolata") match
      case Array(configPath) => (true, configPath)
      case _                 => printUsageAndExit()
  else
    if args.sizeIs != 1 then printUsageAndExit()
    (false, args(0))

/// computed values

Logger.info(s"Begin - config file: ${configPath}")
val conf = configs.readConfig(os.rel / os.RelPath(configPath))
Logger.info("parsed config")

import scala.math.BigDecimal.RoundingMode
import java.time.LocalDate
import stringmatching.regex.Interpolators.r

val invoiceCode = "INV" + (10_000 * conf.client.id + conf.invoice.id)

val optTaxRate: Option[BigDecimal] =
  Option.when(conf.listings.taxRate > 0)(
    BigDecimal(conf.listings.taxRate) / 100
  )

val startDate =
  val r"$year%d/$month%d/$day%d" = conf.invoice.period.start.runtimeChecked
  LocalDate.of(year, month, day)
val dueDate = startDate.plusDays(conf.invoice.period.days)

// formatter for date as 05 Aug 2024
val dateFormatter = java.time.format.DateTimeFormatter.ofPattern("dd MMM yyyy")

case class Order(description: String, quantity: Int, unitPrice: BigDecimal)

type AppendixSection = (
    title: String,
    desc: String,
    itemsTitle: String,
    items: Vector[(id: String, desc: String)]
)

type Appendix = (
    title: String,
    description: String,
    sections: Vector[AppendixSection]
)

var itemsPrice: BigDecimal = 0.0
val itemsB = List.newBuilder[Order]
for item <- conf.listings.items do
  val unitPrice = BigDecimal(item.price) / 100
  val total = item.qty * unitPrice
  itemsPrice += total
  itemsB += Order(item.desc, item.qty, unitPrice)

val (subtotal, items) = optTaxRate match
  case Some(rate) =>
    val tax = itemsPrice * rate
    val taxShow = f"${rate * 100}%.0f%%"
    (itemsPrice + tax, itemsB.result() :+ Order(s"VAT ($taxShow)", 1, tax))
  case None =>
    (itemsPrice, itemsB.result())

val appendixTitles = ('A' to 'Z')
  .to(LazyList)
  .lazyZip(conf.appendices)
  .map((letter, appendix) => s"Appendix $letter ($"${appendix.title}$")")

/// PDF generation

import org.apache.pdfbox.pdmodel.{PDDocument, PDPage, PDPageContentStream}
import org.apache.pdfbox.pdmodel.font.{PDType1Font, PDFont, Standard14Fonts}
import org.apache.pdfbox.pdmodel.font.PDType0Font
import Standard14Fonts.FontName
import org.apache.pdfbox.pdmodel.common.PDRectangle

enum Fonts(name: FontName) extends PDType1Font(name):
  case Helvetica extends Fonts(FontName.HELVETICA)
  case HelveticaBold extends Fonts(FontName.HELVETICA_BOLD)
  case HelveticaOblique extends Fonts(FontName.HELVETICA_OBLIQUE)
  case Courier extends Fonts(FontName.COURIER)
  case TimesRoman extends Fonts(FontName.TIMES_ROMAN)

enum FontRef:
  case Helvetica
  case HelveticaBold
  case HelveticaOblique
  case Courier
  case TimesRoman
  case Monospace

enum PageSize:
  case A4

case class TextStep(dx: Float = 0, dy: Float = 0, text: String)

enum PageElement:
  case Text(
      font: FontRef,
      size: Int,
      x: Float,
      y: Float,
      steps: Vector[TextStep]
  )
  case Line(
      width: Float,
      startX: Float,
      startY: Float,
      endX: Float,
      endY: Float
  )

case class LayoutPage(size: PageSize = PageSize.A4, elements: Vector[PageElement])
case class LayoutDocument(pages: Vector[LayoutPage])

def textStep(text: String, dx: Float = 0, dy: Float = 0): TextStep =
  TextStep(dx = dx, dy = dy, text = text)

object FontPaths:
  val InconsolataRegular =
    os.pwd / "fonts" / "inconsolata-4" / "Inconsolata-Regular.ttf"

trait ExtendedFonts(doc: PDDocument):
  lazy val InconsolataRegular = Font(FontPaths.InconsolataRegular)
  private def Font(path: os.Path) = PDType0Font.load(doc, path.toIO)

end ExtendedFonts

trait FontMetrics:
  def stringWidth(font: FontRef, text: String, fontSize: Int): Float

final class PageBuilder(val size: PageSize = PageSize.A4):
  private val elementsB = Vector.newBuilder[PageElement]

  def text(font: FontRef, fontSize: Int, x: Float, y: Float)(steps: TextStep*): Unit =
    elementsB += PageElement.Text(font, fontSize, x, y, steps.toVector)

  def line(
      width: Float,
      startX: Float,
      startY: Float,
      endX: Float,
      endY: Float
  ): Unit =
    elementsB += PageElement.Line(width, startX, startY, endX, endY)

  def result(): LayoutPage =
    LayoutPage(size, elementsB.result())

def splitText(
    text: String,
    width: Int,
    font: FontRef,
    fontSize: Int,
    fontMetrics: FontMetrics
): List[String] =
  val words = text.split(" ")
  val lines = List.newBuilder[String]
  var currentLine = StringBuilder()
  for word <- words do
    if word.contains("\u000A") then
      word.split("\u000A") match
        case Array(lastWord, initNext) =>
          lines += (currentLine ++= lastWord).toString
          currentLine = StringBuilder() ++= initNext ++= " "
        case Array(lastWord) =>
          lines += (currentLine ++= lastWord).toString
          currentLine = StringBuilder()
        case Array() =>
          lines += currentLine.toString
          currentLine = StringBuilder()
    else
      val currentWidth =
        fontMetrics.stringWidth(
          font,
          currentLine.toString + " " + word,
          fontSize
        )
      if currentWidth > width then
        lines += currentLine.toString
        currentLine = StringBuilder()
      end if
      currentLine ++= word ++ " "
  end for
  if currentLine.nonEmpty then lines += currentLine.toString
  lines.result()

def showMoney(value: BigDecimal, verbose: Boolean = false): String =
  val whole = value.setScale(0, RoundingMode.DOWN)
  val fraction = ((value - whole) * 100).setScale(0, RoundingMode.UP)
  val fPart = if fraction < 10 then f"0$fraction%.0f" else f"$fraction%.0f"
  val combined = f"$whole%.0f,$fPart%s"
  if verbose then
    if conf.currency.left then
      s"${conf.currency.symbol} $combined (${conf.currency.code})"
    else s"$combined ${conf.currency.symbol} (${conf.currency.code})"
  else combined

case class TextStyle(font: FontRef, size: Int, lineHeight: Float = 15)

enum HorizontalAnchor:
  case Left(x: Float)
  case Right(rightEdge: Float)

case class FixedText(
    anchor: HorizontalAnchor,
    y: Float,
    style: TextStyle,
    steps: Vector[TextStep]
)

case class FlowText(x: Float, style: TextStyle, steps: Vector[TextStep])

case class TableColumn(
    header: String,
    width: Float,
    wrapWidth: Option[Int] = None
)

case class TableCell(text: String, wrap: Boolean = false)

case class TableRow(cells: Vector[TableCell])

case class TableSpec(
    columns: Vector[TableColumn],
    headerStyle: TextStyle,
    bodyStyle: TextStyle,
    rows: Vector[TableRow],
    lineWidth: Float = 0.5f,
    rowHeight: Float = 15,
    gridOffsetY: Float = 4,
    textInsetX: Float = 5
)

case class BulletListSpec(
    style: TextStyle,
    width: Int,
    items: Vector[String],
    x: Float = 0,
    bullet: String = "-",
    bulletGap: Float = 10,
    itemGap: Float = 5
)

enum FlowBlock:
  case Gap(height: Float)
  case Row(texts: Vector[FlowText])
  case WrappedText(x: Float, width: Int, style: TextStyle, text: String)
  case Rule(startX: Float = 0, endX: Float = 500, width: Float = 0.5f)
  case Table(spec: TableSpec)
  case BulletList(spec: BulletListSpec)

case class FlowRegion(x: Float, topY: Float, blocks: Vector[FlowBlock])

case class PageSpec(
    size: PageSize = PageSize.A4,
    fixed: Vector[FixedText] = Vector.empty,
    flows: Vector[FlowRegion] = Vector.empty
)

case class DocumentSpec(pages: Vector[PageSpec])

object Markup:
  def stepsForLines(lines: Seq[String], lineHeight: Float): Vector[TextStep] =
    lines.headOption match
      case Some(head) =>
        Vector(textStep(head)) ++
          lines.tail.map(line => textStep(line, dy = -lineHeight))
      case None =>
        Vector.empty

  def at(x: Float, y: Float, style: TextStyle)(lines: String*): FixedText =
    FixedText(
      HorizontalAnchor.Left(x),
      y,
      style,
      stepsForLines(lines, style.lineHeight)
    )

  def atRight(
      rightEdge: Float,
      y: Float,
      style: TextStyle,
      text: String
  ): FixedText =
    FixedText(
      HorizontalAnchor.Right(rightEdge),
      y,
      style,
      Vector(textStep(text))
    )

  def flowText(style: TextStyle, x: Float = 0)(steps: TextStep*): FlowText =
    FlowText(x, style, steps.toVector)

  def row(texts: FlowText*): FlowBlock =
    FlowBlock.Row(texts.toVector)

  def line(style: TextStyle, text: String, x: Float = 0): FlowBlock =
    row(flowText(style, x)(textStep(text)))

  def lines(style: TextStyle, entries: Seq[String], x: Float = 0): FlowBlock =
    row(FlowText(x, style, stepsForLines(entries, style.lineHeight)))

  def wrapped(
      style: TextStyle,
      text: String,
      width: Int,
      x: Float = 0
  ): FlowBlock =
    FlowBlock.WrappedText(x, width, style, text)

  def gap(height: Float): FlowBlock =
    FlowBlock.Gap(height)

  def rule(
      startX: Float = 0,
      endX: Float = 500,
      width: Float = 0.5f
  ): FlowBlock =
    FlowBlock.Rule(startX, endX, width)

object LayoutCompiler:
  private def textDepth(steps: Vector[TextStep]): Float =
    var offsetY = 0f
    var minY = 0f
    for step <- steps do
      offsetY += step.dy
      if offsetY < minY then minY = offsetY
    -minY

  private def emitText(
      page: PageBuilder,
      x: Float,
      y: Float,
      style: TextStyle,
      steps: Vector[TextStep]
  ): Unit =
    if steps.nonEmpty then
      page.text(style.font, style.size, x, y)(steps*)

  private def anchoredX(fixed: FixedText, fontMetrics: FontMetrics): Float =
    fixed.anchor match
      case HorizontalAnchor.Left(x) =>
        x
      case HorizontalAnchor.Right(rightEdge) =>
        val firstText = fixed.steps.headOption.map(_.text).getOrElse("")
        rightEdge - fontMetrics.stringWidth(
          fixed.style.font,
          firstText,
          fixed.style.size
        )

  private def compileTable(
      page: PageBuilder,
      baseX: Float,
      topY: Float,
      spec: TableSpec,
      fontMetrics: FontMetrics
  ): Float =
    val colBoundaries = spec.columns.scanLeft(0f)(_ + _.width).toVector
    val colStarts = colBoundaries.init
    val totalWidth = colBoundaries.lastOption.getOrElse(0f)
    val headerY = topY - spec.rowHeight

    for (column, x) <- spec.columns.zip(colStarts) do
      emitText(
        page,
        baseX + x + spec.textInsetX,
        headerY,
        spec.headerStyle,
        Vector(textStep(column.header))
      )

    val rowLines =
      spec.rows.map { row =>
        row.cells.zip(spec.columns).map { (cell, column) =>
          if cell.wrap then
            splitText(
              cell.text,
              column.wrapWidth.getOrElse(column.width.toInt),
              spec.bodyStyle.font,
              spec.bodyStyle.size,
              fontMetrics
            )
          else List(cell.text)
        }
      }

    val rowHeights = rowLines.map(_.map(_.size).maxOption.getOrElse(1))
    val tableBottomY = topY - ((rowHeights.sum + 1) * spec.rowHeight)

    locally:
      var i = 0
      for bucketSize <- 1 +: rowHeights ++: Seq(1) do
        val y = topY - spec.gridOffsetY - i * spec.rowHeight
        page.line(spec.lineWidth, baseX, y, baseX + totalWidth, y)
        i += bucketSize
      end for

    for x <- colBoundaries do
      page.line(
        spec.lineWidth,
        baseX + x,
        topY - spec.gridOffsetY,
        baseX + x,
        tableBottomY - spec.gridOffsetY
      )

    var rowY = topY - (spec.rowHeight * 2)
    for (cells, rowHeightUnits) <- rowLines.zip(rowHeights) do
      for ((cellLines, _), x) <- cells.zip(spec.columns).zip(colStarts) do
        emitText(
          page,
          baseX + x + spec.textInsetX,
          rowY,
          spec.bodyStyle,
          Markup.stepsForLines(cellLines, spec.bodyStyle.lineHeight)
        )
      end for
      rowY -= spec.rowHeight * rowHeightUnits
    end for

    spec.rowHeight * (rowHeights.sum + 1)

  private def compileBulletList(
      page: PageBuilder,
      baseX: Float,
      topY: Float,
      spec: BulletListSpec,
      fontMetrics: FontMetrics
  ): Float =
    var currentY = topY
    for item <- spec.items do
      val lines =
        splitText(item, spec.width, spec.style.font, spec.style.size, fontMetrics)
      val stepsB = Vector.newBuilder[TextStep]
      stepsB += textStep(spec.bullet)
      lines.headOption.foreach { firstLine =>
        stepsB += textStep(firstLine, dx = spec.bulletGap)
      }
      for line <- lines.tail do
        stepsB += textStep(line, dy = -spec.style.lineHeight)
      end for
      emitText(page, baseX + spec.x, currentY, spec.style, stepsB.result())
      currentY -= spec.style.lineHeight * lines.length
      currentY -= spec.itemGap
    end for
    topY - currentY

  private def compileFlowBlock(
      page: PageBuilder,
      baseX: Float,
      currentY: Float,
      block: FlowBlock,
      fontMetrics: FontMetrics
  ): Float =
    block match
      case FlowBlock.Gap(height) =>
        currentY - height
      case FlowBlock.Row(texts) =>
        var maxDepth = 0f
        for text <- texts do
          emitText(page, baseX + text.x, currentY, text.style, text.steps)
          maxDepth = maxDepth.max(textDepth(text.steps))
        end for
        currentY - maxDepth
      case FlowBlock.WrappedText(x, width, style, text) =>
        val lines = splitText(text, width, style.font, style.size, fontMetrics)
        compileFlowBlock(
          page,
          baseX,
          currentY,
          FlowBlock.Row(
            Vector(FlowText(x, style, Markup.stepsForLines(lines, style.lineHeight)))
          ),
          fontMetrics
        )
      case FlowBlock.Rule(startX, endX, width) =>
        page.line(width, baseX + startX, currentY, baseX + endX, currentY)
        currentY
      case FlowBlock.Table(spec) =>
        currentY - compileTable(page, baseX, currentY, spec, fontMetrics)
      case FlowBlock.BulletList(spec) =>
        currentY - compileBulletList(page, baseX, currentY, spec, fontMetrics)

  private def compilePage(
      pageSpec: PageSpec,
      fontMetrics: FontMetrics
  ): LayoutPage =
    val page = new PageBuilder(pageSpec.size)

    for fixed <- pageSpec.fixed do
      emitText(
        page,
        anchoredX(fixed, fontMetrics),
        fixed.y,
        fixed.style,
        fixed.steps
      )
    end for

    for flow <- pageSpec.flows do
      var currentY = flow.topY
      for block <- flow.blocks do
        currentY = compileFlowBlock(page, flow.x, currentY, block, fontMetrics)
      end for
    end for

    page.result()

  def compile(
      documentSpec: DocumentSpec,
      fontMetrics: FontMetrics
  ): LayoutDocument =
    LayoutDocument(documentSpec.pages.map(compilePage(_, fontMetrics)))

object InvoiceMarkup:
  import Markup.*

  private val titleStyle = TextStyle(FontRef.HelveticaBold, 16, lineHeight = 20)
  private val headingStyle = TextStyle(FontRef.HelveticaBold, 10)
  private val bodyStyle = TextStyle(FontRef.Helvetica, 10)
  private val italicStyle = TextStyle(FontRef.HelveticaOblique, 10)
  private val monospaceStyle = TextStyle(FontRef.Monospace, 10)

  private def invoiceTable: TableSpec =
    TableSpec(
      columns = Vector(
        TableColumn("Description", width = 315, wrapWidth = Some(315)),
        TableColumn("Quantity", width = 50),
        TableColumn("Unit Price", width = 60),
        TableColumn("Total", width = 75)
      ),
      headerStyle = headingStyle,
      bodyStyle = bodyStyle,
      rows = items.map {
        case Order(description, quantity, unitPrice) =>
          val total = quantity * unitPrice
          val qty = if conf.listings.useHours then s"$quantity hrs" else s"$quantity"
          TableRow(
            Vector(
              TableCell(description, wrap = true),
              TableCell(qty),
              TableCell(showMoney(unitPrice)),
              TableCell(showMoney(total))
            )
          )
      }.toVector
    )

  private def bankLines: Vector[String] =
    Vector(
      s"Beneficiary: ${conf.bank.holder}"
    ) ++
      conf.bank.userAddress.toVector.map(userAddress =>
        s"Beneficiary Address: ${userAddress}"
      ) ++
      Vector(
        s"IBAN: ${conf.bank.account}",
        s"Recipient SWIFT/BIC: ${conf.bank.swift}"
      ) ++
      conf.bank.intermediary.toVector.map(intermediary =>
        s"Intermediary bank BIC: ${intermediary}"
      ) ++
      conf.bank.routing.toVector.map(routing =>
        s"Routing number: ${routing}"
      ) ++
      Vector(
        s"Message for payee: ${invoiceCode}",
        s"Bank Name and Address: ${conf.bank.name}",
        conf.bank.address
      )

  private def twintBlocks: Vector[FlowBlock] =
    conf.twint.toVector.flatMap { twint =>
      Vector(
        gap(30),
        row(
          flowText(headingStyle)(textStep("Or pay with TWINT:")),
          flowText(monospaceStyle, x = 115)(textStep(twint))
        )
      )
    }

  private def appendixSummaryBlocks: Vector[FlowBlock] =
    if conf.appendices.isEmpty then Vector.empty
    else
      Vector(
        gap(15),
        rule(),
        gap(15),
        line(headingStyle, "Appendices:"),
        gap(15)
      ) ++
        conf.appendices.zipWithIndex.flatMap { (appendix, idx) =>
          Vector(
            gap(15),
            row(
              flowText(italicStyle)(
                textStep("[x] "),
                textStep(appendixTitles(idx), dx = 15),
                textStep(appendix.description, dy = -italicStyle.lineHeight)
              )
            )
          )
        }.toVector

  private def appendixBlocks(appendix: Appendix): Vector[FlowBlock] =
    appendix.sections.flatMap { section =>
      Vector(
        line(headingStyle, section.title),
        gap(15),
        wrapped(italicStyle, section.desc, width = 500),
        gap(15),
        line(bodyStyle, section.itemsTitle),
        gap(15),
        FlowBlock.BulletList(
          BulletListSpec(
            style = bodyStyle,
            width = 450,
            items = section.items.map((id, desc) => s"$id: $desc").toVector
          )
        )
      )
    }.toVector

  private def appendixPage(appendix: Appendix, appendixIdx: Int): PageSpec =
    PageSpec(
      flows = Vector(
        FlowRegion(
          x = 50,
          topY = 750,
          blocks =
            Vector(
              line(titleStyle, appendixTitles(appendixIdx)),
              gap(20),
              line(italicStyle, appendix.description),
              gap(30)
            ) ++ appendixBlocks(appendix)
        )
      )
    )

  def build: DocumentSpec =
    val firstPageBlocks =
      Vector(
        FlowBlock.Table(invoiceTable),
        gap(15),
        line(headingStyle, s"Total Amount Due: ${showMoney(subtotal, verbose = true)}"),
        gap(30),
        row(
          flowText(headingStyle)(textStep("Bill To:")),
          flowText(bodyStyle, x = 40)(textStep(conf.client.name))
        ),
        gap(15),
        lines(
          bodyStyle,
          Vector(conf.client.address) ++ conf.client.contactPerson.toVector
        ),
        gap(30),
        line(headingStyle, "Payable to the following account:"),
        gap(15),
        lines(monospaceStyle, bankLines)
      ) ++ twintBlocks ++ appendixSummaryBlocks

    val firstPage =
      PageSpec(
        fixed = Vector(
          at(50, 750, titleStyle)("INVOICE"),
          at(50, 730, bodyStyle)(
            conf.business.name,
            conf.business.address,
            conf.business.contact
          ),
          atRight(550, 680, headingStyle, s"Invoice No: ${invoiceCode}"),
          atRight(
            550,
            665,
            bodyStyle,
            s"Issue Date: ${dateFormatter.format(startDate)}"
          ),
          atRight(
            550,
            650,
            bodyStyle,
            s"Due Date: ${dateFormatter.format(dueDate)}"
          )
        ),
        flows = Vector(
          FlowRegion(x = 50, topY = 640, blocks = firstPageBlocks)
        )
      )

    DocumentSpec(
      Vector(firstPage) ++
        conf.appendices.zipWithIndex.map(appendixPage).toVector
    )

object PdfRenderer:
  private final class PdfFontCatalog(document: PDDocument, useInconsolata: Boolean)
      extends ExtendedFonts(document),
        FontMetrics:

    private lazy val monospaceFont =
      if useInconsolata then InconsolataRegular else Fonts.Courier

    def pdfFont(font: FontRef): PDFont =
      font match
        case FontRef.Helvetica         => Fonts.Helvetica
        case FontRef.HelveticaBold     => Fonts.HelveticaBold
        case FontRef.HelveticaOblique  => Fonts.HelveticaOblique
        case FontRef.Courier           => Fonts.Courier
        case FontRef.TimesRoman        => Fonts.TimesRoman
        case FontRef.Monospace         => monospaceFont

    def stringWidth(font: FontRef, text: String, fontSize: Int): Float =
      pdfFont(font).getStringWidth(text) / 1000 * fontSize

  private def rectangle(pageSize: PageSize): PDRectangle =
    pageSize match
      case PageSize.A4 => PDRectangle.A4

  private def renderPage(
      document: PDDocument,
      pageLayout: LayoutPage,
      fonts: PdfFontCatalog
  ): Unit =
    val page = new PDPage(rectangle(pageLayout.size))
    document.addPage(page)

    val contentStream = new PDPageContentStream(document, page)
    try
      for element <- pageLayout.elements do
        element match
          case PageElement.Text(font, fontSize, x, y, steps) =>
            contentStream.beginText()
            contentStream.setFont(fonts.pdfFont(font), fontSize)
            contentStream.newLineAtOffset(x, y)
            for step <- steps do
              if step.dx != 0 || step.dy != 0 then
                contentStream.newLineAtOffset(step.dx, step.dy)
              contentStream.showText(step.text)
            end for
            contentStream.endText()
          case PageElement.Line(width, startX, startY, endX, endY) =>
            contentStream.setLineWidth(width)
            contentStream.moveTo(startX, startY)
            contentStream.lineTo(endX, endY)
            contentStream.stroke()
    finally contentStream.close()

  def render(
      outputPath: os.Path,
      useInconsolata: Boolean
  )(buildLayout: FontMetrics => LayoutDocument): Unit =
    val document = new PDDocument()
    try
      val fonts = new PdfFontCatalog(document, useInconsolata)
      val layout = buildLayout(fonts)
      for pageLayout <- layout.pages do
        renderPage(document, pageLayout, fonts)
      end for
      document.save(outputPath.toIO)
    finally document.close()

PdfRenderer.render(os.pwd / "Invoice.pdf", useInconsolata) { fontMetrics =>
  LayoutCompiler.compile(InvoiceMarkup.build, fontMetrics)
}

Logger.info("Invoice created successfully.")
