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

object InvoiceLayout:
  private def rightAlignText(
      page: PageBuilder,
      fontMetrics: FontMetrics,
      font: FontRef,
      text: String,
      y: Float,
      size: Int = 10
  ): Unit =
    val textWidth = fontMetrics.stringWidth(font, text, size)
    page.text(font, size, 550 - textWidth, y)(textStep(text))

  def build(fontMetrics: FontMetrics): LayoutDocument =
    val firstPage = new PageBuilder()

    firstPage.text(FontRef.HelveticaBold, 16, 50, 750)(textStep("INVOICE"))
    firstPage.text(FontRef.Helvetica, 10, 50, 730)(
      textStep(conf.business.name),
      textStep(conf.business.address, dy = -15),
      textStep(conf.business.contact, dy = -15)
    )

    rightAlignText(
      firstPage,
      fontMetrics,
      FontRef.HelveticaBold,
      s"Invoice No: ${invoiceCode}",
      680
    )
    rightAlignText(
      firstPage,
      fontMetrics,
      FontRef.Helvetica,
      s"Issue Date: ${dateFormatter.format(startDate)}",
      665
    )
    rightAlignText(
      firstPage,
      fontMetrics,
      FontRef.Helvetica,
      s"Due Date: ${dateFormatter.format(dueDate)}",
      650
    )

    val lineBuckets = items.map(item =>
      splitText(item.description, 365 - 50, FontRef.Helvetica, 10, fontMetrics)
    )

    val rows = lineBuckets.flatten.size
    val rowHeight = 15
    val tableTopY = 640
    val tableOffset = 4
    val tableBottomY = tableTopY - ((rows + 1) * rowHeight)
    val tableStartX = 50
    val tableEndX = 550

    locally:
      var i = 0
      for bucketSize <- 1 +: lineBuckets.map(_.size) ++: Seq(1) do
        val y = tableTopY - tableOffset - i * rowHeight
        firstPage.line(0.5f, tableStartX, y, tableEndX, y)
        i += bucketSize
      end for

    val colPositions = List(50, 365, 415, 475, 550)
    for x <- colPositions do
      firstPage.line(
        0.5f,
        x,
        tableTopY - tableOffset,
        x,
        tableBottomY - tableOffset
      )

    val (unitKind, perUnit) = ("Quantity", "Unit Price")
    firstPage.text(FontRef.HelveticaBold, 10, 55, tableTopY - 15)(
      textStep("Description"),
      textStep(unitKind, dx = 315),
      textStep(perUnit, dx = 50),
      textStep("Total", dx = 60)
    )

    var yPosition = tableTopY - rowHeight

    for (Order(_, quantity, unitPrice), desc) <- items.zip(lineBuckets) do
      val total = quantity * unitPrice
      val qty = if conf.listings.useHours then s"$quantity hrs" else s"$quantity"
      val stepsB = Vector.newBuilder[TextStep]
      for firstDesc <- desc.headOption do stepsB += textStep(firstDesc)
      stepsB += textStep(qty, dx = 365 - 50)
      stepsB += textStep(showMoney(unitPrice), dx = 50)
      stepsB += textStep(showMoney(total), dx = 60)
      val otherDescRows = desc.tail
      if otherDescRows.nonEmpty then
        stepsB += textStep(otherDescRows.head, dx = -425, dy = -15)
        for remaining <- otherDescRows.tail do
          stepsB += textStep(remaining, dy = -15)
      end if
      firstPage.text(FontRef.Helvetica, 10, 55, yPosition - 15)(
        stepsB.result()*
      )
      yPosition -= rowHeight * desc.size
    end for

    yPosition -= 15
    firstPage.text(FontRef.HelveticaBold, 10, 50, yPosition)(
      textStep(s"Total Amount Due: ${showMoney(subtotal, verbose = true)}")
    )

    yPosition -= 30
    firstPage.text(FontRef.HelveticaBold, 10, 50, yPosition)(
      textStep("Bill To:")
    )
    firstPage.text(FontRef.Helvetica, 10, 90, yPosition)(
      textStep(conf.client.name)
    )

    yPosition -= 15
    val clientStepsB = Vector.newBuilder[TextStep]
    clientStepsB += textStep(conf.client.address)
    conf.client.contactPerson.foreach { contactPerson =>
      clientStepsB += textStep(contactPerson, dy = -15)
    }
    firstPage.text(FontRef.Helvetica, 10, 50, yPosition)(clientStepsB.result()*)
    if conf.client.contactPerson.nonEmpty then yPosition -= 15

    yPosition -= 30
    firstPage.text(FontRef.HelveticaBold, 10, 50, yPosition)(
      textStep("Payable to the following account:")
    )

    yPosition -= 15
    val bankStepsB = Vector.newBuilder[TextStep]
    bankStepsB += textStep(s"Beneficiary: ${conf.bank.holder}")
    conf.bank.userAddress.foreach { userAddress =>
      bankStepsB += textStep(s"Beneficiary Address: ${userAddress}", dy = -15)
    }
    bankStepsB += textStep(s"IBAN: ${conf.bank.account}", dy = -15)
    bankStepsB += textStep(s"Recipient SWIFT/BIC: ${conf.bank.swift}", dy = -15)
    conf.bank.intermediary.foreach { intermediary =>
      bankStepsB += textStep(s"Intermediary bank BIC: ${intermediary}", dy = -15)
    }
    conf.bank.routing.foreach { routing =>
      bankStepsB += textStep(s"Routing number: ${routing}", dy = -15)
    }
    bankStepsB += textStep(s"Message for payee: ${invoiceCode}", dy = -15)
    bankStepsB += textStep(s"Bank Name and Address: ${conf.bank.name}", dy = -15)
    bankStepsB += textStep(conf.bank.address, dy = -15)
    val bankSteps = bankStepsB.result()
    firstPage.text(FontRef.Monospace, 10, 50, yPosition)(bankSteps*)
    yPosition -= 15 * (bankSteps.size - 1)

    conf.twint.foreach { twint =>
      yPosition -= 30
      firstPage.text(FontRef.HelveticaBold, 10, 50, yPosition)(
        textStep("Or pay with TWINT:")
      )
      firstPage.text(FontRef.Monospace, 10, 165, yPosition)(textStep(twint))
    }

    if conf.appendices.nonEmpty then
      yPosition -= 15
      firstPage.line(0.5f, 50, yPosition, 550, yPosition)

      yPosition -= 15
      firstPage.text(FontRef.HelveticaBold, 10, 50, yPosition)(
        textStep("Appendices:")
      )

      yPosition -= 15
      for (appendix, idx) <- conf.appendices.zipWithIndex do
        yPosition -= 15
        firstPage.text(FontRef.HelveticaOblique, 10, 50, yPosition)(
          textStep("[x] "),
          textStep(appendixTitles(idx), dx = 15),
          textStep(appendix.description, dy = -15)
        )
        yPosition -= 15
      end for
    end if

    val appendixPages =
      conf.appendices.zipWithIndex.map { (appendix, appendixIdx) =>
        val appendixPage = new PageBuilder()
        appendixPage.text(FontRef.HelveticaBold, 16, 50, 750)(
          textStep(appendixTitles(appendixIdx))
        )

        var appendixY = 750 - 20
        appendixPage.text(FontRef.HelveticaOblique, 10, 50, appendixY)(
          textStep(appendix.description)
        )
        appendixY -= 30

        for section <- appendix.sections do
          appendixPage.text(FontRef.HelveticaBold, 10, 50, appendixY)(
            textStep(section.title)
          )

          val lines =
            splitText(section.desc, 500, FontRef.HelveticaOblique, 10, fontMetrics)
          for (line, idx) <- lines.zipWithIndex do
            appendixPage.text(
              FontRef.HelveticaOblique,
              10,
              50,
              appendixY - 15 * (idx + 1)
            )(textStep(line))
          end for
          appendixY -= 15 * (lines.length + 1)

          appendixPage.text(FontRef.Helvetica, 10, 50, appendixY)(
            textStep(section.itemsTitle)
          )
          appendixY -= 15

          for (id, desc) <- section.items do
            val lines2 =
              splitText(s"$id: $desc", 450, FontRef.Helvetica, 10, fontMetrics)
            val itemStepsB = Vector.newBuilder[TextStep]
            itemStepsB += textStep("-")
            lines2.headOption.foreach { firstLine =>
              itemStepsB += textStep(firstLine, dx = 10)
            }
            for line <- lines2.tail do
              itemStepsB += textStep(line, dy = -15)
            end for
            appendixPage.text(FontRef.Helvetica, 10, 50, appendixY)(
              itemStepsB.result()*
            )
            appendixY -= 15 * lines2.length
            appendixY -= 5
          end for
        end for

        appendixPage.result()
      }

    LayoutDocument(Vector(firstPage.result()) ++ appendixPages)

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

PdfRenderer.render(os.pwd / "Invoice.pdf", useInconsolata)(InvoiceLayout.build)

Logger.info("Invoice created successfully.")
