#! /usr/bin/env scala shebang
//> using scala 3.8.1
//> using toolkit 0.9.1
//> using dep org.apache.pdfbox:pdfbox:3.0.7
//> using dep "io.github.bishabosha::enhanced-string-interpolator:1.0.2"
//> using dep io.github.bishabosha:scala-object-notation_3:0.0.0-42-ad47ff
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
      case _ => printUsageAndExit()
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

object FontPaths:
  val InconsolataRegular = os.pwd / "fonts" / "inconsolata-4" / "Inconsolata-Regular.ttf"

trait ExtendedFonts(doc: PDDocument):
  val InconsolataRegular = Font(FontPaths.InconsolataRegular)
  private def Font(path: os.Path) = PDType0Font.load(doc, path.toIO)

end ExtendedFonts

// Create a new PDF document
val document = new PDDocument()
val page = new PDPage(PDRectangle.A4)
document.addPage(page)

val contentStream = new PDPageContentStream(document, page)

object ExtendedFonts extends ExtendedFonts(document)

val monospaceFont = if useInconsolata then ExtendedFonts.InconsolataRegular else Fonts.Courier

def rightAlignText(font: PDFont, text: String, y: Int, size: Int = 10): Unit =
  contentStream.beginText()
  contentStream.setFont(font, size)
  val textWidth = font.getStringWidth(text) / 1000 * size
  contentStream.newLineAtOffset(550 - textWidth, y)
  contentStream.showText(text)
  contentStream.endText()

// Start writing to the PDF
contentStream.beginText()
contentStream.setFont(Fonts.HelveticaBold, 16)
contentStream.newLineAtOffset(50, 750)
contentStream.showText("INVOICE")
contentStream.endText()

contentStream.beginText()
contentStream.setFont(Fonts.Helvetica, 10)
contentStream.newLineAtOffset(50, 730)
contentStream.showText(conf.business.name)
contentStream.newLineAtOffset(0, -15)
contentStream.showText(conf.business.address)
contentStream.newLineAtOffset(0, -15)
contentStream.showText(conf.business.contact)
contentStream.endText()

rightAlignText(Fonts.HelveticaBold, s"Invoice No: ${invoiceCode}", 680)
rightAlignText(
  Fonts.Helvetica,
  s"Issue Date: ${dateFormatter.format(startDate)}",
  665
)
rightAlignText(
  Fonts.Helvetica,
  s"Due Date: ${dateFormatter.format(dueDate)}",
  650
)

def splitText(
    text: String,
    width: Int,
    font: PDFont,
    fontSize: Int
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
        font.getStringWidth(
          currentLine.toString + " " + word
        ) / 1000 * fontSize
      if currentWidth > width then
        lines += currentLine.toString
        currentLine = StringBuilder()
      end if
      currentLine ++= word ++ " "
  end for
  if currentLine.nonEmpty then lines += currentLine.toString
  lines.result()

// Draw table grid
contentStream.setLineWidth(0.5f)

val lineBuckets =
  items.map(item => splitText(item.description, 365 - 50, Fonts.Helvetica, 10))

val rows = lineBuckets.flatten.size
val rowHeight = 15
val tableTopY = 640
val tableOffset = 4
val tableBottomY = tableTopY - ((rows + 1) * rowHeight)
val tableStartX = 50
val tableEndX = 550

// Draw horizontal lines
locally:
  var i = 0
  for bucketSize <- 1 +: lineBuckets.map(_.size) ++: Seq(1) do
    val y = tableTopY - tableOffset - i * rowHeight
    contentStream.moveTo(tableStartX, y)
    contentStream.lineTo(tableEndX, y)
    contentStream.stroke()
    i += bucketSize
  end for
// Draw vertical lines
val colPositions = List(50, 365, 415, 475, 550)
colPositions.foreach { x =>
  contentStream.moveTo(x, tableTopY - tableOffset)
  contentStream.lineTo(x, tableBottomY - tableOffset)
  contentStream.stroke()
}

// Table headers
contentStream.beginText()
contentStream.setFont(Fonts.HelveticaBold, 10)
contentStream.newLineAtOffset(55, tableTopY - 15)
contentStream.showText("Description")
val (unitKind, perUnit) = ("Quantity", "Unit Price")
contentStream.newLineAtOffset(315, 0)
contentStream.showText(unitKind)
contentStream.newLineAtOffset(50, 0)
contentStream.showText(perUnit)
contentStream.newLineAtOffset(60, 0)
contentStream.showText("Total")
contentStream.endText()

var yPosition = tableTopY - rowHeight

extension (cs: PDPageContentStream)
  def pushLine(jumps: Int = 1, initial: Int = 0) =
    val offset = jumps * 15
    contentStream.newLineAtOffset(initial, -offset)
    yPosition -= offset

def rawPushLine(jumps: Int = 1) =
  val offset = jumps * 15
  yPosition -= offset

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

// Table content
for (Order(_, quantity, unitPrice), desc) <- items.zip(lineBuckets) do
  val total = quantity * unitPrice

  contentStream.beginText()
  contentStream.setFont(Fonts.Helvetica, 10)
  contentStream.newLineAtOffset(55, yPosition - 15)
  for firstDesc <- desc.headOption do contentStream.showText(firstDesc)
  contentStream.newLineAtOffset(365 - 50, 0)
  val qty = if conf.listings.useHours then s"$quantity hrs" else s"$quantity"
  contentStream.showText(qty)
  contentStream.newLineAtOffset(50, 0)
  contentStream.showText(showMoney(unitPrice))
  contentStream.newLineAtOffset(60, 0)
  contentStream.showText(showMoney(total))
  val otherDescRows = desc.tail
  if otherDescRows.nonEmpty then
    contentStream.newLineAtOffset(-425, 0)
    for remaining <- desc.tail do
      yPosition -= rowHeight
      contentStream.newLineAtOffset(0, -15)
      contentStream.showText(remaining)
    end for
  end if
  contentStream.endText()

  yPosition -= rowHeight
end for
// Subtotal, Tax, and Total
yPosition -= 15 // Extra space between the table and the following content
contentStream.beginText()
contentStream.setFont(Fonts.HelveticaBold, 10)
contentStream.newLineAtOffset(50, yPosition)
contentStream.showText(
  s"Total Amount Due: ${showMoney(subtotal, verbose = true)}"
)
contentStream.endText()

// Bill To
yPosition -= 30 // More space between total and payment instructions
contentStream.beginText()
contentStream.setFont(Fonts.HelveticaBold, 10)
contentStream.newLineAtOffset(50, yPosition)
contentStream.showText(s"Bill To:")
contentStream.endText()

contentStream.beginText()
contentStream.setFont(Fonts.Helvetica, 10)
contentStream.newLineAtOffset(90, yPosition)
contentStream.showText(conf.client.name)
contentStream.endText()
rawPushLine()
contentStream.beginText()
contentStream.setFont(Fonts.Helvetica, 10)
contentStream.newLineAtOffset(50, yPosition)
contentStream.showText(conf.client.address)
conf.client.contactPerson.match
  case Some(contactPerson) =>
    contentStream.pushLine()
    contentStream.showText(contactPerson)
  case _ =>
end match
contentStream.endText()

// Payment Instructions
rawPushLine(jumps = 2) // More space between total and payment instructions
contentStream.beginText()
contentStream.setFont(Fonts.HelveticaBold, 10)
contentStream.newLineAtOffset(50, yPosition)
contentStream.showText("Payable to the following account:")
contentStream.endText()

rawPushLine() // space between payment instructions and bank details

contentStream.beginText()
contentStream.setFont(monospaceFont, 10)
contentStream.newLineAtOffset(50, yPosition)
contentStream.showText(s"Beneficiary: ${conf.bank.holder}")
conf.bank.userAddress match
  case Some(userAddress) =>
    contentStream.pushLine()
    contentStream.showText(s"Beneficiary Address: ${userAddress}")
  case _ =>
end match
contentStream.pushLine()
contentStream.showText(s"IBAN: ${conf.bank.account}")
contentStream.pushLine()
contentStream.showText(s"Recipient SWIFT/BIC: ${conf.bank.swift}")
conf.bank.intermediary.match
  case Some(intermediary) =>
    contentStream.pushLine()
    contentStream.showText(
      s"Intermediary bank BIC: ${intermediary}"
    )
  case None =>
end match
conf.bank.routing.match
  case Some(routing) =>
    contentStream.pushLine()
    contentStream.showText(s"Routing number: ${routing}")
  case _ =>
contentStream.pushLine()
contentStream.showText(s"Message for payee: ${invoiceCode}")
contentStream.pushLine()
contentStream.showText(s"Bank Name and Address: ${conf.bank.name}")
contentStream.pushLine()
contentStream.showText(conf.bank.address)
contentStream.endText()

conf.twint.match
  case Some(twint) =>
    rawPushLine(2)
    contentStream.beginText()
    contentStream.setFont(Fonts.HelveticaBold, 10)
    contentStream.newLineAtOffset(50, yPosition)
    contentStream.showText("Or pay with TWINT:")
    contentStream.endText()

    // rawPushLine() // space between payment instructions and bank details

    contentStream.beginText()
    contentStream.setFont(monospaceFont, 10)
    contentStream.newLineAtOffset(165, yPosition)
    contentStream.showText(twint)
    contentStream.endText()
  case _ =>
end match

// Add description of appendices, add a single line separator if any exist

val appendixTitles = ('A' to 'Z')
  .to(LazyList)
  .lazyZip(conf.appendices)
  .map((letter, appendix) => s"Appendix $letter ($"${appendix.title}$")")

if conf.appendices.nonEmpty then
  rawPushLine()
  contentStream.setLineWidth(0.5f)
  contentStream.moveTo(50, yPosition)
  contentStream.lineTo(550, yPosition)
  contentStream.stroke()

  rawPushLine() // space between line and appendices
  contentStream.beginText()
  contentStream.setFont(Fonts.HelveticaBold, 10)
  contentStream.newLineAtOffset(50, yPosition)
  contentStream.showText("Appendices:")
  contentStream.endText()

  rawPushLine() // space between appendices title and the list

  for (appendix, idx) <- conf.appendices.zipWithIndex do
    rawPushLine()
    contentStream.beginText()
    contentStream.setFont(Fonts.HelveticaOblique, 10)
    contentStream.newLineAtOffset(50, yPosition)
    contentStream.showText("[x] ")
    contentStream.newLineAtOffset(15, 0)
    contentStream.showText(appendixTitles(idx))
    contentStream.pushLine()
    contentStream.showText(appendix.description)
    contentStream.endText()
end if

// Closing the content stream
contentStream.close()

// For each appendix, make a new page with the appendix content
for (appendix, appendixIdx) <- conf.appendices.zipWithIndex do
  val appendixPage = new PDPage(PDRectangle.A4)
  document.addPage(appendixPage)
  val contentStream2 = new PDPageContentStream(document, appendixPage)
  // add title
  contentStream2.beginText()
  contentStream2.setFont(Fonts.HelveticaBold, 16)
  contentStream2.newLineAtOffset(50, 750)
  contentStream2.showText(appendixTitles(appendixIdx))
  contentStream2.endText()
  // list all of the sections
  var yPosition = 750 - 20

  contentStream2.beginText()
  contentStream2.setFont(Fonts.HelveticaOblique, 10)
  contentStream2.newLineAtOffset(50, yPosition)
  contentStream2.showText(appendix.description)
  contentStream2.endText()
  yPosition -= 30

  for (section, idx) <- appendix.sections.zipWithIndex do
    contentStream2.beginText()
    contentStream2.setFont(Fonts.HelveticaBold, 10)
    contentStream2.newLineAtOffset(50, yPosition)
    contentStream2.showText(section.title)
    contentStream2.endText()
    // add the content of the section, splitting lines by measuring the width of the text

    val lines = splitText(section.desc, 500, Fonts.HelveticaOblique, 10)
    for (line, idx) <- lines.zipWithIndex do
      contentStream2.beginText()
      contentStream2.setFont(Fonts.HelveticaOblique, 10)
      contentStream2.newLineAtOffset(50, yPosition - 15 * (idx + 1))
      contentStream2.showText(line)
      contentStream2.endText()
    end for
    yPosition -= 15 * (lines.length + 1)

    contentStream2.beginText()
    contentStream2.setFont(Fonts.Helvetica, 10)
    contentStream2.newLineAtOffset(50, yPosition)
    contentStream2.showText(section.itemsTitle)
    contentStream2.endText()
    yPosition -= 15

    // draw a table from the items in the section
    for (id, desc) <- section.items do
      contentStream2.beginText()
      contentStream2.setFont(Fonts.Helvetica, 10)
      contentStream2.newLineAtOffset(50, yPosition)
      contentStream2.showText("-")
      contentStream2.newLineAtOffset(10, 0)
      val lines2 = splitText(s"$id: $desc", 450, Fonts.Helvetica, 10)
      for (line, idx) <- lines2.zipWithIndex do
        contentStream2.showText(line)
        contentStream2.newLineAtOffset(0, -15)
      end for
      contentStream2.endText()
      yPosition -= 15 * (lines2.length)
      yPosition -= 5 // gap in list?
  end for
  contentStream2.close()
end for
// Saving the document
document.save("Invoice.pdf")
document.close()

Logger.info("Invoice created successfully.")
