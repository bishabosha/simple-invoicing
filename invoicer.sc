#! /usr/bin/env scala shebang
//> using scala 3.8.1
//> using toolkit 0.9.1
//> using dep org.apache.pdfbox:pdfbox:3.0.7
//> using dep "io.github.bishabosha::enhanced-string-interpolator:1.0.2"
//> using dep io.github.bishabosha::scala-object-notation:0.1.0
//> using dep ch.epfl.lamp::steps::0.2.0
//> using file Configs.scala
//> using file Layout.scala
//> using file PdfRenderer.scala

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

object InvoiceMarkup:
  import Html.*

  private val titleStyle = TextStyle(FontRef.HelveticaBold, 16, lineHeight = 20)
  private val headingStyle = TextStyle(FontRef.HelveticaBold, 10)
  private val bodyStyle = TextStyle(FontRef.Helvetica, 10)
  private val italicStyle = TextStyle(FontRef.HelveticaOblique, 10)
  private val monospaceStyle = TextStyle(FontRef.Monospace, 10)

  private def invoiceTable: Fragment =
    table(headingStyle, bodyStyle)(
      th("Description", width = 315, wrapWidth = Some(315)),
      th("Quantity", width = 50),
      th("Unit Price", width = 60),
      th("Total", width = 75)
    )(
      items.map {
        case Order(description, quantity, unitPrice) =>
          val total = quantity * unitPrice
          val qty = if conf.listings.useHours then s"$quantity hrs" else s"$quantity"
          tr(
            td(description, wrap = true),
            td(qty),
            td(showMoney(unitPrice)),
            td(showMoney(total))
          )
      }*
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

  private def clientBlock: Fragment =
    div(
      row(
        span(headingStyle)(txt("Bill To:")),
        span(bodyStyle, x = 40)(txt(conf.client.name))
      ),
      gap(15),
      p(bodyStyle)(
        (Vector(conf.client.address) ++ conf.client.contactPerson.toVector)*
      )
    )

  private def paymentBlock: Fragment =
    div(
      p(headingStyle)("Payable to the following account:"),
      gap(15),
      p(monospaceStyle)(bankLines*)
    )

  private def twintBlock: Fragment =
    conf.twint match
      case Some(twint) =>
        div(
          gap(30),
          row(
            span(headingStyle)(txt("Or pay with TWINT:")),
            span(monospaceStyle, x = 115)(txt(twint))
          )
        )
      case None =>
        div()

  private def appendixSummaryItem(appendix: Appendix, idx: Int): Fragment =
    div(
      gap(15),
      row(
        span(italicStyle)(
          txt("[x] "),
          txt(appendixTitles(idx), dx = 15),
          txt(appendix.description, dy = -italicStyle.lineHeight)
        )
      )
    )

  private def appendixSummaryBlock: Fragment =
    if conf.appendices.isEmpty then div()
    else
      val items =
        conf.appendices.zipWithIndex.map(appendixSummaryItem).toVector
      div(
        div(
          gap(15),
          hr(),
          gap(15),
          p(headingStyle)("Appendices:")
        ),
        div(
          gap(15),
          div(items*)
        )
      )

  private def appendixSection(section: AppendixSection): Fragment =
    div(
      p(headingStyle)(section.title),
      gap(15),
      wrapped(italicStyle, width = 500)(section.desc),
      gap(15),
      p(bodyStyle)(section.itemsTitle),
      gap(15),
      ul(bodyStyle, width = 450)(
        section.items.map((id, desc) => li(s"$id: $desc"))*
      )
    )

  private def appendixPage(appendix: Appendix, appendixIdx: Int): PageSpec =
    page()(
      body(topY = 750)(
        div(
          p(titleStyle)(appendixTitles(appendixIdx)),
          gap(20),
          p(italicStyle)(appendix.description),
          gap(30),
          div(appendix.sections.map(appendixSection)*)
        )
      )
    )

  def build: DocumentSpec =
    val firstPage =
      page(
        fixed = Vector(
          fixedLeft(50, 750, titleStyle)("INVOICE"),
          fixedLeft(50, 730, bodyStyle)(
            conf.business.name,
            conf.business.address,
            conf.business.contact
          ),
          fixedRight(550, 680, headingStyle, s"Invoice No: ${invoiceCode}"),
          fixedRight(
            550,
            665,
            bodyStyle,
            s"Issue Date: ${dateFormatter.format(startDate)}"
          ),
          fixedRight(
            550,
            650,
            bodyStyle,
            s"Due Date: ${dateFormatter.format(dueDate)}"
          )
        )
      )(
        body(topY = 640)(
          div(
            invoiceTable,
            gap(15),
            p(headingStyle)(
              s"Total Amount Due: ${showMoney(subtotal, verbose = true)}"
            ),
            gap(30),
            clientBlock,
            gap(30),
            paymentBlock,
            twintBlock,
            appendixSummaryBlock
          )
        )
      )

    document(
      (Vector(firstPage) ++ conf.appendices.zipWithIndex.map(appendixPage).toVector)*
    )

val invoiceDocument = InvoiceMarkup.build
PdfRenderer.render(os.pwd / "Invoice.pdf", useInconsolata, invoiceDocument)

Logger.info("Invoice created successfully.")
