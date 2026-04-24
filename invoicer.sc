#! /usr/bin/env scala shebang
//> using scala 3.8.1
//> using toolkit 0.9.2
//> using dep org.apache.pdfbox:pdfbox:3.0.7
//> using dep "io.github.bishabosha::enhanced-string-interpolator:1.0.2"
//> using dep io.github.bishabosha::scala-object-notation:0.2.1
//> using dep ch.epfl.lamp::steps::0.2.1
//> using files Configs.scala Layout.scala PdfRenderer.scala Logger.scala
//> using options -Wall -Werror
import java.text.DecimalFormatSymbols
import java.text.DecimalFormat

import NamedTuple.AnyNamedTuple
import configs.InvoiceSchema
import scala.math.BigDecimal.RoundingMode
import java.time.LocalDate
import java.time.format.DateTimeFormatter
import java.time.format.DateTimeParseException

case class CliArgs(
    monospaceFontPath: Option[String] = None,
    outputPath: String = "Invoice.pdf",
    configPath: Option[String] = None
)

def printUsageAndExit(exitCode: Int = 0): Nothing =
  Console.err.println(
    """usage: ./invoicer.sc [--monospace-font <file>] [--output <file>] <config-file>
      |
      |options:
      |  --help              Show this help
      |  --monospace-font    Use a custom monospace font file for bank/TWINT fields
      |  --output <file>     Write the PDF to a custom path
      |""".stripMargin
  )
  sys.exit(exitCode)

def fail(msg: String): Nothing =
  Logger.error(msg)
  sys.exit(1)

def parseArgs(remaining: List[String], current: CliArgs = CliArgs()): CliArgs =
  remaining match
    case Nil =>
      current.configPath match
        case Some(_) => current
        case None    => printUsageAndExit(1)
    case "--help" :: _ =>
      printUsageAndExit()
    case "--monospace-font" :: fontPath :: tail =>
      parseArgs(tail, current.copy(monospaceFontPath = Some(fontPath)))
    case "--monospace-font" :: Nil =>
      fail("missing value for --monospace-font")
    case "--output" :: output :: tail =>
      parseArgs(tail, current.copy(outputPath = output))
    case "--output" :: Nil =>
      fail("missing value for --output")
    case flag :: _ if flag.startsWith("--") =>
      fail(s"unknown option: ${flag}")
    case configPath :: tail =>
      current.configPath match
        case None =>
          parseArgs(tail, current.copy(configPath = Some(configPath)))
        case Some(existing) =>
          fail(
            s"received more than one config file: ${existing}, ${configPath}"
          )

def requireValid(condition: Boolean, message: => String): Unit =
  if !condition then fail(message)

def parsePath(rawPath: String, label: String): os.Path =
  try os.Path(rawPath, os.pwd)
  catch
    case _: IllegalArgumentException =>
      fail(s"invalid ${label} path: ${rawPath}")

def parseInvoiceDate(dateText: String): LocalDate =
  try LocalDate.parse(dateText, DateTimeFormatter.ofPattern("yyyy/M/d"))
  catch
    case _: DateTimeParseException =>
      fail(
        s"invalid invoice.period.start '${dateText}', expected format yyyy/m/d"
      )

val cliArgs = parseArgs(args.toList)
val monospaceFontPath = cliArgs.monospaceFontPath.map(parsePath(_, "monospace font"))
val configPath = parsePath(cliArgs.configPath.get, "config")
val outputPath = parsePath(cliArgs.outputPath, "output")

requireValid(os.isFile(configPath), s"config file not found: ${configPath.toString}")
requireValid(
  !os.isDir(outputPath),
  s"output path points to a directory: ${outputPath.toString}"
)
monospaceFontPath.foreach { fontPath =>
  requireValid(
    os.isFile(fontPath),
    s"monospace font file not found: ${fontPath.toString}"
  )
}

Logger.info(s"Begin - config file: ${configPath.toString}")
Logger.info(s"Output file: ${outputPath.toString}")
monospaceFontPath.foreach(fontPath => Logger.info(s"Monospace font file: ${fontPath.toString}"))
val conf = configs.readConfig(configPath)
Logger.info("parsed config")

requireValid(conf.client.id >= 0, "client.id must be non-negative")
requireValid(conf.invoice.id >= 0, "invoice.id must be non-negative")
requireValid(
  conf.invoice.period.days > 0,
  "invoice.period.days must be positive"
)
requireValid(
  conf.listings.taxRate >= 0 && conf.listings.taxRate <= 100,
  "listings.taxRate must be between 0 and 100"
)
requireValid(
  conf.listings.items.nonEmpty,
  "listings.items must not be empty"
)
for (item, idx) <- conf.listings.items.iterator.zipWithIndex do
  requireValid(item.qty > 0, s"listings.items(${idx}).qty must be positive")
  requireValid(item.price >= 0, s"listings.items(${idx}).price must be non-negative")

/// computed values

val invoiceCode = "INV" + (10_000 * conf.client.id + conf.invoice.id)
val issueDate = parseInvoiceDate(conf.invoice.period.start)
val dueDate = issueDate.plusDays(conf.invoice.period.days)

val MoneyScale = 2

def roundMoney(value: BigDecimal): BigDecimal =
  value.setScale(MoneyScale, RoundingMode.HALF_UP)

val optTaxRate: Option[BigDecimal] =
  Option.when(conf.listings.taxRate > 0)(
    BigDecimal(conf.listings.taxRate.toLong) / 100
  )

val dateFormatter = DateTimeFormatter.ofPattern("dd MMM yyyy")

val quantityFormatter = {
  val symbols = new DecimalFormatSymbols();
  symbols.setDecimalSeparator(',');
  val fmt = new DecimalFormat("0.##", symbols)
  (q: BigDecimal) => fmt.format(q)
}

val moneyFormatter = {
  val symbols = new DecimalFormatSymbols();
  symbols.setDecimalSeparator(',')
  val fmt = new DecimalFormat("0.00", symbols)
  fmt.setRoundingMode(java.math.RoundingMode.HALF_UP)
  (amount: BigDecimal) => fmt.format(roundMoney(amount).bigDecimal)
}

case class InvoiceItem(
    description: String,
    quantity: BigDecimal,
    unitPrice: BigDecimal,
    total: BigDecimal
)

type Search[Key <: String, Keys <: Tuple, Values <: Tuple] =
  (Keys, Values) match
    case (Key *: _, v *: _) => v
    case (_ *: ks, _ *: vs) => Search[Key, ks, vs]

type SelectField[NT <: AnyNamedTuple, F <: String] = NT match
  case NamedTuple.NamedTuple[ns, ts] =>
    Search[F, ns, ts]

type SelectElem[F <: scala.collection.Seq[?]] = F match
  case scala.collection.Seq[e] => e

type AppendixSection = SelectElem[SelectField[Appendix, "sections"]]
type Appendix = SelectElem[SelectField[InvoiceSchema, "appendices"]]

val items = conf.listings.items.map { item =>
  val qtyDec = BigDecimal.decimal(item.qty)
  val unitPrice = BigDecimal(item.price) / 100
  val lineTotal = roundMoney(qtyDec * unitPrice)
  InvoiceItem(item.desc, qtyDec, unitPrice, lineTotal)
}

val itemsSubtotal =
  items.iterator.map(_.total).foldLeft(BigDecimal(0))(_ + _)

val taxAmount = optTaxRate.map(rate => roundMoney(itemsSubtotal * rate))
val grandTotal = itemsSubtotal + taxAmount.getOrElse(BigDecimal(0))
val quantityColumnTitle =
  if conf.listings.useHours then "Hours" else "Quantity"
val taxLabel = s"VAT (${conf.listings.taxRate}%)"

val titleStream =
  for
    n <- LazyList.iterate(0)(_ + 1)
    a <- ('A' to 'Z').to(LazyList)
  yield if n == 0 then a else s"$a$n"

val appendixTitles = titleStream
  .lazyZip(conf.appendices)
  .map((letter, appendix) => s"Appendix $letter ($"${appendix.title}$")")

def showMoney(value: BigDecimal, verbose: Boolean = false): String =
  val combined = moneyFormatter(value)
  if verbose then
    if conf.currency.left then s"${conf.currency.symbol} $combined (${conf.currency.code})"
    else s"$combined ${conf.currency.symbol} (${conf.currency.code})"
  else combined

// lay out the data

object InvoiceMarkup:
  import FontFamily.*
  import FontStyle.*
  import FontWeight.*
  import Html.*
  import Style.*
  import TextAlign.*
  import WhiteSpace.*

  private val titleStyle =
    Style(fontWeight = Bold, fontSize = 16, lineHeight = 20)
  private val headingStyle = Style(fontWeight = Bold)
  private val bodyStyle = Style()
  private val italicStyle = Style(fontStyle = Italic)
  private val monospaceStyle = Style(fontFamily = Monospace)
  private val summaryRowStyle =
    Style(width = Some(500.px), marginTop = 1.lh)
  private val summaryLabelStyle = headingStyle.copy(marginLeft = 320.px)
  private val summaryValueStyle = bodyStyle.copy(marginLeft = 415.px)

  def businessSection: Fragment =
    p(bodyStyle)(
      conf.business.name,
      conf.business.address,
      conf.business.contact
    )

  def dateSection: Fragment =
    div(
      style = Style(
        width = Some(500.px),
        textAlign = Right,
        marginTop = 1.lh,
        gap = 1.lh,
        marginBottom = 5.px
      )
    )(
      p(headingStyle)(s"Invoice No: ${invoiceCode}"),
      p(bodyStyle)(
        s"Issue Date: ${dateFormatter.format(issueDate)}",
        s"Due Date: ${dateFormatter.format(dueDate)}",
        s"Payment Terms: net ${conf.invoice.period.days} days"
      )
    )

  def purchaseSummary: Fragment =
    div(
      table(
        headingStyle,
        bodyStyle,
        style = Style(marginBottom = 0.2.lh)
      )(
        th("Description", Style(width = Some(315.px), whiteSpace = Wrap)),
        th(quantityColumnTitle, Style(width = Some(50.px))),
        th("Unit Price", Style(width = Some(60.px))),
        th("Total", Style(width = Some(75.px)))
      )(
        items.map { case InvoiceItem(description, quantity, unitPrice, total) =>
          val qtyFormat = quantityFormatter(quantity)
          val qty =
            if conf.listings.useHours then s"$qtyFormat hrs"
            else s"$qtyFormat"
          tr(
            td(description),
            td(qty),
            td(showMoney(unitPrice)),
            td(showMoney(total))
          )
        }*
      ),
      div(style = Style(width = Some(500.px), marginBottom = 2.lh))(
        taxAmount match
          case Some(tax) =>
            div(
              row(summaryRowStyle)(
                span(summaryLabelStyle)("Subtotal:"),
                span(summaryValueStyle)(showMoney(itemsSubtotal, verbose = true))
              ),
              row(summaryRowStyle)(
                span(summaryLabelStyle)(s"${taxLabel}:"),
                span(summaryValueStyle)(showMoney(tax, verbose = true))
              )
            )
          case None =>
            div(),
        row(summaryRowStyle)(
          span(summaryLabelStyle.copy(fontWeight = Bold))("Total Amount Due:"),
          span(summaryValueStyle.copy(fontWeight = Bold))(
            showMoney(grandTotal, verbose = true)
          )
        )
      )
    )

  def clientSummary: Fragment =
    div(style = Style(marginBottom = 2.lh))(
      row(style = Style(marginBottom = 1.lh))(
        span(headingStyle)("Bill To:"),
        span(bodyStyle.copy(marginLeft = 40.px))(conf.client.name)
      ),
      p(bodyStyle)(
        (Vector(conf.client.address) ++ conf.client.contactPerson.toVector.map(person =>
          s"Attn: ${person}"
        ))*
      )
    )

  def paymentDetails: Fragment =
    div(
      p(headingStyle.copy(marginBottom = 1.lh))(
        "Payable to the following account:"
      ),
      p(monospaceStyle)(
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
          conf.bank.routing.toVector.map(routing => s"Routing number: ${routing}") ++
          Vector(
            s"Message for payee: ${invoiceCode}",
            s"Bank Name and Address: ${conf.bank.name}",
            conf.bank.address
          )*
      ),
      locally(
        conf.twint.match
          case Some(twint) =>
            row(style = Style(marginTop = 2.lh))(
              span(headingStyle)("Or pay with TWINT:"),
              span(monospaceStyle.copy(marginLeft = 95.px))(twint)
            )
          case None =>
            div()
      )
    )

  def appendixSummaryBlock: Fragment =
    def summaryItem(appendix: Appendix, idx: Int): Fragment =
      row(style = Style(marginTop = 1.lh))(
        span(italicStyle)("[x]"),
        span(italicStyle.copy(marginLeft = 15.px))(
          appendixTitles(idx),
          appendix.description
        )
      )
    if conf.appendices.isEmpty then div()
    else
      div(
        hr(style = Style(marginTop = 1.lh, marginBottom = 1.lh)),
        p(headingStyle)("Appendices:"),
        div(style = Style(marginTop = 1.lh))(
          conf.appendices.zipWithIndex.map(summaryItem).toVector*
        )
      )

  def build: DocumentSpec =
    val firstPage =
      page()(
        p(titleStyle.copy(marginBottom = 1.lh))("INVOICE"),
        businessSection,
        dateSection,
        purchaseSummary,
        clientSummary,
        paymentDetails,
        appendixSummaryBlock
      )

    def appendixPage(appendix: Appendix, appendixIdx: Int): PageSpec =
      def section(section: AppendixSection): Fragment =
        div(
          p(headingStyle.copy(marginBottom = 1.lh))(section.title),
          p(
            italicStyle.copy(
              width = Some(500.px),
              marginBottom = 1.lh,
              whiteSpace = Wrap
            )
          )(
            section.desc
          ),
          p(bodyStyle.copy(marginBottom = 1.lh))(section.itemsTitle),
          ul(bodyStyle, style = Style(width = Some(450.px)))(
            section.items.map((id, desc) => li(s"$id: $desc"))*
          )
        )
      page()(
        p(titleStyle.copy(marginBottom = 1.lh))(
          appendixTitles(appendixIdx)
        ),
        p(italicStyle.copy(marginBottom = 2.lh))(
          appendix.description
        ),
        div(
          appendix.sections.map(section)*
        )
      )

    document(
      (Vector(firstPage) ++ conf.appendices.zipWithIndex
        .map(appendixPage)
        .toVector)*
    )

val invoiceDocument = InvoiceMarkup.build
Logger.info("Built markup.")

PdfRenderer.render(outputPath, monospaceFontPath, invoiceDocument)

Logger.info("Invoice created successfully.")
