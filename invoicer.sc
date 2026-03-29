#! /usr/bin/env scala shebang
//> using scala 3.8.1
//> using toolkit 0.9.1
//> using dep org.apache.pdfbox:pdfbox:3.0.7
//> using dep "io.github.bishabosha::enhanced-string-interpolator:1.0.2"
//> using dep io.github.bishabosha::scala-object-notation:0.2.0
//> using dep ch.epfl.lamp::steps::0.2.0
//> using files Configs.scala Layout.scala PdfRenderer.scala Logger.scala

import NamedTuple.AnyNamedTuple
import configs.InvoiceSchema
import scala.math.BigDecimal.RoundingMode
import java.time.LocalDate
import stringmatching.regex.Interpolators.r

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

Logger.info(s"Begin - config file: ${configPath}")
val conf = configs.readConfig(os.rel / os.RelPath(configPath))
Logger.info("parsed config")

/// computed values

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
        s"Issue Date: ${dateFormatter.format(startDate)}",
        s"Due Date: ${dateFormatter.format(dueDate)}"
      )
    )

  def purchaseSummary: Fragment =
    div(
      table(
        headingStyle,
        bodyStyle,
        style = Style(marginBottom = 1.lh, paddingLeft = 5.px)
      )(
        th("Description", Style(width = Some(315.px), whiteSpace = Wrap)),
        th("Quantity", Style(width = Some(50.px))),
        th("Unit Price", Style(width = Some(60.px))),
        th("Total", Style(width = Some(75.px)))
      )(
        items.map { case Order(description, quantity, unitPrice) =>
          val total = quantity * unitPrice
          val qty =
            if conf.listings.useHours then s"$quantity hrs"
            else s"$quantity"
          tr(
            td(description),
            td(qty),
            td(showMoney(unitPrice)),
            td(showMoney(total))
          )
        }*
      ),
      p(headingStyle.copy(marginBottom = 2.lh))(
        s"Total Amount Due: ${showMoney(subtotal, verbose = true)}"
      )
    )

  def clientSummary: Fragment =
    div(style = Style(marginBottom = 2.lh))(
      row(style = Style(marginBottom = 1.lh))(
        span(headingStyle)("Bill To:"),
        span(bodyStyle.copy(marginLeft = 40.px))(conf.client.name)
      ),
      p(bodyStyle)(
        (Vector(conf.client.address) ++ conf.client.contactPerson.toVector)*
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
          conf.bank.routing.toVector.map(routing =>
            s"Routing number: ${routing}"
          ) ++
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

PdfRenderer.render(os.pwd / "Invoice.pdf", useInconsolata, invoiceDocument)

Logger.info("Invoice created successfully.")
