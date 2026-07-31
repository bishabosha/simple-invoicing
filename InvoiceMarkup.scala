import java.text.DecimalFormatSymbols
import java.text.DecimalFormat
import scala.math.BigDecimal.RoundingMode
import java.time.format.DateTimeFormatter
import configs.InvoiceSchema
import java.time.LocalDate

class InvoiceMarkup(conf: InvoiceSchema, issueDate: LocalDate) {
  import FontFamily.*
  import FontStyle.*
  import FontWeight.*
  import Html.*
  import Style.*
  import TextAlign.*
  import WhiteSpace.*
  import configs.SelectField
  import configs.SelectElem

  private val titleStyle =
    Style(fontWeight = Bold, fontSize = 16, lineHeight = 20)
  private val headingStyle = Style(fontWeight = Bold)
  private val bodyStyle = Style(width = Some(500.px))
  private val italicStyle = Style(fontStyle = Italic)
  private val monospaceStyle = Style(fontFamily = Monospace)
  private val summaryRowStyle =
    Style(width = Some(500.px), marginTop = 1.lh)
  private val summaryLabelStyle = headingStyle.copy(marginLeft = 320.px)
  private val summaryValueStyle = bodyStyle.copy(marginLeft = 415.px)

  type AppendixSection = SelectElem[SelectField[Appendix, "sections"]]
  type Appendix = SelectElem[SelectField[InvoiceSchema, "appendices"]]

  def roundMoney(value: BigDecimal): BigDecimal =
    val MoneyScale = 2
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

  val invoiceCode = "INV" + (10_000 * conf.client.id + conf.invoice.id)
  val dueDate = issueDate.plusDays(conf.invoice.period.days)

  case class InvoiceItem(
      description: String,
      quantity: BigDecimal,
      unitPrice: BigDecimal,
      total: BigDecimal
  )

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
      n <- LazyList.iterate(0)(_ + 1).map(_.toString)
      a <- ('A' to 'Z').to(LazyList).map(_.toString)
    yield if n == "0" then a else s"$a$n"

  val appendixTitles = titleStream
    .lazyZip(conf.appendices)
    .map((letter, appendix) => s"Appendix $letter ($"${appendix.title}$")")

  def showMoney(value: BigDecimal, verbose: Boolean = false): String =
    val combined = moneyFormatter(value)
    if verbose then
      if conf.currency.left then s"${conf.currency.symbol} $combined (${conf.currency.code})"
      else s"$combined ${conf.currency.symbol} (${conf.currency.code})"
    else combined

  def businessSection: Fragment =
    p(bodyStyle)(conf.business)

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
        conf.bank.toSeq.map { case (key, value) =>
          s"${key}: ${value.replaceAllLiterally("$INVOICE_NO", invoiceCode)}"
        }*
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

  def build: DocumentSpec = {
    val firstPage = page()(
      p(titleStyle.copy(marginBottom = 1.lh))("INVOICE"),
      businessSection,
      dateSection,
      purchaseSummary,
      clientSummary,
      paymentDetails,
      appendixSummaryBlock
    )

    def appendixPage(appendix: Appendix, appendixIdx: Int): PageSpec = {
      def section(section: AppendixSection): Fragment = div(
        p(headingStyle.copy(marginBottom = 1.lh))(section.title),
        p(
          italicStyle.copy(
            width = Some(500.px),
            whiteSpace = Wrap
          )
        )(
          section.desc
        ),
        p(bodyStyle.copy(marginBottom = 1.lh))(section.itemsTitle),
        ul(bodyStyle, style = Style(width = Some(450.px)))(
          section.items.toList.map({ (id, desc) => li(s"$id: $desc") })*
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
    }

    document(
      (Vector(firstPage) ++ conf.appendices.zipWithIndex
        .map(appendixPage)
        .toVector)*
    )
  }
}
