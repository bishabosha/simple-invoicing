import java.time.LocalDate
import java.time.format.DateTimeFormatter
import java.util.Locale
import configs.InvoiceSchema

/** Optional image assets for the studio layout. Missing textures fall back to
  * flat fills; a missing logo is simply omitted.
  */
case class StudioAssets(
    logo: Option[os.Path] = None,
    paperTexture: Option[os.Path] = None,
    accentTexture: Option[os.Path] = None
)

/** Fixed wording used by the studio layout; everything else comes from the
  * invoice config.
  */
case class StudioLabels(
    title: String,
    invoiceDate: String,
    invoiceNo: String => String,
    dateFormat: DateTimeFormatter,
    descriptionHeader: String,
    quantityHeader: String,
    hoursHeader: String,
    unitPriceHeader: String,
    totalHeader: String,
    subtotal: String,
    taxLabel: Int => String,
    totalAmount: String,
    billedTo: String,
    payableTo: String,
    attn: String => String,
    paymentNote: Int => String,
    thankYou: String,
    contact: String
)

object StudioLabels:
  val english: StudioLabels = StudioLabels(
    title = "Invoice",
    invoiceDate = "Invoice date:",
    invoiceNo = code => s"Invoice no: $code",
    dateFormat = DateTimeFormatter.ofPattern("d MMMM yyyy", Locale.ENGLISH),
    descriptionHeader = "DESCRIPTION",
    quantityHeader = "QTY",
    hoursHeader = "HOURS",
    unitPriceHeader = "UNIT PRICE",
    totalHeader = "TOTAL",
    subtotal = "Subtotal",
    taxLabel = rate => s"VAT ($rate%)",
    totalAmount = "TOTAL AMOUNT",
    billedTo = "Billed to:",
    payableTo = "Payable to:",
    attn = person => s"Attn: $person",
    paymentNote = days => s"Please make payment within $days days",
    thankYou = "Thank you for your business",
    contact = "Contact"
  )

/** Alternative single-page invoice layout with a letterpress feel: centred
  * serif masthead over a short rule, billed-to / payable-to columns up top,
  * a tinted listings header band, a tinted totals card, and a full-bleed
  * footer band with centred contact details and an optional logo bleeding
  * off the lower-left page edge.
  */
class StudioInvoiceMarkup(
    conf: InvoiceSchema,
    issueDate: LocalDate,
    assets: StudioAssets,
    labels: StudioLabels = StudioLabels.english
) {
  import FontFamily.*
  import FontStyle.*
  import FontWeight.*
  import Html.*
  import Style.*
  import TextAlign.*
  import WhiteSpace.*

  private val base = InvoiceMarkup(conf, issueDate)

  private val PaperColor = Rgb(249, 248, 244)
  private val AccentColor = Rgb(233, 237, 229)
  private val InkGrey = Rgb(96, 100, 94)

  private val serifTitleStyle =
    Style(fontFamily = Times, fontSize = 32, lineHeight = 36, textAlign = Center)
  private val serifHeadingStyle =
    Style(fontFamily = Times, fontWeight = Bold, fontSize = 12, lineHeight = 17)
  private val columnHeaderStyle =
    Style(fontWeight = Bold, fontSize = 9, lineHeight = 14)
  private val bodyStyle = Style(fontSize = 11, lineHeight = 16)
  private val smallStyle = Style(fontSize = 10, lineHeight = 14)
  private val noteStyle = Style(fontSize = 8, lineHeight = 10)
  private val itemNoteStyle =
    Style(fontStyle = Italic, fontSize = 10, lineHeight = 14, color = InkGrey)

  /** Pure vertical space (the flow cursor tracks baselines, so an empty
    * stack's top margin is the only thing it contributes); used to give
    * tinted blocks bottom padding, since a stack's background box ends at
    * its last child.
    */
  private def spacer(height: Float): Fragment =
    div(Style(marginTop = height.px))()

  private def money(value: BigDecimal): String =
    val amount = base.showMoney(value)
    if conf.currency.left then s"${conf.currency.symbol} $amount"
    else s"$amount ${conf.currency.symbol}"

  private def mastheadSection: Fragment =
    div(
      // short framing rule lifted above the flow origin into the top margin
      hr(
        Style(width = Some(80.px), marginLeft = 210.px, borderWidth = 1f, marginTop = (-34).px)
      ),
      p(serifTitleStyle.copy(marginTop = 34.px))(labels.title),
      hr(
        Style(width = Some(160.px), marginLeft = 170.px, borderWidth = 1f, marginTop = 10.px)
      ),
      p(bodyStyle.copy(textAlign = Center, marginTop = 22.px))(
        labels.invoiceNo(base.invoiceCode)
      ),
      p(smallStyle.copy(textAlign = Center, marginTop = 15.px))(
        s"${labels.invoiceDate} ${labels.dateFormat.format(issueDate)}"
      )
    )

  private def partiesSection: Fragment =
    val clientLines =
      Vector(conf.client.name, conf.client.address) ++
        conf.client.contactPerson.toVector.map(labels.attn)
    val bankLines = conf.bank.toSeq.map { case (key, value) =>
      s"$key: ${value.replaceAllLiterally("$INVOICE_NO", base.invoiceCode)}"
    }
    div(
      row(Style(marginTop = 40.px))(
        span(serifHeadingStyle)(labels.billedTo),
        span(serifHeadingStyle.copy(marginLeft = 290.px))(labels.payableTo)
      ),
      row(Style(marginTop = 16.px))(
        span(bodyStyle.copy(width = Some(250.px), whiteSpace = Wrap))(clientLines*),
        span(bodyStyle.copy(marginLeft = 290.px, width = Some(210.px), whiteSpace = Wrap))(
          bankLines*
        )
      ),
      p(noteStyle.copy(marginTop = 14.px))(
        labels.paymentNote(conf.invoice.period.days)
      )
    )

  private def itemsSection: Fragment =
    def columnsRow(rowStyle: Style, cellStyle: Style)(
        desc: String,
        qty: String,
        unitPrice: String,
        total: String
    ): Fragment =
      row(rowStyle)(
        span(cellStyle.copy(marginLeft = 10.px, width = Some(280.px), whiteSpace = Wrap))(
          desc
        ),
        span(cellStyle.copy(marginLeft = 300.px))(qty),
        span(cellStyle.copy(marginLeft = 360.px))(unitPrice),
        span(cellStyle.copy(marginLeft = 445.px))(total)
      )
    div(
      div(Style(marginTop = 30.px, backgroundColor = Some(AccentColor)))(
        columnsRow(Style(marginTop = 13.px), columnHeaderStyle)(
          labels.descriptionHeader,
          if conf.listings.useHours then labels.hoursHeader else labels.quantityHeader,
          labels.unitPriceHeader,
          labels.totalHeader
        ),
        spacer(6)
      ),
      div(
        base.items.map { item =>
          div(
            columnsRow(Style(marginTop = 18.px), bodyStyle)(
              item.description,
              base.quantityFormatter(item.quantity),
              money(item.unitPrice),
              money(item.total)
            ),
            item.body match
              case Some(text) =>
                p(
                  itemNoteStyle.copy(
                    marginTop = 13.px,
                    marginLeft = 10.px,
                    width = Some(268.px),
                    whiteSpace = Wrap
                  )
                )(text)
              case None => div(),
            hr(Style(marginTop = 12.px))
          )
        }*
      )
    )

  private def totalsSection: Fragment =
    def cardRow(labelStyle: Style, valueStyle: Style)(
        label: String,
        value: String
    ): Fragment =
      row(Style(marginTop = 14.px))(
        span(labelStyle.copy(marginLeft = 14.px))(label),
        span(valueStyle.copy(marginLeft = 116.px))(value)
      )
    div(
      Style(
        marginLeft = 290.px,
        width = Some(210.px),
        marginTop = 24.px,
        backgroundColor = Some(AccentColor)
      )
    )(
      base.taxAmount match
        case Some(tax) =>
          div(
            cardRow(smallStyle, smallStyle)(labels.subtotal, money(base.itemsSubtotal)),
            cardRow(smallStyle, smallStyle)(
              labels.taxLabel(conf.listings.taxRate),
              money(tax)
            )
          )
        case None => div(),
      cardRow(
        serifHeadingStyle.copy(fontSize = 11),
        serifHeadingStyle.copy(fontSize = 12)
      )(labels.totalAmount, money(base.grandTotal)),
      spacer(9)
    )

  private def copyrightSection: Fragment =
    conf.copyright match
      case Some(text) =>
        p(itemNoteStyle.copy(textAlign = Center, marginTop = 30.px))(text)
      case None => div()

  private def footerSection: Fragment =
    div(style =
      Style(
        width = Some(500.px),
        marginTop = 44.px,
        backgroundColor = Some(AccentColor),
        backgroundImage = assets.accentTexture.map(_.toString),
        backgroundBleed = true,
        // keep the band near the page bottom unless the flowed content
        // above pushes it lower
        maxTop = Some(200f)
      )
    )(
      p(columnHeaderStyle.copy(textAlign = Center, marginTop = 30.px))(labels.contact),
      p(bodyStyle.copy(textAlign = Center, marginTop = 18.px, whiteSpace = Wrap))(
        conf.business
      ),
      p(
        serifHeadingStyle.copy(
          fontWeight = FontWeight.Normal,
          fontStyle = Italic,
          fontSize = 11,
          textAlign = Center,
          marginTop = 20.px
        )
      )(labels.thankYou),
      assets.logo match
        case Some(logo) =>
          // small roundel bleeding off the lower-left page edge
          absoluteImg(logo.toString, x = 15, y = 28, width = 120, height = 120)
        case None => div()
    )

  private val pageBackground: Option[PageBackground] =
    Some(
      assets.paperTexture
        .map(path => PageBackground.Texture(path.toString))
        .getOrElse(PageBackground.Fill(PaperColor))
    )

  def build: DocumentSpec =
    val firstPage = page(background = pageBackground)(
      mastheadSection,
      partiesSection,
      itemsSection,
      totalsSection,
      copyrightSection,
      footerSection
    )
    val appendixPages = conf.appendices.zipWithIndex.map { (appendix, idx) =>
      base.appendixPage(appendix, idx).copy(background = pageBackground)
    }
    document((Vector(firstPage) ++ appendixPages)*)
}
