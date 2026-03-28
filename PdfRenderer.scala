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
  val InconsolataRegular =
    os.pwd / "fonts" / "inconsolata-4" / "Inconsolata-Regular.ttf"

trait ExtendedFonts(doc: PDDocument):
  lazy val InconsolataRegular = Font(FontPaths.InconsolataRegular)
  private def Font(path: os.Path) = PDType0Font.load(doc, path.toIO)

end ExtendedFonts

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
      useInconsolata: Boolean,
      documentSpec: DocumentSpec
  ): Unit =
    val document = new PDDocument()
    try
      val fonts = new PdfFontCatalog(document, useInconsolata)
      val layout = LayoutCompiler.compile(documentSpec, fonts)
      for pageLayout <- layout.pages do
        renderPage(document, pageLayout, fonts)
      end for
      document.save(outputPath.toIO)
    finally document.close()
