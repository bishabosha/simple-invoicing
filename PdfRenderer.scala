import org.apache.pdfbox.pdmodel.{PDDocument, PDPage, PDPageContentStream}
import org.apache.pdfbox.pdmodel.font.{PDType1Font, PDFont, Standard14Fonts}
import org.apache.pdfbox.pdmodel.font.PDType0Font
import Standard14Fonts.FontName
import org.apache.pdfbox.pdmodel.common.PDRectangle
import scala.collection.mutable

enum Fonts(name: FontName) extends PDType1Font(name):
  case Helvetica extends Fonts(FontName.HELVETICA)
  case HelveticaBold extends Fonts(FontName.HELVETICA_BOLD)
  case HelveticaOblique extends Fonts(FontName.HELVETICA_OBLIQUE)
  case Courier extends Fonts(FontName.COURIER)
  case TimesRoman extends Fonts(FontName.TIMES_ROMAN)

object PdfRenderer:
  private final class PdfFontCatalog(
      document: PDDocument,
      monospaceFontPath: Option[os.Path]
  ) extends FontMetrics:
    private val MaxCachedTextLength = 64
    private val MaxWidthCacheEntries = 4096
    private val widthCache = mutable.HashMap.empty[(FontRef, Int, String), Float]

    private lazy val monospaceFont =
      monospaceFontPath
        .map(path => PDType0Font.load(document, path.toIO))
        .getOrElse(Fonts.Courier)

    def pdfFont(font: FontRef): PDFont =
      font match
        case FontRef.Helvetica        => Fonts.Helvetica
        case FontRef.HelveticaBold    => Fonts.HelveticaBold
        case FontRef.HelveticaOblique => Fonts.HelveticaOblique
        case FontRef.Courier          => Fonts.Courier
        case FontRef.TimesRoman       => Fonts.TimesRoman
        case FontRef.Monospace        => monospaceFont

    private def measuredWidth(font: FontRef, text: String, fontSize: Int): Float =
      pdfFont(font).getStringWidth(text) / 1000 * fontSize

    def stringWidth(font: FontRef, text: String, fontSize: Int): Float =
      if text.length > MaxCachedTextLength then measuredWidth(font, text, fontSize)
      else
        if widthCache.size >= MaxWidthCacheEntries then widthCache.clear()
        widthCache.getOrElseUpdate(
          (font, fontSize, text),
          measuredWidth(font, text, fontSize)
        )

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
              if step.dx != 0 || step.dy != 0 then contentStream.newLineAtOffset(step.dx, step.dy)
              contentStream.showText(step.text)
            end for
            contentStream.endText()
          case PageElement.Line(width, startX, startY, endX, endY) =>
            contentStream.setLineWidth(width)
            contentStream.moveTo(startX, startY)
            contentStream.lineTo(endX, endY)
            contentStream.stroke()
    finally contentStream.close()

  def tempFileOp(dest: os.Path)(f: os.Path => Unit): Unit =
    val outputDir = dest / os.up
    os.makeDir.all(outputDir)
    val tempDir =
      os.temp.dir(
        prefix = s".${dest.baseName}-",
        dir = outputDir
      )
    val tempOutputPath = tempDir / dest.last
    try
      f(tempOutputPath)
      os.move(tempOutputPath, dest, replaceExisting = true, atomicMove = true)
    finally
      val _ = os.remove(tempOutputPath)
      val _ = os.remove(tempDir)

  def render(
      outputPath: os.Path,
      monospaceFontPath: Option[os.Path],
      documentSpec: DocumentSpec
  ): Unit =
    val document = new PDDocument()
    try
      val fonts = new PdfFontCatalog(document, monospaceFontPath)
      Logger.info("Built font catalog.")
      val layout = LayoutCompiler.compile(documentSpec, fonts)
      Logger.info("Compiled layout.")
      for (pageLayout, idx) <- layout.pages.zipWithIndex do
        renderPage(document, pageLayout, fonts)
        Logger.info(s"Rendered page ${idx + 1} of ${layout.pages.size}")
      end for
      tempFileOp(outputPath)(tempOutputPath => document.save(tempOutputPath.toIO))
    finally document.close()
