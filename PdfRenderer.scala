import org.apache.pdfbox.pdmodel.{PDDocument, PDPage, PDPageContentStream}
import org.apache.pdfbox.pdmodel.font.{PDType1Font, PDFont, Standard14Fonts}
import org.apache.pdfbox.pdmodel.font.PDType0Font
import Standard14Fonts.FontName
import org.apache.pdfbox.pdmodel.common.PDRectangle
import org.apache.pdfbox.pdmodel.graphics.image.PDImageXObject
import scala.collection.mutable

enum Fonts(name: FontName) extends PDType1Font(name):
  case Helvetica extends Fonts(FontName.HELVETICA)
  case HelveticaBold extends Fonts(FontName.HELVETICA_BOLD)
  case HelveticaOblique extends Fonts(FontName.HELVETICA_OBLIQUE)
  case Courier extends Fonts(FontName.COURIER)
  case TimesRoman extends Fonts(FontName.TIMES_ROMAN)
  case TimesBold extends Fonts(FontName.TIMES_BOLD)
  case TimesItalic extends Fonts(FontName.TIMES_ITALIC)

object PdfRenderer:
  private final class PdfFontCatalog(
      document: PDDocument,
      fontFiles: Map[FontRef, os.Path]
  ) extends FontMetrics:
    private val MaxCachedTextLength = 64
    private val MaxWidthCacheEntries = 4096
    private val widthCache = mutable.HashMap.empty[(FontRef, Int, String), Float]
    private val customFonts = mutable.HashMap.empty[FontRef, PDFont]

    private def standardFont(font: FontRef): PDFont =
      font match
        case FontRef.Helvetica        => Fonts.Helvetica
        case FontRef.HelveticaBold    => Fonts.HelveticaBold
        case FontRef.HelveticaOblique => Fonts.HelveticaOblique
        case FontRef.Courier          => Fonts.Courier
        case FontRef.TimesRoman       => Fonts.TimesRoman
        case FontRef.TimesBold        => Fonts.TimesBold
        case FontRef.TimesItalic      => Fonts.TimesItalic
        case FontRef.Monospace        => Fonts.Courier

    def pdfFont(font: FontRef): PDFont =
      fontFiles.get(font) match
        case Some(path) =>
          customFonts.getOrElseUpdate(font, PDType0Font.load(document, path.toIO))
        case None => standardFont(font)

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

  private final class PdfImageCatalog(document: PDDocument):
    private val cache = mutable.HashMap.empty[String, PDImageXObject]

    def image(src: String): PDImageXObject =
      cache.getOrElseUpdate(src, PDImageXObject.createFromFile(src, document))

  private def rectangle(pageSize: PageSize): PDRectangle =
    pageSize match
      case PageSize.A4 => PDRectangle.A4

  private def renderPage(
      document: PDDocument,
      pageLayout: LayoutPage,
      fonts: PdfFontCatalog,
      images: PdfImageCatalog
  ): Unit =
    val page = new PDPage(rectangle(pageLayout.size))
    document.addPage(page)

    def setFillColor(contentStream: PDPageContentStream, color: Rgb): Unit =
      contentStream.setNonStrokingColor(color.r / 255f, color.g / 255f, color.b / 255f)

    val contentStream = new PDPageContentStream(document, page)
    try
      for element <- pageLayout.elements do
        element match
          case PageElement.Text(font, fontSize, color, x, y, steps) =>
            setFillColor(contentStream, color)
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
          case PageElement.Rect(x, y, width, height, color) =>
            setFillColor(contentStream, color)
            contentStream.addRect(x, y, width, height)
            contentStream.fill()
          case PageElement.Image(src, x, y, width, height) =>
            contentStream.drawImage(images.image(src), x, y, width, height)
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
      fontFiles: Map[FontRef, os.Path],
      documentSpec: DocumentSpec
  ): Unit =
    val document = new PDDocument()
    try
      val fonts = new PdfFontCatalog(document, fontFiles)
      Logger.info("Built font catalog.")
      val images = new PdfImageCatalog(document)
      val layout = LayoutCompiler.compile(documentSpec, fonts)
      Logger.info("Compiled layout.")
      for (pageLayout, idx) <- layout.pages.zipWithIndex do
        renderPage(document, pageLayout, fonts, images)
        Logger.info(s"Rendered page ${idx + 1} of ${layout.pages.size}")
      end for
      tempFileOp(outputPath)(tempOutputPath => document.save(tempOutputPath.toIO))
    finally document.close()
