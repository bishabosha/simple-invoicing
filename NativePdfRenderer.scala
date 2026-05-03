import scala.collection.mutable
import scala.scalanative.unsafe.*

object NativePdfRenderer:
  private val A4Width = 595.2756
  private val A4Height = 841.8898

  private final class CairoFontCatalog(
      measureContext: Ptr[CairoContext]
  ) extends FontMetrics:
    private val MaxCachedTextLength = 64
    private val MaxWidthCacheEntries = 4096
    private val widthCache = mutable.HashMap.empty[(FontRef, Int, String), Float]

    private def measuredWidth(font: FontRef, text: String, fontSize: Int): Float =
      Zone.acquire { implicit zone =>
        selectFont(measureContext, font, fontSize)
        val extents = stackalloc[CairoTextExtents]()
        CairoPdfApi.cairo_text_extents(measureContext, toCString(text), extents)
        CairoPdf.check(CairoPdfApi.cairo_status(measureContext), "measuring text")
        extents._5.toFloat
      }

    def stringWidth(font: FontRef, text: String, fontSize: Int): Float =
      if text.length > MaxCachedTextLength then measuredWidth(font, text, fontSize)
      else
        if widthCache.size >= MaxWidthCacheEntries then widthCache.clear()
        widthCache.getOrElseUpdate(
          (font, fontSize, text),
          measuredWidth(font, text, fontSize)
        )

  private def pageDimensions(pageSize: PageSize): (Double, Double) =
    pageSize match
      case PageSize.A4 => (A4Width, A4Height)

  private def fontFace(font: FontRef): (String, CInt, CInt) =
    font match
      case FontRef.Helvetica =>
        ("Helvetica", CairoPdf.FontSlantNormal, CairoPdf.FontWeightNormal)
      case FontRef.HelveticaBold =>
        ("Helvetica", CairoPdf.FontSlantNormal, CairoPdf.FontWeightBold)
      case FontRef.HelveticaOblique =>
        ("Helvetica", CairoPdf.FontSlantOblique, CairoPdf.FontWeightNormal)
      case FontRef.Courier =>
        ("Courier", CairoPdf.FontSlantNormal, CairoPdf.FontWeightNormal)
      case FontRef.TimesRoman =>
        ("Times", CairoPdf.FontSlantNormal, CairoPdf.FontWeightNormal)
      case FontRef.Monospace =>
        ("monospace", CairoPdf.FontSlantNormal, CairoPdf.FontWeightNormal)

  private def selectFont(cr: Ptr[CairoContext], font: FontRef, size: Int): Unit =
    Zone.acquire { implicit zone =>
      val (family, slant, weight) = fontFace(font)
      CairoPdfApi.cairo_select_font_face(cr, toCString(family), slant, weight)
      CairoPdfApi.cairo_set_font_size(cr, size.toDouble)
      CairoPdf.check(CairoPdfApi.cairo_status(cr), s"selecting font ${family}")
    }

  private def renderText(
      cr: Ptr[CairoContext],
      pageHeight: Double,
      font: FontRef,
      fontSize: Int,
      x: Float,
      y: Float,
      steps: Vector[TextStep]
  ): Unit =
    selectFont(cr, font, fontSize)
    var currentX = x.toDouble
    var currentY = y.toDouble
    for step <- steps do
      currentX += step.dx
      currentY += step.dy
      Zone.acquire { implicit zone =>
        CairoPdfApi.cairo_move_to(cr, currentX, pageHeight - currentY)
        CairoPdfApi.cairo_show_text(cr, toCString(step.text))
      }
      CairoPdf.check(CairoPdfApi.cairo_status(cr), "drawing text")

  private def renderLine(
      cr: Ptr[CairoContext],
      pageHeight: Double,
      width: Float,
      startX: Float,
      startY: Float,
      endX: Float,
      endY: Float
  ): Unit =
    CairoPdfApi.cairo_set_line_width(cr, width.toDouble)
    CairoPdfApi.cairo_move_to(cr, startX.toDouble, pageHeight - startY.toDouble)
    CairoPdfApi.cairo_line_to(cr, endX.toDouble, pageHeight - endY.toDouble)
    CairoPdfApi.cairo_stroke(cr)
    CairoPdf.check(CairoPdfApi.cairo_status(cr), "drawing line")

  private def renderPage(
      cr: Ptr[CairoContext],
      pageLayout: LayoutPage
  ): Unit =
    val (pageWidth, pageHeight) = pageDimensions(pageLayout.size)
    CairoPdfApi.cairo_pdf_surface_set_size(
      CairoPdfApi.cairo_get_target(cr),
      pageWidth,
      pageHeight
    )
    CairoPdfApi.cairo_set_source_rgb(cr, 1, 1, 1)
    CairoPdfApi.cairo_rectangle(cr, 0, 0, pageWidth, pageHeight)
    CairoPdfApi.cairo_fill(cr)
    CairoPdfApi.cairo_set_source_rgb(cr, 0, 0, 0)
    for element <- pageLayout.elements do
      element match
        case PageElement.Text(font, fontSize, x, y, steps) =>
          renderText(cr, pageHeight, font, fontSize, x, y, steps)
        case PageElement.Line(width, startX, startY, endX, endY) =>
          renderLine(cr, pageHeight, width, startX, startY, endX, endY)
    CairoPdfApi.cairo_show_page(cr)
    CairoPdf.check(CairoPdfApi.cairo_status(cr), "finishing page")

  def render(
      outputPath: String,
      documentSpec: DocumentSpec
  ): Unit =
    Zone.acquire { implicit zone =>
      val surface = CairoPdfApi.cairo_pdf_surface_create(
        toCString(outputPath),
        A4Width,
        A4Height
      )
      val cr = CairoPdfApi.cairo_create(surface)
      try
        CairoPdf.check(CairoPdfApi.cairo_surface_status(surface), "creating PDF surface")
        CairoPdf.check(CairoPdfApi.cairo_status(cr), "creating drawing context")

        val fonts = new CairoFontCatalog(cr)
        Logger.info("Built native Cairo font catalog.")
        val layout = LayoutCompiler.compile(documentSpec, fonts)
        Logger.info("Compiled layout.")

        for (pageLayout, idx) <- layout.pages.zipWithIndex do
          renderPage(cr, pageLayout)
          Logger.info(s"Rendered page ${idx + 1} of ${layout.pages.size}")
        end for

        CairoPdfApi.cairo_surface_finish(surface)
        CairoPdf.check(CairoPdfApi.cairo_surface_status(surface), "writing PDF")
      finally
        CairoPdfApi.cairo_destroy(cr)
        CairoPdfApi.cairo_surface_destroy(surface)
    }
