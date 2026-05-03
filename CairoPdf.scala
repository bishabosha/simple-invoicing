import scala.scalanative.unsafe.*

type CairoContext = CStruct0
type CairoSurface = CStruct0
type CairoTextExtents = CStruct6[CDouble, CDouble, CDouble, CDouble, CDouble, CDouble]

@extern
@link("cairo")
object CairoPdfApi:
  def cairo_pdf_surface_create(
      filename: CString,
      widthInPoints: CDouble,
      heightInPoints: CDouble
  ): Ptr[CairoSurface] = extern

  def cairo_pdf_surface_set_size(
      surface: Ptr[CairoSurface],
      widthInPoints: CDouble,
      heightInPoints: CDouble
  ): Unit = extern

  def cairo_surface_finish(surface: Ptr[CairoSurface]): Unit = extern
  def cairo_surface_destroy(surface: Ptr[CairoSurface]): Unit = extern
  def cairo_surface_status(surface: Ptr[CairoSurface]): CInt = extern

  def cairo_create(surface: Ptr[CairoSurface]): Ptr[CairoContext] = extern
  def cairo_destroy(cr: Ptr[CairoContext]): Unit = extern
  def cairo_get_target(cr: Ptr[CairoContext]): Ptr[CairoSurface] = extern
  def cairo_status(cr: Ptr[CairoContext]): CInt = extern
  def cairo_status_to_string(status: CInt): CString = extern

  def cairo_select_font_face(
      cr: Ptr[CairoContext],
      family: CString,
      slant: CInt,
      weight: CInt
  ): Unit = extern

  def cairo_set_font_size(cr: Ptr[CairoContext], size: CDouble): Unit = extern
  def cairo_text_extents(
      cr: Ptr[CairoContext],
      utf8: CString,
      extents: Ptr[CairoTextExtents]
  ): Unit = extern

  def cairo_move_to(cr: Ptr[CairoContext], x: CDouble, y: CDouble): Unit = extern
  def cairo_line_to(cr: Ptr[CairoContext], x: CDouble, y: CDouble): Unit = extern
  def cairo_set_line_width(cr: Ptr[CairoContext], width: CDouble): Unit = extern
  def cairo_set_source_rgb(cr: Ptr[CairoContext], red: CDouble, green: CDouble, blue: CDouble): Unit =
    extern
  def cairo_rectangle(
      cr: Ptr[CairoContext],
      x: CDouble,
      y: CDouble,
      width: CDouble,
      height: CDouble
  ): Unit = extern
  def cairo_fill(cr: Ptr[CairoContext]): Unit = extern
  def cairo_stroke(cr: Ptr[CairoContext]): Unit = extern
  def cairo_show_text(cr: Ptr[CairoContext], utf8: CString): Unit = extern
  def cairo_show_page(cr: Ptr[CairoContext]): Unit = extern

object CairoPdf:
  final val StatusSuccess = 0

  final val FontSlantNormal = 0
  final val FontSlantItalic = 1
  final val FontSlantOblique = 2

  final val FontWeightNormal = 0
  final val FontWeightBold = 1

  def check(status: CInt, action: String): Unit =
    if status != StatusSuccess then
      val message = fromCString(CairoPdfApi.cairo_status_to_string(status))
      sys.error(s"Cairo failed while ${action}: ${message}")
