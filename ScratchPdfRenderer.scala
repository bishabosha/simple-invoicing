import java.io.ByteArrayOutputStream
import java.nio.file.Files
import java.nio.file.Path
import scala.collection.mutable.ArrayBuffer

object ScratchPdfRenderer:
  private val A4Width = 595.2756f
  private val A4Height = 841.8898f

  private val Cp1252Specials: Map[Char, Int] =
    Map(
      '\u20ac' -> 0x80,
      '\u2018' -> 0x91,
      '\u2019' -> 0x92,
      '\u201c' -> 0x93,
      '\u201d' -> 0x94,
      '\u2022' -> 0x95,
      '\u2013' -> 0x96,
      '\u2014' -> 0x97,
      '\u2122' -> 0x99
    )

  private final class ScratchFontCatalog extends FontMetrics:
    def stringWidth(font: FontRef, text: String, fontSize: Int): Float =
      val units = text.iterator.map(charWidth(font, _)).sum
      units.toFloat / 1000f * fontSize

    private def charWidth(font: FontRef, char: Char): Int =
      font match
        case FontRef.Courier | FontRef.Monospace =>
          600
        case FontRef.TimesRoman =>
          romanWidth(char)
        case FontRef.HelveticaBold =>
          (helveticaWidth(char) * 1.05).round.toInt
        case FontRef.Helvetica | FontRef.HelveticaOblique =>
          helveticaWidth(char)

    private def helveticaWidth(char: Char): Int =
      char match
        case ' '                  => 278
        case '\t'                 => 556
        case '.' | ',' | ':' | ';' => 278
        case '!' | '|' | '\''     => 278
        case '"'                  => 355
        case '(' | ')' | '[' | ']' => 333
        case '-'                  => 333
        case '/' | '\\'           => 278
        case char if char.isDigit => 556
        case char if char >= 'A' && char <= 'Z' => 667
        case char if char >= 'a' && char <= 'z' => 500
        case '\u20ac'             => 556
        case _                    => 500

    private def romanWidth(char: Char): Int =
      char match
        case ' '                  => 250
        case '.' | ',' | ':' | ';' => 250
        case '-'                  => 333
        case char if char.isDigit => 500
        case char if char >= 'A' && char <= 'Z' => 611
        case char if char >= 'a' && char <= 'z' => 444
        case '\u20ac'             => 500
        case _                    => 444

  private final class PdfBuilder:
    private val objects = ArrayBuffer.empty[Array[Byte]]

    def addObject(body: String): Int =
      addObject(ascii(body))

    def addObject(body: Array[Byte]): Int =
      objects += body
      objects.size

    def reserveObject(): Int =
      addObject(Array.emptyByteArray)

    def replaceObject(objectId: Int, body: String): Unit =
      objects(objectId - 1) = ascii(body)

    def writeTo(path: Path): Unit =
      val builtObjects = objects.toVector
      val out = new ByteArrayOutputStream()
      writeAscii(out, "%PDF-1.4\n%\u00e2\u00e3\u00cf\u00d3\n")
      val offsets = builtObjects.zipWithIndex.map { (body, idx) =>
        val offset = out.size()
        writeAscii(out, s"${idx + 1} 0 obj\n")
        out.write(body)
        writeAscii(out, "\nendobj\n")
        offset
      }
      val xrefOffset = out.size()
      writeAscii(out, s"xref\n0 ${builtObjects.size + 1}\n")
      writeAscii(out, "0000000000 65535 f \n")
      for offset <- offsets do writeAscii(out, s"${leftPad(offset.toString, 10, '0')} 00000 n \n")
      writeAscii(
        out,
        s"""trailer
           |<< /Size ${builtObjects.size + 1} /Root 1 0 R >>
           |startxref
           |${xrefOffset}
           |%%EOF
           |""".stripMargin
      )
      val parent = path.toAbsolutePath.getParent
      if parent != null then
        val _ = Files.createDirectories(parent)
      val _ = Files.write(path, out.toByteArray)

  private def pageDimensions(pageSize: PageSize): (Float, Float) =
    pageSize match
      case PageSize.A4 => (A4Width, A4Height)

  private def fontResource(font: FontRef): String =
    font match
      case FontRef.Helvetica        => "F1"
      case FontRef.HelveticaBold    => "F2"
      case FontRef.HelveticaOblique => "F3"
      case FontRef.Courier          => "F4"
      case FontRef.TimesRoman       => "F5"
      case FontRef.Monospace        => "F4"

  private def baseFont(font: FontRef): String =
    font match
      case FontRef.Helvetica        => "Helvetica"
      case FontRef.HelveticaBold    => "Helvetica-Bold"
      case FontRef.HelveticaOblique => "Helvetica-Oblique"
      case FontRef.Courier          => "Courier"
      case FontRef.TimesRoman       => "Times-Roman"
      case FontRef.Monospace        => "Courier"

  private def renderPageContent(pageLayout: LayoutPage): Array[Byte] =
    val content = new StringBuilder()
    val (pageWidth, pageHeight) = pageDimensions(pageLayout.size)
    content.append(s"1 1 1 rg\n")
    content.append(s"0 0 ${num(pageWidth)} ${num(pageHeight)} re\n")
    content.append("f\n")
    content.append("0 0 0 RG\n0 0 0 rg\n")
    for element <- pageLayout.elements do
      element match
        case PageElement.Text(font, fontSize, x, y, steps) =>
          var currentX = x
          var currentY = y
          content.append("BT\n")
          content.append(s"/${fontResource(font)} ${fontSize} Tf\n")
          for step <- steps do
            currentX += step.dx
            currentY += step.dy
            content.append(s"1 0 0 1 ${num(currentX)} ${num(currentY)} Tm\n")
            content.append(s"<${hexText(step.text)}> Tj\n")
          content.append("ET\n")
        case PageElement.Line(width, startX, startY, endX, endY) =>
          content.append(s"${num(width)} w\n")
          content.append(s"${num(startX)} ${num(startY)} m\n")
          content.append(s"${num(endX)} ${num(endY)} l\n")
          content.append("S\n")
    ascii(content.toString)

  private def renderStream(content: Array[Byte]): Array[Byte] =
    val out = new ByteArrayOutputStream()
    writeAscii(out, s"<< /Length ${content.length} >>\nstream\n")
    out.write(content)
    writeAscii(out, "\nendstream")
    out.toByteArray

  private def fontObject(font: FontRef): String =
    s"<< /Type /Font /Subtype /Type1 /BaseFont /${baseFont(font)} /Encoding /WinAnsiEncoding >>"

  private def pageObject(
      pagesObjectId: Int,
      contentObjectId: Int,
      fontObjectIds: Map[String, Int],
      pageSize: PageSize
  ): String =
    val (width, height) = pageDimensions(pageSize)
    val fonts = fontObjectIds.toVector
      .sortBy(_._1)
      .map { (resource, objectId) => s"/${resource} ${objectId} 0 R" }
      .mkString(" ")
    s"""<< /Type /Page
       |   /Parent ${pagesObjectId} 0 R
       |   /MediaBox [0 0 ${num(width)} ${num(height)}]
       |   /Resources << /Font << ${fonts} >> >>
       |   /Contents ${contentObjectId} 0 R
       |>>""".stripMargin

  private def hexText(text: String): String =
    val b = new StringBuilder(text.length * 2)
    for char <- text do
      val byte =
        if char <= 0x7f then char.toInt
        else if char >= 0xa0 && char <= 0xff then char.toInt
        else Cp1252Specials.getOrElse(char, '?'.toInt)
      b.append(hexByte(byte & 0xff))
    b.toString

  private def ascii(text: String): Array[Byte] =
    text.getBytes(java.nio.charset.StandardCharsets.ISO_8859_1)

  private def writeAscii(out: ByteArrayOutputStream, text: String): Unit =
    out.write(ascii(text))

  private def num(value: Float): String =
    val scaled = Math.round(value.toDouble * 1000d)
    val sign = if scaled < 0 then "-" else ""
    val abs = Math.abs(scaled)
    val whole = abs / 1000
    val fraction = (abs % 1000).toInt
    if fraction == 0 then s"${sign}${whole}"
    else
      val padded = leftPad(fraction.toString, 3, '0')
      s"${sign}${whole}.${padded.reverse.dropWhile(_ == '0').reverse}"

  private def leftPad(value: String, width: Int, char: Char): String =
    if value.length >= width then value
    else char.toString * (width - value.length) + value

  private def hexByte(value: Int): String =
    val hex = "0123456789ABCDEF"
    s"${hex.charAt((value >> 4) & 0x0f)}${hex.charAt(value & 0x0f)}"

  def render(outputPath: String, documentSpec: DocumentSpec): Unit =
    val layout = LayoutCompiler.compile(documentSpec, ScratchFontCatalog())
    Logger.info("Compiled layout with scratch PDF font metrics.")

    val builder = PdfBuilder()
    val _ = builder.addObject("<< /Type /Catalog /Pages 2 0 R >>")

    val pagesObjectId = builder.reserveObject()
    val fontObjectIds =
      Vector(
        FontRef.Helvetica,
        FontRef.HelveticaBold,
        FontRef.HelveticaOblique,
        FontRef.Courier,
        FontRef.TimesRoman
      ).map { font =>
        fontResource(font) -> builder.addObject(fontObject(font))
      }.toMap

    val pageObjectIds = Vector.newBuilder[Int]
    for (pageLayout, idx) <- layout.pages.zipWithIndex do
      val contentObjectId = builder.addObject(renderStream(renderPageContent(pageLayout)))
      val pageObjectId = builder.addObject(
        pageObject(pagesObjectId, contentObjectId, fontObjectIds, pageLayout.size)
      )
      pageObjectIds += pageObjectId
      Logger.info(s"Rendered scratch page ${idx + 1} of ${layout.pages.size}")
    end for

    val pages = pageObjectIds.result()
    val kids = pages.map(id => s"${id} 0 R").mkString(" ")
    builder.replaceObject(
      pagesObjectId,
      s"<< /Type /Pages /Kids [${kids}] /Count ${pages.size} >>"
    )
    builder.writeTo(Path.of(outputPath))
