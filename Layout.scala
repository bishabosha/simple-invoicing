enum FontRef:
  case Helvetica
  case HelveticaBold
  case HelveticaOblique
  case Courier
  case TimesRoman
  case Monospace

enum PageSize:
  case A4

case class TextStep(dx: Float = 0, dy: Float = 0, text: String)

enum PageElement:
  case Text(
      font: FontRef,
      size: Int,
      x: Float,
      y: Float,
      steps: Vector[TextStep]
  )
  case Line(
      width: Float,
      startX: Float,
      startY: Float,
      endX: Float,
      endY: Float
  )

case class LayoutPage(size: PageSize = PageSize.A4, elements: Vector[PageElement])
case class LayoutDocument(pages: Vector[LayoutPage])

def textStep(text: String, dx: Float = 0, dy: Float = 0): TextStep =
  TextStep(dx = dx, dy = dy, text = text)

trait FontMetrics:
  def stringWidth(font: FontRef, text: String, fontSize: Int): Float

final class PageBuilder(val size: PageSize = PageSize.A4):
  private val elementsB = Vector.newBuilder[PageElement]

  def text(font: FontRef, fontSize: Int, x: Float, y: Float)(steps: TextStep*): Unit =
    elementsB += PageElement.Text(font, fontSize, x, y, steps.toVector)

  def line(
      width: Float,
      startX: Float,
      startY: Float,
      endX: Float,
      endY: Float
  ): Unit =
    elementsB += PageElement.Line(width, startX, startY, endX, endY)

  def result(): LayoutPage =
    LayoutPage(size, elementsB.result())

enum WrapToken:
  case Segment(text: String, width: Float)
  case Break

def tokenizeText(
    text: String,
    font: FontRef,
    fontSize: Int,
    fontMetrics: FontMetrics
): Vector[WrapToken] =
  val space = " "
  val spaceWidth = fontMetrics.stringWidth(font, space, fontSize)
  if text.isEmpty then return Vector(WrapToken.Segment(space, spaceWidth))
  val tokens = Vector.newBuilder[WrapToken]
  var idx = 0
  var afterBreak = false
  while idx < text.length do
    text.charAt(idx) match
      case '\n' =>
        tokens += WrapToken.Break
        afterBreak = true
        idx += 1
      case ' ' =>
        if afterBreak then afterBreak = false
        else tokens += WrapToken.Segment(space, spaceWidth)
        idx += 1
      case _ =>
        afterBreak = false
        val start = idx
        while idx < text.length && text.charAt(idx) != ' ' && text.charAt(idx) != '\n' do
          idx += 1
        val word = text.substring(start, idx)
        val wordWidth = fontMetrics.stringWidth(font, word, fontSize)
        if idx >= text.length then
          tokens += WrapToken.Segment(word + space, wordWidth + spaceWidth)
        else
          text.charAt(idx) match
            case '\n' =>
              tokens += WrapToken.Segment(word, wordWidth)
            case ' ' =>
              tokens += WrapToken.Segment(word + space, wordWidth + spaceWidth)
              idx += 1
              while idx < text.length && text.charAt(idx) == ' ' do
                tokens += WrapToken.Segment(space, spaceWidth)
                idx += 1
              end while
  tokens.result()

def splitText(
    text: String,
    width: Int,
    font: FontRef,
    fontSize: Int,
    fontMetrics: FontMetrics
): List[String] =
  val lines = List.newBuilder[String]
  var currentLine = StringBuilder()
  var currentLineWidth = 0f
  for token <- tokenizeText(text, font, fontSize, fontMetrics) do
    token match
      case WrapToken.Break =>
        lines += currentLine.toString
        currentLine = StringBuilder()
        currentLineWidth = 0f
      case WrapToken.Segment(text, tokenWidth) =>
        if currentLine.nonEmpty && currentLineWidth + tokenWidth > width then
          lines += currentLine.toString
          currentLine = StringBuilder()
          currentLineWidth = 0f
        end if
        currentLine ++= text
        currentLineWidth += tokenWidth
  end for
  if currentLine.nonEmpty then lines += currentLine.toString
  lines.result()

enum CssLength:
  case Px(value: Float)
  case Lh(value: Float)

  def resolve(lineHeight: Float): Float =
    this match
      case CssLength.Px(value) => value
      case CssLength.Lh(value) => value * lineHeight

object CssLength:
  val Zero: CssLength = CssLength.Px(0)

enum TextAlign:
  case Left
  case Right

enum WhiteSpace:
  case NoWrap
  case Wrap

enum FontFamily:
  case Helvetica
  case Courier
  case Times
  case Monospace

enum FontWeight:
  case Normal
  case Bold

enum FontStyle:
  case Normal
  case Italic

case class Style(
    marginTop: CssLength = CssLength.Zero,
    marginBottom: CssLength = CssLength.Zero,
    marginLeft: CssLength = CssLength.Zero,
    width: Option[CssLength] = None,
    paddingLeft: CssLength = CssLength.Zero,
    gap: CssLength = CssLength.Zero,
    textAlign: TextAlign = TextAlign.Left,
    whiteSpace: WhiteSpace = WhiteSpace.NoWrap,
    fontFamily: FontFamily = FontFamily.Helvetica,
    fontSize: Int = 10,
    fontWeight: FontWeight = FontWeight.Normal,
    fontStyle: FontStyle = FontStyle.Normal,
    lineHeight: Float = 15,
    borderWidth: Float = 0.5f,
    marker: String = "-"
):
  def resolveMarginTop(): Float =
    marginTop.resolve(lineHeight)

  def resolveMarginBottom(): Float =
    marginBottom.resolve(lineHeight)

  def resolveMarginLeft(): Float =
    marginLeft.resolve(lineHeight)

  def resolveWidth(): Option[Float] =
    width.map(_.resolve(lineHeight))

  def resolvePaddingLeft(): Float =
    paddingLeft.resolve(lineHeight)

  def resolveGap(): Float =
    gap.resolve(lineHeight)

  def fontRef: FontRef =
    fontFamily match
      case FontFamily.Helvetica =>
        (fontWeight, fontStyle) match
          case (FontWeight.Bold, FontStyle.Normal) => FontRef.HelveticaBold
          case (_, FontStyle.Italic)               => FontRef.HelveticaOblique
          case _                                   => FontRef.Helvetica
      case FontFamily.Courier =>
        FontRef.Courier
      case FontFamily.Times =>
        FontRef.TimesRoman
      case FontFamily.Monospace =>
        FontRef.Monospace

  def textOnly: Style =
    copy(
      marginTop = CssLength.Zero,
      marginBottom = CssLength.Zero,
      marginLeft = CssLength.Zero,
      width = None,
      paddingLeft = CssLength.Zero,
      gap = CssLength.Zero,
      textAlign = TextAlign.Left,
      whiteSpace = WhiteSpace.NoWrap,
      borderWidth = 0.5f,
      marker = "-"
    )

object Style:
  extension (value: Int)
    def px: CssLength =
      CssLength.Px(value.toFloat)

    def lh: CssLength =
      CssLength.Lh(value.toFloat)

  extension (value: Float)
    def px: CssLength =
      CssLength.Px(value)

    def lh: CssLength =
      CssLength.Lh(value)

case class FlowText(style: Style, lines: Vector[String])

case class TableColumn(header: String, style: Style)

case class TableCell(text: String)

case class TableRow(cells: Vector[TableCell])

case class TableSpec(
    columns: Vector[TableColumn],
    headerStyle: Style,
    bodyStyle: Style,
    rows: Vector[TableRow]
)

case class BulletListSpec(itemStyle: Style, items: Vector[String])

enum FlowBlock:
  case Stack(style: Style, children: Vector[FlowBlock])
  case Row(style: Style, texts: Vector[FlowText])
  case Rule(style: Style)
  case Table(style: Style, spec: TableSpec)
  case BulletList(style: Style, spec: BulletListSpec)

case class PageSpec(size: PageSize = PageSize.A4, blocks: Vector[FlowBlock])
case class DocumentSpec(pages: Vector[PageSpec])

object Html:
  type Fragment = Vector[FlowBlock]

  def stepsForLines(lines: Seq[String], lineHeight: Float): Vector[TextStep] =
    lines.headOption match
      case Some(head) =>
        Vector(textStep(head)) ++
          lines.tail.map(line => textStep(line, dy = -lineHeight))
      case None =>
        Vector.empty

  def span(style: Style = Style())(lines: String*): FlowText =
    FlowText(style, lines.toVector)

  def row(texts: FlowText*): Fragment =
    row(Style())(texts*)

  def row(style: Style = Style())(texts: FlowText*): Fragment =
    Vector(FlowBlock.Row(style, texts.toVector))

  def div(children: Fragment*): Fragment =
    div(Style())(children*)

  def div(style: Style = Style())(children: Fragment*): Fragment =
    Vector(FlowBlock.Stack(style, children.iterator.flatMap(_.iterator).toVector))

  def p(style: Style)(lines: String*): Fragment =
    row(style)(FlowText(style.textOnly, lines.toVector))

  def hr(style: Style = Style()): Fragment =
    Vector(FlowBlock.Rule(style))

  def th(header: String, style: Style): TableColumn =
    TableColumn(header, style)

  def td(text: String): TableCell =
    TableCell(text)

  def tr(cells: TableCell*): TableRow =
    TableRow(cells.toVector)

  def table(
      headerStyle: Style,
      bodyStyle: Style,
      style: Style = Style()
  )(columns: TableColumn*)(rows: TableRow*): Fragment =
    Vector(
      FlowBlock.Table(
        style,
        TableSpec(
          columns = columns.toVector,
          headerStyle = headerStyle,
          bodyStyle = bodyStyle,
          rows = rows.toVector
        )
      )
    )

  def li(text: String): String =
    text

  def ul(
      itemStyle: Style,
      style: Style = Style()
  )(items: String*): Fragment =
    Vector(
      FlowBlock.BulletList(
        style,
        BulletListSpec(
          itemStyle = itemStyle,
          items = items.toVector
        )
      )
    )

  def page(size: PageSize = PageSize.A4)(blocks: Fragment*): PageSpec =
    PageSpec(size, blocks.iterator.flatMap(_.iterator).toVector)

  def document(pages: PageSpec*): DocumentSpec =
    DocumentSpec(pages.toVector)

object LayoutCompiler:
  private val DefaultPageOriginX = 50f
  private val DefaultPageOriginY = 750f
  private val DefaultRuleWidth = 500f
  private val DefaultTableTextInsetX = 5f
  private val DefaultTableGridOffsetY = 4f
  private val DefaultListBulletGap = 10f
  private val DefaultListItemGap = 5f

  private case class PreparedText(
      style: Style,
      lines: Vector[String],
      firstLineWidth: Float,
      depth: Float
  )

  private def linesDepth(lines: Vector[String], lineHeight: Float): Float =
    if lines.size <= 1 then 0f
    else lineHeight * (lines.size - 1)

  private def emitText(
      page: PageBuilder,
      x: Float,
      y: Float,
      style: Style,
      steps: Vector[TextStep]
  ): Unit =
    if steps.nonEmpty then
      page.text(style.fontRef, style.fontSize, x, y)(steps*)

  private def requireWidth(style: Style, owner: String): Int =
    style.resolveWidth().map(_.toInt).getOrElse(sys.error(s"$owner requires Style.width"))

  private def resolvedStartY(style: Style, currentY: Float): Float = currentY - style.resolveMarginTop()

  private def resolvedBaseX(style: Style, baseX: Float): Float = baseX + style.resolveMarginLeft()

  private def resolvedOrDefault(length: CssLength, lineHeight: Float, default: Float): Float =
    if length == CssLength.Zero then default else length.resolve(lineHeight)

  private def lineWidth(style: Style, line: String, fontMetrics: FontMetrics): Float =
    fontMetrics.stringWidth(style.fontRef, line, style.fontSize)

  private def rightAlignedText(
      style: Style,
      lines: Vector[String],
      rightEdge: Float,
      fontMetrics: FontMetrics
  ): (Float, Vector[TextStep]) =
    val head = lines.head
    val firstX = rightEdge - lineWidth(style, head, fontMetrics)
    val stepsB = Vector.newBuilder[TextStep]
    stepsB += textStep(head)

    var previousX = firstX
    for line <- lines.tail do
      val x = rightEdge - lineWidth(style, line, fontMetrics)
      stepsB += textStep(line, dx = x - previousX, dy = -style.lineHeight)
      previousX = x
    end for

    (firstX, stepsB.result())

  private def rowFirstLineWidth(
      texts: Vector[PreparedText]
  ): Float =
    var maxX = 0f
    for text <- texts do
      val startX = text.style.resolveMarginLeft()
      maxX = maxX.max(startX + text.firstLineWidth)
    maxX

  private def inheritStyle(style: Style, containerStyle: Style): Style =
    style.copy(
      width = style.width.orElse(containerStyle.width),
      textAlign =
        if style.textAlign == TextAlign.Left then containerStyle.textAlign
        else style.textAlign,
      whiteSpace =
        if style.whiteSpace == WhiteSpace.NoWrap then containerStyle.whiteSpace
        else style.whiteSpace
    )

  private def sameTypography(left: Style, right: Style): Boolean =
    left.fontRef == right.fontRef && left.fontSize == right.fontSize && left.lineHeight == right.lineHeight

  private def resolvedTextLines(
      style: Style,
      lines: Vector[String],
      owner: String,
      fontMetrics: FontMetrics
  ): Vector[String] =
    style.whiteSpace match
      case WhiteSpace.NoWrap =>
        lines
      case WhiteSpace.Wrap =>
        lines.flatMap { line =>
          splitText(
            line,
            requireWidth(style, owner),
            style.fontRef,
            style.fontSize,
            fontMetrics
          )
        }

  private def prepareText(
      rowStyle: Style,
      text: FlowText,
      fontMetrics: FontMetrics
  ): PreparedText =
    val style = inheritStyle(text.style, rowStyle)
    val lines = resolvedTextLines(style, text.lines, "FlowText", fontMetrics)
    PreparedText(
      style = style,
      lines = lines,
      firstLineWidth = lines.headOption.map(lineWidth(style, _, fontMetrics)).getOrElse(0f),
      depth = linesDepth(lines, style.lineHeight)
    )

  private def compileTable(
      page: PageBuilder,
      baseX: Float,
      topY: Float,
      style: Style,
      spec: TableSpec,
      fontMetrics: FontMetrics
  ): Float =
    val rowHeight = spec.bodyStyle.lineHeight
    val lineWidth = style.borderWidth
    val textInsetX = resolvedOrDefault(
      style.paddingLeft,
      style.lineHeight,
      DefaultTableTextInsetX
    )

    val colBoundaries =
      spec.columns.scanLeft(0f) { (sum, column) =>
        sum + requireWidth(column.style, "TableColumn")
      }.toVector
    val colStarts = colBoundaries.init
    val totalWidth = colBoundaries.lastOption.getOrElse(0f)
    val headerY = topY - rowHeight

    for (column, x) <- spec.columns.zip(colStarts) do
      emitText(
        page,
        baseX + x + textInsetX,
        headerY,
        spec.headerStyle,
        Vector(textStep(column.header))
      )

    val rowLines =
      spec.rows.map { row =>
        row.cells.zip(spec.columns).map { (cell, column) =>
          column.style.whiteSpace match
            case WhiteSpace.Wrap =>
              splitText(
                cell.text,
                requireWidth(column.style, "TableColumn"),
                spec.bodyStyle.fontRef,
                spec.bodyStyle.fontSize,
                fontMetrics
              )
            case WhiteSpace.NoWrap =>
              List(cell.text)
        }
      }

    val rowHeights = rowLines.map(_.map(_.size).maxOption.getOrElse(1))
    val tableBottomY = topY - (rowHeights.sum + 1) * rowHeight

    locally:
      var i = 0
      for bucketSize <- 1 +: rowHeights ++: Seq(1) do
        val y = topY - DefaultTableGridOffsetY - i * rowHeight
        page.line(lineWidth, baseX, y, baseX + totalWidth, y)
        i += bucketSize
      end for

    for x <- colBoundaries do
      page.line(
        lineWidth,
        baseX + x,
        topY - DefaultTableGridOffsetY,
        baseX + x,
        tableBottomY - DefaultTableGridOffsetY
      )

    var rowY = topY - (rowHeight * 2)
    for (cells, rowHeightUnits) <- rowLines.zip(rowHeights) do
      for (cellLines, x) <- cells.zip(colStarts) do
        emitText(
          page,
          baseX + x + textInsetX,
          rowY,
          spec.bodyStyle,
          Html.stepsForLines(cellLines, spec.bodyStyle.lineHeight)
        )
      end for
      rowY -= rowHeight * rowHeightUnits
    end for

    rowHeight * (rowHeights.sum + 1)

  private def compileBulletList(
      page: PageBuilder,
      baseX: Float,
      topY: Float,
      style: Style,
      spec: BulletListSpec,
      fontMetrics: FontMetrics
  ): Float =
    val width = requireWidth(style, "BulletList")
    val bulletGap =
      resolvedOrDefault(style.paddingLeft, style.lineHeight, DefaultListBulletGap)
    val itemGap =
      resolvedOrDefault(style.gap, style.lineHeight, DefaultListItemGap)

    var currentY = topY
    for item <- spec.items do
      val lines =
        splitText(
          item,
          width,
          spec.itemStyle.fontRef,
          spec.itemStyle.fontSize,
          fontMetrics
        )
      val stepsB = Vector.newBuilder[TextStep]
      stepsB += textStep(style.marker)
      lines.headOption.foreach { firstLine =>
        stepsB += textStep(firstLine, dx = bulletGap)
      }
      for line <- lines.tail do
        stepsB += textStep(line, dy = -spec.itemStyle.lineHeight)
      end for
      emitText(page, baseX, currentY, spec.itemStyle, stepsB.result())
      currentY -= spec.itemStyle.lineHeight * lines.length
      currentY -= itemGap
    end for
    topY - currentY

  private def compileFlowBlock(
      page: PageBuilder,
      baseX: Float,
      currentY: Float,
      block: FlowBlock,
      inheritedStyle: Option[Style],
      fontMetrics: FontMetrics
  ): Float =
    block match
      case FlowBlock.Stack(style, children) =>
        val resolvedStyle = inheritedStyle.fold(style)(inheritStyle(style, _))
        var nextY = resolvedStartY(resolvedStyle, currentY)
        val childBaseX = resolvedBaseX(resolvedStyle, baseX)
        for (child, idx) <- children.zipWithIndex do
          nextY = compileFlowBlock(
            page,
            childBaseX,
            nextY,
            child,
            Some(resolvedStyle),
            fontMetrics
          )
          if idx < children.size - 1 then nextY -= resolvedStyle.resolveGap()
        end for
        nextY - resolvedStyle.resolveMarginBottom()
      case FlowBlock.Row(style, texts) =>
        val resolvedStyle = inheritedStyle.fold(style)(inheritStyle(style, _))
        val y = resolvedStartY(resolvedStyle, currentY)
        val rawBaseX = resolvedBaseX(resolvedStyle, baseX)
        val preparedTexts = texts.map(prepareText(resolvedStyle, _, fontMetrics))
        val isRightAlignedParagraph =
          resolvedStyle.textAlign == TextAlign.Right &&
            preparedTexts.size == 1 &&
            preparedTexts.head.lines.size > 1

        if isRightAlignedParagraph then
          val text = preparedTexts.head
          val rightEdge = rawBaseX + resolvedStyle.resolveWidth().getOrElse(0f)
          val (x, steps) =
            rightAlignedText(text.style, text.lines, rightEdge, fontMetrics)
          emitText(page, x, y, text.style, steps)
          y - text.depth - resolvedStyle.resolveMarginBottom()
        else
          val rowBaseX =
            resolvedStyle.textAlign match
              case TextAlign.Left =>
                rawBaseX
              case TextAlign.Right =>
                val rightEdge = rawBaseX + resolvedStyle.resolveWidth().getOrElse(0f)
                rightEdge - rowFirstLineWidth(preparedTexts)

          var maxDepth = 0f
          locally:
            var currentStyle: Option[Style] = None
            var currentX = 0f
            var currentLineStartX = 0f
            var currentLineY = 0f
            var stepsB = Vector.newBuilder[TextStep]

            def flushGroup(): Unit =
              currentStyle.foreach { style =>
                emitText(page, currentX, y, style, stepsB.result())
              }
              currentStyle = None
              stepsB = Vector.newBuilder[TextStep]

            for text <- preparedTexts do
              val textX = rowBaseX + text.style.resolveMarginLeft()
              if text.lines.nonEmpty then
                currentStyle match
                  case Some(style) if sameTypography(style, text.style) =>
                    val first = text.lines.head
                    stepsB += textStep(
                      first,
                      dx = textX - currentLineStartX,
                      dy = -currentLineY
                    )
                  case _ =>
                    flushGroup()
                    currentStyle = Some(text.style)
                    currentX = textX
                    currentLineStartX = textX
                    currentLineY = 0f
                    stepsB += textStep(text.lines.head)

                for line <- text.lines.tail do
                  stepsB += textStep(line, dy = -text.style.lineHeight)
                end for
                currentLineStartX = textX
                currentLineY = -text.depth
              end if
              maxDepth = maxDepth.max(text.depth)
            end for
            flushGroup()

          y - maxDepth - resolvedStyle.resolveMarginBottom()
      case FlowBlock.Rule(style) =>
        val y = resolvedStartY(style, currentY)
        val x = resolvedBaseX(style, baseX)
        val width = style.resolveWidth().getOrElse(DefaultRuleWidth)
        page.line(style.borderWidth, x, y, x + width, y)
        y - style.resolveMarginBottom()
      case FlowBlock.Table(style, spec) =>
        val y = resolvedStartY(style, currentY)
        val x = resolvedBaseX(style, baseX)
        val depth = compileTable(page, x, y, style, spec, fontMetrics)
        y - depth - style.resolveMarginBottom()
      case FlowBlock.BulletList(style, spec) =>
        val y = resolvedStartY(style, currentY)
        val x = resolvedBaseX(style, baseX)
        val depth = compileBulletList(page, x, y, style, spec, fontMetrics)
        y - depth - style.resolveMarginBottom()

  private def compilePage(
      pageSpec: PageSpec,
      fontMetrics: FontMetrics
  ): LayoutPage =
    val page = new PageBuilder(pageSpec.size)
    var nextY = DefaultPageOriginY
    for block <- pageSpec.blocks do
      nextY = compileFlowBlock(
        page,
        DefaultPageOriginX,
        nextY,
        block,
        None,
        fontMetrics
      )
    end for
    page.result()

  def compile(
      documentSpec: DocumentSpec,
      fontMetrics: FontMetrics
  ): LayoutDocument =
    LayoutDocument(documentSpec.pages.map(compilePage(_, fontMetrics)))
