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

def splitText(
    text: String,
    width: Int,
    font: FontRef,
    fontSize: Int,
    fontMetrics: FontMetrics
): List[String] =
  val words = text.split(" ")
  val lines = List.newBuilder[String]
  var currentLine = StringBuilder()
  for word <- words do
    if word.contains("\u000A") then
      word.split("\u000A") match
        case Array(lastWord, initNext) =>
          lines += (currentLine ++= lastWord).toString
          currentLine = StringBuilder() ++= initNext ++= " "
        case Array(lastWord) =>
          lines += (currentLine ++= lastWord).toString
          currentLine = StringBuilder()
        case Array() =>
          lines += currentLine.toString
          currentLine = StringBuilder()
    else
      val currentWidth =
        fontMetrics.stringWidth(
          font,
          currentLine.toString + " " + word,
          fontSize
        )
      if currentWidth > width then
        lines += currentLine.toString
        currentLine = StringBuilder()
      end if
      currentLine ++= word ++ " "
  end for
  if currentLine.nonEmpty then lines += currentLine.toString
  lines.result()

case class TextStyle(font: FontRef, size: Int, lineHeight: Float = 15)

enum HorizontalAnchor:
  case Left(x: Float)
  case Right(rightEdge: Float)

case class FixedText(
    anchor: HorizontalAnchor,
    y: Float,
    style: TextStyle,
    steps: Vector[TextStep]
)

case class FlowText(x: Float, style: TextStyle, steps: Vector[TextStep])

case class TableColumn(
    header: String,
    width: Float,
    wrapWidth: Option[Int] = None
)

case class TableCell(text: String, wrap: Boolean = false)

case class TableRow(cells: Vector[TableCell])

case class TableSpec(
    columns: Vector[TableColumn],
    headerStyle: TextStyle,
    bodyStyle: TextStyle,
    rows: Vector[TableRow],
    lineWidth: Float = 0.5f,
    rowHeight: Float = 15,
    gridOffsetY: Float = 4,
    textInsetX: Float = 5
)

case class BulletListSpec(
    style: TextStyle,
    width: Int,
    items: Vector[String],
    x: Float = 0,
    bullet: String = "-",
    bulletGap: Float = 10,
    itemGap: Float = 5
)

enum FlowBlock:
  case Gap(height: Float)
  case Row(texts: Vector[FlowText])
  case WrappedText(x: Float, width: Int, style: TextStyle, text: String)
  case Rule(startX: Float = 0, endX: Float = 500, width: Float = 0.5f)
  case Table(spec: TableSpec)
  case BulletList(spec: BulletListSpec)

case class FlowRegion(x: Float, topY: Float, blocks: Vector[FlowBlock])

case class PageSpec(
    size: PageSize = PageSize.A4,
    fixed: Vector[FixedText] = Vector.empty,
    flows: Vector[FlowRegion] = Vector.empty
)

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

  def txt(content: String, dx: Float = 0, dy: Float = 0): TextStep =
    textStep(content, dx = dx, dy = dy)

  def fixedLeft(x: Float, y: Float, style: TextStyle)(lines: String*): FixedText =
    FixedText(
      HorizontalAnchor.Left(x),
      y,
      style,
      stepsForLines(lines, style.lineHeight)
    )

  def fixedRight(
      rightEdge: Float,
      y: Float,
      style: TextStyle,
      text: String
  ): FixedText =
    FixedText(
      HorizontalAnchor.Right(rightEdge),
      y,
      style,
      Vector(textStep(text))
    )

  def span(style: TextStyle, x: Float = 0)(steps: TextStep*): FlowText =
    FlowText(x, style, steps.toVector)

  def row(texts: FlowText*): Fragment =
    Vector(FlowBlock.Row(texts.toVector))

  def div(children: Fragment*): Fragment =
    children.foldLeft(Vector.empty[FlowBlock])(_ ++ _)

  def p(style: TextStyle, x: Float = 0)(lines: String*): Fragment =
    row(FlowText(x, style, stepsForLines(lines, style.lineHeight)))

  def wrapped(style: TextStyle, width: Int, x: Float = 0)(text: String): Fragment =
    Vector(FlowBlock.WrappedText(x, width, style, text))

  def gap(height: Float): Fragment =
    Vector(FlowBlock.Gap(height))

  def hr(
      startX: Float = 0,
      endX: Float = 500,
      width: Float = 0.5f
  ): Fragment =
    Vector(FlowBlock.Rule(startX, endX, width))

  def th(header: String, width: Float, wrapWidth: Option[Int] = None): TableColumn =
    TableColumn(header, width, wrapWidth)

  def td(text: String, wrap: Boolean = false): TableCell =
    TableCell(text, wrap)

  def tr(cells: TableCell*): TableRow =
    TableRow(cells.toVector)

  def table(
      headerStyle: TextStyle,
      bodyStyle: TextStyle,
      lineWidth: Float = 0.5f,
      rowHeight: Float = 15,
      gridOffsetY: Float = 4,
      textInsetX: Float = 5
  )(columns: TableColumn*)(rows: TableRow*): Fragment =
    Vector(
      FlowBlock.Table(
        TableSpec(
          columns = columns.toVector,
          headerStyle = headerStyle,
          bodyStyle = bodyStyle,
          rows = rows.toVector,
          lineWidth = lineWidth,
          rowHeight = rowHeight,
          gridOffsetY = gridOffsetY,
          textInsetX = textInsetX
        )
      )
    )

  def li(text: String): String =
    text

  def ul(
      style: TextStyle,
      width: Int,
      x: Float = 0,
      bullet: String = "-",
      bulletGap: Float = 10,
      itemGap: Float = 5
  )(items: String*): Fragment =
    Vector(
      FlowBlock.BulletList(
        BulletListSpec(
          style = style,
          width = width,
          items = items.toVector,
          x = x,
          bullet = bullet,
          bulletGap = bulletGap,
          itemGap = itemGap
        )
      )
    )

  def body(topY: Float, x: Float = 50)(children: Fragment*): FlowRegion =
    FlowRegion(x = x, topY = topY, blocks = div(children*))

  def page(
      size: PageSize = PageSize.A4,
      fixed: Seq[FixedText] = Seq.empty
  )(regions: FlowRegion*): PageSpec =
    PageSpec(size = size, fixed = fixed.toVector, flows = regions.toVector)

  def document(pages: PageSpec*): DocumentSpec =
    DocumentSpec(pages.toVector)

object LayoutCompiler:
  private def textDepth(steps: Vector[TextStep]): Float =
    var offsetY = 0f
    var minY = 0f
    for step <- steps do
      offsetY += step.dy
      if offsetY < minY then minY = offsetY
    -minY

  private def emitText(
      page: PageBuilder,
      x: Float,
      y: Float,
      style: TextStyle,
      steps: Vector[TextStep]
  ): Unit =
    if steps.nonEmpty then
      page.text(style.font, style.size, x, y)(steps*)

  private def anchoredX(fixed: FixedText, fontMetrics: FontMetrics): Float =
    fixed.anchor match
      case HorizontalAnchor.Left(x) =>
        x
      case HorizontalAnchor.Right(rightEdge) =>
        val firstText = fixed.steps.headOption.map(_.text).getOrElse("")
        rightEdge - fontMetrics.stringWidth(
          fixed.style.font,
          firstText,
          fixed.style.size
        )

  private def compileTable(
      page: PageBuilder,
      baseX: Float,
      topY: Float,
      spec: TableSpec,
      fontMetrics: FontMetrics
  ): Float =
    val colBoundaries = spec.columns.scanLeft(0f)(_ + _.width).toVector
    val colStarts = colBoundaries.init
    val totalWidth = colBoundaries.lastOption.getOrElse(0f)
    val headerY = topY - spec.rowHeight

    for (column, x) <- spec.columns.zip(colStarts) do
      emitText(
        page,
        baseX + x + spec.textInsetX,
        headerY,
        spec.headerStyle,
        Vector(textStep(column.header))
      )

    val rowLines =
      spec.rows.map { row =>
        row.cells.zip(spec.columns).map { (cell, column) =>
          if cell.wrap then
            splitText(
              cell.text,
              column.wrapWidth.getOrElse(column.width.toInt),
              spec.bodyStyle.font,
              spec.bodyStyle.size,
              fontMetrics
            )
          else List(cell.text)
        }
      }

    val rowHeights = rowLines.map(_.map(_.size).maxOption.getOrElse(1))
    val tableBottomY = topY - ((rowHeights.sum + 1) * spec.rowHeight)

    locally:
      var i = 0
      for bucketSize <- 1 +: rowHeights ++: Seq(1) do
        val y = topY - spec.gridOffsetY - i * spec.rowHeight
        page.line(spec.lineWidth, baseX, y, baseX + totalWidth, y)
        i += bucketSize
      end for

    for x <- colBoundaries do
      page.line(
        spec.lineWidth,
        baseX + x,
        topY - spec.gridOffsetY,
        baseX + x,
        tableBottomY - spec.gridOffsetY
      )

    var rowY = topY - (spec.rowHeight * 2)
    for (cells, rowHeightUnits) <- rowLines.zip(rowHeights) do
      for ((cellLines, _), x) <- cells.zip(spec.columns).zip(colStarts) do
        emitText(
          page,
          baseX + x + spec.textInsetX,
          rowY,
          spec.bodyStyle,
          Html.stepsForLines(cellLines, spec.bodyStyle.lineHeight)
        )
      end for
      rowY -= spec.rowHeight * rowHeightUnits
    end for

    spec.rowHeight * (rowHeights.sum + 1)

  private def compileBulletList(
      page: PageBuilder,
      baseX: Float,
      topY: Float,
      spec: BulletListSpec,
      fontMetrics: FontMetrics
  ): Float =
    var currentY = topY
    for item <- spec.items do
      val lines =
        splitText(item, spec.width, spec.style.font, spec.style.size, fontMetrics)
      val stepsB = Vector.newBuilder[TextStep]
      stepsB += textStep(spec.bullet)
      lines.headOption.foreach { firstLine =>
        stepsB += textStep(firstLine, dx = spec.bulletGap)
      }
      for line <- lines.tail do
        stepsB += textStep(line, dy = -spec.style.lineHeight)
      end for
      emitText(page, baseX + spec.x, currentY, spec.style, stepsB.result())
      currentY -= spec.style.lineHeight * lines.length
      currentY -= spec.itemGap
    end for
    topY - currentY

  private def compileFlowBlock(
      page: PageBuilder,
      baseX: Float,
      currentY: Float,
      block: FlowBlock,
      fontMetrics: FontMetrics
  ): Float =
    block match
      case FlowBlock.Gap(height) =>
        currentY - height
      case FlowBlock.Row(texts) =>
        var maxDepth = 0f
        for text <- texts do
          emitText(page, baseX + text.x, currentY, text.style, text.steps)
          maxDepth = maxDepth.max(textDepth(text.steps))
        end for
        currentY - maxDepth
      case FlowBlock.WrappedText(x, width, style, text) =>
        val lines = splitText(text, width, style.font, style.size, fontMetrics)
        compileFlowBlock(
          page,
          baseX,
          currentY,
          FlowBlock.Row(
            Vector(FlowText(x, style, Html.stepsForLines(lines, style.lineHeight)))
          ),
          fontMetrics
        )
      case FlowBlock.Rule(startX, endX, width) =>
        page.line(width, baseX + startX, currentY, baseX + endX, currentY)
        currentY
      case FlowBlock.Table(spec) =>
        currentY - compileTable(page, baseX, currentY, spec, fontMetrics)
      case FlowBlock.BulletList(spec) =>
        currentY - compileBulletList(page, baseX, currentY, spec, fontMetrics)

  private def compilePage(
      pageSpec: PageSpec,
      fontMetrics: FontMetrics
  ): LayoutPage =
    val page = new PageBuilder(pageSpec.size)

    for fixed <- pageSpec.fixed do
      emitText(
        page,
        anchoredX(fixed, fontMetrics),
        fixed.y,
        fixed.style,
        fixed.steps
      )
    end for

    for flow <- pageSpec.flows do
      var currentY = flow.topY
      for block <- flow.blocks do
        currentY = compileFlowBlock(page, flow.x, currentY, block, fontMetrics)
      end for
    end for

    page.result()

  def compile(
      documentSpec: DocumentSpec,
      fontMetrics: FontMetrics
  ): LayoutDocument =
    LayoutDocument(documentSpec.pages.map(compilePage(_, fontMetrics)))
