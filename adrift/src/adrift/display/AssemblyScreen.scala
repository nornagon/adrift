package adrift.display

import adrift.Action.AssemblyAction
import adrift.items.ItemKind
import adrift.{Action, GameState, Rect}
import org.lwjgl.glfw.GLFW.*

class AssemblyScreen(display: GLFWDisplay, state: GameState) extends Screen {
  private val nearbyItems = state.nearbyItems
  case class BuildInfo(
    kind: ItemKind,
    assemblyPlan: Option[Seq[AssemblyAction]]
  )
  private val buildInfos = state.data.items.values.filter(_.parts.nonEmpty).toSeq.map { k =>
    BuildInfo(k, state.isBuildable(nearbyItems, k))
  }.sortBy(bi => (bi.assemblyPlan.isEmpty, bi.kind.name))

  def selectedKind: Option[ItemKind] = {
    val is = buildInfos
    if (cursor >= 0 && cursor < is.size) Some(is(cursor).kind)
    else None
  }

  var cursor = 0
  var scrollY = 0

  def moveCursor(d: Int): Unit = {
    cursor = math.max(0, math.min(cursor + d, buildInfos.size - 1))
    if (cursor >= scrollY + 30) scrollY += 1
    if (cursor < scrollY) scrollY -= 1
  }

  override def key(key: Int, scancode: Int, action: Int, mods: Int): Unit = {
    (action, key) match {
      case (GLFW_PRESS | GLFW_REPEAT, DirectionKey(0, dy)) =>
        moveCursor(dy)
      case (GLFW_PRESS, GLFW_KEY_ENTER) =>
        val BuildInfo(kind, ops) = buildInfos(cursor)

        if (ops.nonEmpty) {
          display.pushAction(Action.Assemble(kind, ops.get))
          display.popScreen()
        }
      case _ =>
    }
  }

  override def render(renderer: GlyphRenderer): Unit = {
    import layout3.*
    import UI.Color.*

    val w = Column(
      crossAxisAlignment = CrossAxisAlignment.Stretch,
      children = Seq(Text("Assemble".withFg(disabledGreen))) ++ {
        Seq(ConstrainedBox(
          BoxConstraints(maxHeight = 30),
          Scrollable(
            offset = Offset(0, -scrollY),
            content = Column(
              children = buildInfos.zipWithIndex.map { case (BuildInfo(itemKind, actions), i) =>
                val fg = if (actions.nonEmpty) lightGreen else disabledGreen
                Row(Seq(
                  if (i == cursor)
                    Text("\u0010".withFg(selectedGreen), halfWidth = false)
                  else
                    ConstrainedBox(BoxConstraints(minWidth = 1)),
                  Flexible(ConstrainedBox(BoxConstraints(maxHeight = 1), Text(itemKind.name.withFg(fg)))),
                ))
              }
            )
          )
        ))
      } ++ (selectedKind match {
        case Some(sel) =>
          Seq(
            ConstrainedBox(BoxConstraints(minHeight = 1)),
            Text("Parts".withFg(disabledGreen))
          ) ++ sel.parts.map(part => {
            val available = nearbyItems.exists(_.kind == part.kind)
            val fg = if (available) lightGreen else disabledGreen
            Row(
              Seq(
                ConstrainedBox(BoxConstraints(minWidth = 1)),
                Text((part.kind.name + (if (part.count != 1) s" x ${part.count}" else "")).withFg(fg)),
                Spacer(),
              ) ++ (if (part.attachment.nonEmpty) {
                val Some(attachment) = part.attachment
                val available = state.toolsProviding(attachment.assembly).nonEmpty
                Seq(Text(attachment.assembly.id.withFg(if (available) lightGreen else disabledGreen), textAlignment = TextAlignment.Right))
              } else Seq.empty)
            )
          }) ++ Seq(
            ConstrainedBox(BoxConstraints(minHeight = 1)),
            Text(sel.description.withFg(disabledGreen))
          )
        case None => Seq.empty
      }) // ++ actionGuide
    )
      .background(darkGreen)
      .lrBorder(fg = lightGreen, bg = darkGreen)

    val bounds = Rect.centeredAt(renderer.bounds.center, 25, 40)
    val r = w.inflate.asInstanceOf[RenderBox]
    r.layout(BoxConstraints(minWidth = bounds.width, maxWidth = bounds.width, maxHeight = bounds.height))
    r.paint(renderer, Offset(bounds.l, bounds.t))
  }
}
