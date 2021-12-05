package adrift.display

import adrift.items.ItemKind
import adrift.{Action, GameState, Rect}
import org.lwjgl.glfw.GLFW.*

class AssemblyScreen(display: GLFWDisplay, state: GameState) extends Screen {
  private val buildable = state.buildableItems2(state.nearbyItems)

  def selectedKind: Option[ItemKind] = {
    val is = buildable
    if (cursor >= 0 && cursor < is.size) Some(is(cursor)._1)
    else None
  }

  var cursor = 0

  def moveCursor(d: Int): Unit = cursor = math.max(0, math.min(cursor + d, buildable.size - 1))

  override def key(key: Int, scancode: Int, action: Int, mods: Int): Unit = {
    (action, key) match {
      case (GLFW_PRESS | GLFW_REPEAT, DirectionKey(0, dy)) =>
        moveCursor(dy)
      case (GLFW_PRESS, GLFW_KEY_ENTER) =>
        val (kind, ops) = buildable(cursor)

        display.pushAction(Action.Assemble(kind, ops))
        display.popScreen()
      case _ =>
    }
  }

  override def render(renderer: GlyphRenderer): Unit = {
    import layout3.*
    import UI.Color.*

    val w = LRBorder(
      fg = lightGreen, bg = darkGreen,
      content = Background(
        bg = darkGreen, content = Column(
          crossAxisAlignment = CrossAxisAlignment.Stretch,
          children = Seq(Text("Assemble".withFg(disabledGreen))) ++ {
            if (buildable.isEmpty)
              Seq(Text(""), Text("Nothing can be assembled with the parts available.".withFg(red)))
            else buildable.zipWithIndex.map { case ((itemKind, actions), i) =>
              Row(Seq(
                if (i == cursor)
                  Text("\u0010".withFg(selectedGreen), halfWidth = false)
                else
                  ConstrainedBox(BoxConstraints(minWidth = 1)),
                Flexible(ConstrainedBox(BoxConstraints(maxHeight = 1), Text(itemKind.name.withFg(lightGreen)))),
              ))
            }
          } ++ (selectedKind match {
            case Some(sel) =>
              Seq(
                ConstrainedBox(BoxConstraints(minHeight = 1)),
                Text("Parts".withFg(disabledGreen))
              ) ++ sel.parts.map(part => {
                Text((" " + part.kind.name + (if (part.count != 1) s" x ${part.count}" else "")).withFg(disabledGreen))
              }) ++ Seq(
                ConstrainedBox(BoxConstraints(minHeight = 1)),
                Text(sel.description.withFg(disabledGreen))
              )
            case None => Seq.empty
          }) // ++ actionGuide
        )
      )
    )

    val bounds = Rect.centeredAt(renderer.bounds.center, 25, 40)
    val r = w.inflate.asInstanceOf[RenderBox]
    r.layout(BoxConstraints(minWidth = bounds.width, maxWidth = bounds.width, maxHeight = bounds.height))
    r.paint(renderer, Offset(bounds.l, bounds.t))
  }
}
