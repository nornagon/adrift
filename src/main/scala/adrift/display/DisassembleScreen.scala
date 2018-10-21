package adrift.display

import adrift.items.ItemOperation
import adrift.{Action, GameState, ItemLocation}
import org.lwjgl.glfw.GLFW._

class DisassembleScreen(display: GLFWDisplay, state: GameState, itemLocation: ItemLocation) extends Screen {
  var button = 0

  val item = state.itemAtLocation(itemLocation)

  val operations = item.kind.parts.map(_._2).toSet
  val relevantTools = state.nearbyItems.filter(tool => tool._1.kind.provides.toSet.intersect(operations).nonEmpty)
  val tools = operations.toSeq.sorted(Ordering.by((i: ItemOperation) => i.id)).map { op =>
    op -> relevantTools.find { case (i, _) => i.kind.provides.contains(op) }
  }

  val canDisassemble = tools.forall(_._2.nonEmpty)

  override def key(key: Int, scancode: Int, action: Int, mods: Int): Unit = {
    if (action == GLFW_PRESS || action == GLFW_REPEAT)
      key match {
        case GLFW_KEY_L | GLFW_KEY_RIGHT => button = (button + 1) % 2
        case GLFW_KEY_H | GLFW_KEY_LEFT => button = (button - 1 + 2) % 2
        case GLFW_KEY_ENTER =>
          if (button == 1) {
            if (canDisassemble) {
              display.popScreen()
              display.popScreen()
              display.pushAction(Action.Disassemble(itemLocation))
            }
          } else {
            display.popScreen()
          }
        case _ =>
      }
  }

  private val anchor = (8, 5)
  override def render(renderer: GlyphRenderer): Unit = {

    val parts = item.parts.groupBy(_.kind)

    val lines =
      s"""Disassemble ${item.kind.name}?
         |
         |Tools available:
         |
         |${
        tools.map {
          case (op, Some((i, loc))) => s"  ${i.kind.name}"
          case (op, None) => s"  <missing ${op.id}>"
        }.mkString("\n")}
         |
         |Recovery:
         |
         |${parts.map({ case (kind, is) => s"  ${is.size} x ${kind.name}"}).mkString("\n")}
         |
         |
       """.stripMargin.split('\n').init

    object Color {
      val UIDisabled = (0.5f, 0f, 0f, 1f)
      val UIActiveDisabled = (1f, 0f, 0f, 1f)
    }

    renderer.frame(left = anchor._1, top = anchor._2, width = 40, lines = lines)
    renderer.drawString(anchor._1 + 2, anchor._2 + lines.length,
      if (button == 0) "[Nevermind]" else " Nevermind ",
      fg=if (button == 0) (1f, 1f, 1f, 1f) else (0.5f, 0.5f, 0.5f, 1f))
    renderer.drawString(anchor._1 + 15, anchor._2 + lines.length,
      if (button == 1) "[Disassemble]" else " Disassemble ",
      fg=if (button == 1)
        if (canDisassemble) (1f, 1f, 1f, 1f) else Color.UIActiveDisabled
      else
        if (canDisassemble) (0.5f, 0.5f, 0.5f, 1f) else Color.UIDisabled)
  }
}
