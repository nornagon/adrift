package adrift.display

import adrift.GameState
import adrift.items.Item

class AssemblyScreen(display: GLFWDisplay, state: GameState) extends Screen {
  override def key(key: Int, scancode: Int, action: Int, mods: Int): Unit = {

  }

  override def render(renderer: GlyphRenderer): Unit = {
    val buildable = state.buildableItems(state.nearbyItems.map(_._1))

    renderer.frame(
      left = 1,
      top = 1,
      width = 40,
      title = "Assemble what?",
      lines =
        s"""
           |${buildable.map(k => s"${Item.item_display(k.display).toChar} ${k.name}").mkString("\n")}
         """.stripMargin.split("\n").init.tail
    )
  }
}
