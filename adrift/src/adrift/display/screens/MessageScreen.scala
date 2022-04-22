package adrift.display.screens

import adrift.Rect
import adrift.display.screens.UI.Color.*
import adrift.display.{GLFWDisplay, GlyphRenderer, Screen, layout}
import org.lwjgl.glfw.GLFW.*

class MessageScreen(display: GLFWDisplay) extends Screen {
  override def key(key: Int, scancode: Int, action: Int, mods: Int): Unit = {
    (action, key) match {
      case (GLFW_PRESS, GLFW_KEY_SPACE | GLFW_KEY_ENTER) =>
        display.popScreen()
      case _ =>
    }
  }

  override def render(renderer: GlyphRenderer): Unit = {
    import layout.*

    val bounds = Rect.centeredAt(renderer.bounds.center, 30, 40)

    val w = Column(
      crossAxisAlignment = CrossAxisAlignment.Stretch,
      children = Seq(
        Text(
          """The U.S.S. Dandelion. A Neptune-class seed ship, bound for Luyten's Star, planet b, twelve light-years from home. You barely made it two.
            |
            |Was it a NAVSYS failure? Or was the whole Diaspora project a doomed dream from the start? You didn't have time to find out. Dandelion's cryostats dumped twenty-five of you, half-frozen and coughing onto the deck. Through the haze of reanimation, the full meaning of the hissing and creaking sounds you were hearing arrived like a meteor in your awareness.
            |
            |You don't remember your dash to the escape ship, but the pounding pain in your limbs is evidence that you were not supposed to move that fast before you were fully thawed. In your blind panic, you didn't check who was with you. It was clear not everyone would make it.
            |
            |You didn't think you'd be the only one.
            |
            |The airlock thunked closed and you watched the Dandelion spin away into the infinite night, disintegrating. That terrible hissing sound still echoes in your head. Or... wait. That's not your imagination. Is that... coming from aft? Oh, no."""
            .stripMargin
            .withFg(lightGreen)
        ),
        Text("\n"),
        Text("[Press SPACE to continue]".withFg(disabledGreen), textAlignment = TextAlignment.Right),
      )
    )
      .background(darkGreen)
      .lrBorder(fg = lightGreen, bg = darkGreen)

    val r = w.inflate.asInstanceOf[RenderBox]
    r.layout(BoxConstraints(minWidth = bounds.width, maxWidth = bounds.width, maxHeight = bounds.height))
    r.paint(renderer, Offset(bounds.l, bounds.t))
  }
}
