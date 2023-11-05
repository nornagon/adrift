package adrift.display.screens

import adrift.{Color, Rect}
import adrift.display.GlyphRenderer.{AnnotatedString, ColoredString}
import adrift.display.screens.UI.Color.*
import adrift.display.{GLFWDisplay, GlyphRenderer, Screen, layout}
import org.lwjgl.glfw.GLFW.*

class MessageScreen(display: GLFWDisplay) extends Screen {
  import scala.language.implicitConversions
  sealed trait AD
  case class ColorAnn(c: Color) extends AD
  case class DelayAnn(delay: Double) extends AD
  type AS = AnnotatedString[AD]

  implicit class ASStringContext(sc: StringContext) {
    def as[T](args: Any*) = {
      AnnotatedString[T](sc.s(args: _*))
    }
  }


  private val message: Seq[AS] =
    Seq[AS](
      as"The U.S.S. Dandelion. " +
        as"A Neptune-class seed ship, bound for Luyten b, twelve light-years from home. ".withAnn(DelayAnn(0.5): AD) +
        as"You barely made it two.".withAnn(DelayAnn(0.5): AD),
      as"Was it a NAVSYS failure? " +
        as"Or was the whole Diaspora project a doomed dream from the start? ".withAnn(DelayAnn(0.5): AD) +
        as"You didn't have time to find out. ".withAnn(DelayAnn(0.5): AD) +
        as"The Dandelion's cryostats dumped twenty-five of you, half-frozen and coughing onto the deck. ".withAnn(DelayAnn(0.5): AD) +
        as"Through the haze of reanimation, the full meaning of the hissing and creaking sounds you were hearing arrived like a meteor in your awareness.".withAnn(DelayAnn(0.5): AD),
      as"You don't remember your dash to the escape ship, but the pounding pain in your limbs is evidence that you were not supposed to move that fast before you were fully thawed. " +
        as"In your blind panic, you didn't check who was with you. ".withAnn(DelayAnn(0.5): AD) +
        as"It was clear not everyone would make it.".withAnn(DelayAnn(0.5): AD),
      as"You didn't think you'd be the only one.",
      as"The airlock thunked closed and you watched the Dandelion spin away into the infinite night, disintegrating. " +
        as"That terrible hissing sound still echoes in your head. ".withAnn(DelayAnn(0.5): AD) +
        as"Or... ".withAnn(DelayAnn(0.5)) +
        as"wait. ".withAnn(DelayAnn(1)) +
        as"That's not your imagination. ".withAnn(DelayAnn(0.5)) +
        as"Is that... ".withAnn(DelayAnn(0.5)) +
        as"coming from aft? ".withAnn(DelayAnn(1)) +
        as"Oh, ".withAnn(DelayAnn(1)) +
        as"no.".withAnn(DelayAnn(0.2))
    )

  implicit def csFromAs(as: AS): ColoredString = new AnnotatedString[Color](as.s, as.as.flatMap { a =>
    a.data match {
      case ColorAnn(c) => Some(a.copy(data = c))
      case _ => None
    }
  })

  // cs"${color(lightgreen)}{The U.S.S. Dandelion.${delay(0.5)} A Neptune-class seed ship, bound for Luyten b, twelve light-years from home.${delay(0.5)} You barely made it two.}"
  // cs"<lightgreen>The U.S.S. Dandelion.<delay:0.5> A Neptune-class seed ship, bound for Luyten b, twelve light-years from home.<delay:0.5> You barely made it two."

  private var i = 0

  private var animStart = System.nanoTime()
  private val charsPerSec = 60

  private def delayAnns = message(i).as.filter(a => a.data match { case _: DelayAnn => true; case _ => false }).asInstanceOf[Seq[GlyphRenderer.Ann[DelayAnn]]]
  private def mcts(t: Double) = {
    var start = 0d
    var startI = 0
    val delays = for (a <- delayAnns) yield {
      val aStart = start + (a.from - startI).toDouble / charsPerSec
      val aEnd = aStart + a.data.delay
      start = aEnd
      startI = a.from
      (aStart, aEnd)
    }
    val applicableDelays = delays.filter(_._1 < t)
    var c = 0d
    var le = 0d
    for ((s,e) <- applicableDelays) {
      c += (s - le) * charsPerSec
      le = e
    }
    if (t > le)
      c += (t - le) * charsPerSec
    c
  }

  private def elapsed = (System.nanoTime() - animStart) / 1e9
  private def maxCharsToShow = (mcts(elapsed)).toInt
  override def animating: Boolean = maxCharsToShow < message(i).length

  override def key(key: Int, scancode: Int, action: Int, mods: Int): Unit = {
    (action, key) match {
      case (GLFW_PRESS, GLFW_KEY_SPACE | GLFW_KEY_ENTER) =>
        if (animating)
          animStart = 0
        else
          if (i < message.length - 1) {
            i += 1
            animStart = System.nanoTime()
          } else
            display.popScreen()
      case _ =>
    }
  }

  private def hideCharsAfter(cs: ColoredString, n: Int): ColoredString = {
    cs.substring(0, math.min(n, cs.length)) + cs.substring(math.min(n, cs.length), cs.length).withFg(Color.Transparent)
  }

  override def render(renderer: GlyphRenderer): Unit = {
    import layout.*

    val bounds = Rect.centeredAt(renderer.bounds.center, 30, 40)

    val w = Column(
      crossAxisAlignment = CrossAxisAlignment.Stretch,
      children =
        message.take(i).map(s => Text((s + as"\n\n").withAnn(ColorAnn(lightGreen)))) ++ Seq(
          Text(hideCharsAfter(message(i).withAnn(ColorAnn(lightGreen)), maxCharsToShow)),
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
