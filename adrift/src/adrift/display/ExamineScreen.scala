package adrift.display
import adrift.display.CP437.BoxDrawing
import adrift.items.{Item, Message}
import adrift.{Action, Color, GameState, Location, OnFloor}
import org.lwjgl.glfw.GLFW._

object DirectionKey {
  def unapply(key: Int): Option[(Int, Int)] =
      key match {
        case GLFW_KEY_LEFT | GLFW_KEY_H => Some((-1, 0))
        case GLFW_KEY_DOWN | GLFW_KEY_J => Some((0, 1))
        case GLFW_KEY_UP | GLFW_KEY_K => Some((0, -1))
        case GLFW_KEY_RIGHT | GLFW_KEY_L => Some((1, 0))
        case GLFW_KEY_Y => Some((-1, -1))
        case GLFW_KEY_U => Some((1, -1))
        case GLFW_KEY_B => Some((-1, 1))
        case GLFW_KEY_N => Some((1, 1))
        case _ => None
      }

}

class ExamineDirectionScreen(display: GLFWDisplay, state: GameState) extends Screen {
  override def key(key: Int, scancode: Int, action: Int, mods: Int): Unit = {
    if (action == GLFW_RELEASE) {
      key match {
        case DirectionKey((dx, dy)) =>
          display.popScreen()
          val location = state.player + (dx, dy)
          if (state.items.lookup(OnFloor(location)).nonEmpty)
            display.pushScreen(new ExamineScreen(display, state, location))
        case _ =>
      }
    }
  }

  override def render(renderer: GlyphRenderer): Unit = {
    renderer.frame(renderer.bounds.center._1 - 8, 0, 16, null, Seq("Examine where?"))
  }
}

class ExamineScreen(display: GLFWDisplay, state: GameState, location: Location) extends Screen {
  var selected: Int = 0
  private var openStack: Seq[Item] = Seq.empty
  private def items = openStack.lastOption.map(_.parts).getOrElse(state.items.lookup(OnFloor(location)))

  def isOpened(item: Item): Boolean =
    state.sendMessage(item, Message.IsOpened()).opened

  override def key(key: Int, scancode: Int, action: Int, mods: Int): Unit = {
    if (action == GLFW_PRESS || action == GLFW_REPEAT) {
      key match {
        case DirectionKey((0, -1)) =>
          val n = items.size
          selected = (selected + n - 1) % n
        case DirectionKey((0, 1)) =>
          val n = items.size
          selected = (selected + 1) % n
        case GLFW_KEY_O if items(selected).parts.nonEmpty =>
          val item = items(selected)
          if (isOpened(item) || { state.receive(Action.Open(item)); isOpened(item) }) {
            openStack :+= item
            selected = 0
          }
        case GLFW_KEY_C if items(selected).parts.nonEmpty =>
          val item = items(selected)
          if (isOpened(item))
            state.receive(Action.Close(item))
        case GLFW_KEY_ESCAPE | DirectionKey(-1, 0) if openStack.nonEmpty =>
          display.preventDefault()
          val wasSelected = openStack.last
          openStack = openStack.init
          selected = math.max(0, items.indexOf(wasSelected))
        case _ =>
      }
    }
  }

  override def render(renderer: GlyphRenderer): Unit = {
    import GlyphRenderer.{CS, Ann, wrapCS}
    val width = 24
    val Some((sx, sy)) = display.worldToScreen(state)(location.xy)
    val (char, fg, bg) = Appearance.charAtPosition(state, location)
    val lightGreen = Color.fromBytes(217, 255, 102)
    val disabledGreen = Color.fromBytes(77, 102, 0)
    val selectedGreen = Color.fromBytes(0, 140, 0)
    val darkGreen = Color.fromBytes(32, 64, 0)
    renderer.drawChar(sx, sy, char, fg, bg = selectedGreen)
    renderer.drawChar(sx + 1, sy, BoxDrawing.L_R_, fg = lightGreen)
    var nextY = 0
    def sprintln(s: String, fg: Color = lightGreen, bg: Color = darkGreen): Unit =
      sprintlnColored(CS(s, Seq(Ann(0, s.length, fg))), bg = bg)
    def sprintlnColored(ss: CS, defaultFg: Color = lightGreen, bg: Color = darkGreen): Unit = {
      val left = sx + 2
      val top = nextY + sy
      var x = 0
      val y = 0
      renderer.drawChar(left + x, top + y, 0xdd, fg = lightGreen, bg = bg)
      x += 1

      for ((s, anns) <- ss.parts) {
        val fg = anns.lastOption.map(_.fg).getOrElse(defaultFg)
        val maxWidth = width - 1 - x
        renderer.drawString(left + x, top + y, s, fg = fg, bg = bg, maxWidth = maxWidth)
        x += math.min(maxWidth, s.length)
      }

      while (x < width - 1) {
        renderer.drawChar(left + x, top + y, ' ', bg = bg)
        x += 1
      }

      renderer.drawChar(left + x, top + y, 0xde, fg = lightGreen, bg = bg)

      nextY += 1
    }
    openStack.zipWithIndex.foreach { case (it, idx) =>
      val prefix = if (idx == 0) "" else " " * (idx - 1) + "\u00c0"
      sprintln(prefix + state.itemDisplayName(it))
      if (isOpened(it)) {
        renderer.drawChar(sx + width, sy + nextY - 1, 0xff, fg = lightGreen, bg = darkGreen)
      }
    }
    val is = items
    for (y <- is.indices) {
      val bg = if (y == selected) selectedGreen else darkGreen
      val prefix = if (openStack.isEmpty) "" else " " * (openStack.size - 1) + (if (y == is.size - 1) "\u00c0" else "\u00c3")
      val item = is(y)
      sprintln(prefix + state.itemDisplayName(item), bg = bg)
      if (isOpened(item)) {
        renderer.drawChar(sx + width, sy + nextY - 1, 0xff, fg = lightGreen, bg = bg)
      }
    }
    sprintln("")
    val item = is(selected)
    GlyphRenderer.wrap(item.kind.description, width - 2).foreach(sprintln(_, fg = disabledGreen))
    sprintln("")
    val actions = Seq(
      Option.when(item.parts.nonEmpty && !isOpened(item))(CS("open", Seq(Ann(0, 1, lightGreen)))),
      Option.when(item.parts.nonEmpty && isOpened(item))(CS("close", Seq(Ann(0, 1, lightGreen)))),
      Some(CS("diagnose", Seq(Ann(0, 1, lightGreen)))),
      Option.when(openStack.nonEmpty)(CS("remove", Seq(Ann(0, 1, lightGreen)))),
    ).flatten
    val actionStr = wrapCS(actions.reduce(_ + CS(" ", Seq.empty) + _), width - 2)
    actionStr.foreach(sprintlnColored(_, defaultFg = disabledGreen))
  }
}
