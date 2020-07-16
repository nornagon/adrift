package adrift.display
import adrift.display.CP437.BoxDrawing
import adrift.display.GlyphRenderer.ColoredString
import adrift.items.{Item, ItemKind, Message}
import adrift._
import adrift.items.Message.Provides
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
  private def missingParts = openStack.lastOption.map(getMissingParts).getOrElse(Seq.empty)
  private def getMissingParts(item: Item): Seq[(ItemKind, Int)] = {
    val partCountByKind = item.parts.groupBy(_.kind).view.mapValues(_.size)
    for {
      ip <- item.kind.parts
      presentCount = partCountByKind.getOrElse(ip.kind, 0)
      if presentCount < ip.count
    } yield (ip.kind, ip.count - presentCount)
  }

  sealed trait MenuEntry {
    def text: String
  }
  case class ItemEntry(item: Item) extends MenuEntry {
    override def text: String = item.kind.name
  }
  case class MissingItemEntry(kind: ItemKind, count: Int) extends MenuEntry {
    override def text: String = s"${kind.name}${if (count > 1) s" x $count" else ""}"
  }

  private def menuEntries: Seq[MenuEntry] =
    items.map(ItemEntry) ++
      missingParts.map { case (k, c) => MissingItemEntry(k, c) }

  private val lightGreen = Color.fromBytes(217, 255, 102)
  private val disabledGreen = Color.fromBytes(77, 102, 0)
  private val selectedGreen = Color.fromBytes(0, 140, 0)
  private val darkGreen = Color.fromBytes(32, 64, 0)
  private val red = Color.fromBytes(255, 51, 51)
  private val disabledRed = Color.fromBytes(102, 0, 0)

  case class Command(name: String, execute: () => Unit, available: Boolean = true) {
    private val pat = raw"(.*)\{(.)\}(.*)".r
    private def color = if (available) disabledGreen else disabledRed
    private def highlightColor = if (available) lightGreen else red
    val (char, display) = name match {
      case pat(pre, char, post) =>
        (char, ColoredString(pre).withFg(color) + ColoredString(char).withFg(highlightColor) + ColoredString(post).withFg(color))
    }
    val key: Int = GLFW_KEY_A + char.charAt(0) - 'a'
  }

  private def commands(menuEntry: MenuEntry): Seq[Command] = {
    import Option.when
    menuEntry match {
      case ItemEntry(item) =>
        val Message.IsDiagnosable(diagnosable, diagnoseOp) = state.sendMessage(item, Message.IsDiagnosable())
        val opAvailable = diagnosable && state.toolsProviding(diagnoseOp.get).nonEmpty
        Seq(
          when(item.parts.nonEmpty)
          (Command("{o}pen", () => doOpen(item))),

          when(diagnosable)
          (Command("{d}iagnose", () => doDiagnose(item), available = opAvailable)),

          when(openStack.nonEmpty)
          (Command("{r}emove", () => doRemove(openStack, item), available = {
            val parent = openStack.last
            val disassemblyOp = parent.kind.parts.find(_.kind == item.kind).get.operation
            state.nearbyItems.exists(i => state.sendMessage(i, Provides(disassemblyOp)).provides)
          })),
        ).flatten
      case MissingItemEntry(kind, count) =>
        val partAvailable = state.nearbyItems.find(_.kind == kind)
        Seq(
          Some(Command("{i}nstall", () => doInstall(openStack.last, partAvailable.get), available = partAvailable.nonEmpty))
        ).flatten
    }
  }

  private def doOpen(item: Item): Unit = {
    openStack :+= item
    selected = 0
  }

  private def doDiagnose(item: Item): Unit = {
    state.receive(Action.Diagnose(item))
  }

  private def doRemove(parents: Seq[Item], item: Item): Unit = {
    assert(parents.last.parts.contains(item))
    state.receive(Action.Remove(parents, item))

    var ps = parents
    do {
      selected = math.min(selected, ps.last.parts.size - 1)
      ps = ps.init
    } while (selected < 0 && ps.nonEmpty)
    if (selected < 0) display.popScreen()
  }

  private def doInstall(parent: Item, part: Item): Unit = {
    state.receive(Action.Install(parent, part))
    selected = math.min(selected + 1, menuEntries.size - 1)
  }

  override def key(key: Int, scancode: Int, action: Int, mods: Int): Unit = {
    if (action == GLFW_PRESS || action == GLFW_REPEAT) {
      key match {
        case DirectionKey((0, -1)) =>
          val n = menuEntries.size
          selected = (selected + n - 1) % n
        case DirectionKey((0, 1)) =>
          val n = menuEntries.size
          selected = (selected + 1) % n
        case GLFW_KEY_ESCAPE | DirectionKey(-1, 0) if openStack.nonEmpty =>
          display.preventDefault()
          val wasSelected = openStack.last
          openStack = openStack.init
          selected = math.max(0, items.indexOf(wasSelected))
        case k =>
          val selectedItem = menuEntries(selected)
          commands(selectedItem).find(c => c.available && c.key == k) foreach { _.execute() }
      }
    }
  }

  override def render(renderer: GlyphRenderer): Unit = {
    import GlyphRenderer.{Ann, ColoredString, wrapCS}
    val width = 24
    val Some((sx, sy)) = display.worldToScreen(state)(location.xy)
    val (char, fg, bg) = Appearance.charAtPosition(state, location)

    renderer.drawChar(sx, sy, char, fg, bg = selectedGreen)
    renderer.drawChar(sx + 1, sy, BoxDrawing.L_R_, fg = lightGreen)
    var nextY = 0
    def sprintln(s: String, fg: Color = lightGreen, bg: Color = darkGreen, wrap: Boolean = false): Unit = {
      if (wrap) {
        for (line <- GlyphRenderer.wrap(s, width - 2))
          sprintln(line, fg = fg, bg = bg, wrap = false)
      } else {
        sprintlnColored(ColoredString(s, Seq(Ann(0, s.length, fg))), bg = bg)
      }
    }

    def sprintlnColored(ss: ColoredString, defaultFg: Color = lightGreen, bg: Color = darkGreen): Unit = {
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
      sprintln(prefix + it.kind.name)
      if (state.isKnownToBeNonFunctional(it))
        renderer.drawChar(sx + width, sy + nextY - 1, '!', fg = red, bg = darkGreen)
    }
    val entries = menuEntries
    for (y <- entries.indices) {
      val bg = if (y == selected) selectedGreen else darkGreen
      val prefix = if (openStack.isEmpty) "" else " " * (openStack.size - 1) + (if (y == entries.size - 1) "\u00c0" else "\u00c3")
      val entry = entries(y)
      sprintln(prefix + entry.text, bg = bg, fg = if (entry.isInstanceOf[ItemEntry]) lightGreen else disabledGreen)
      entry match {
        case ItemEntry(item) =>
          if (state.isKnownToBeNonFunctional(item))
            renderer.drawChar(sx + width, sy + nextY - 1, '!', fg = red, bg = bg)
          else if (state.sendMessage(item, Message.IsDiagnosable()).diagnosable)
            renderer.drawChar(sx + width, sy + nextY - 1, '?', fg = disabledGreen, bg = bg)
        case _ =>
      }
    }
    sprintln("")
    val entry = entries(selected)
    entry match {
      case ItemEntry(item) =>
        val conditions = state.visibleConditions(item)
        for (condition <- conditions) sprintln(condition, fg = red)
        if (conditions.nonEmpty) sprintln("")
        sprintln(item.kind.description, fg = disabledGreen, wrap = true)
      case MissingItemEntry(kind, count) =>
        count match {
          case 1 =>
            sprintln("This part is missing.", fg = disabledGreen, wrap = true)
          case n =>
            sprintln(s"$n of these are missing.", fg = disabledGreen, wrap = true)
        }
    }
    val actionCS = commands(entry).map(_.display)
    if (actionCS.nonEmpty) {
      sprintln("")
      val actionStr = wrapCS(actionCS.reduce(_ + ColoredString(" ", Seq.empty) + _), width - 2)
      actionStr.foreach(sprintlnColored(_, defaultFg = disabledGreen))
    }
  }
}
