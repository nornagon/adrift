package adrift.display.screens

import adrift.*
import adrift.display.*
import adrift.display.CP437.BoxDrawing
import adrift.display.GlyphRenderer.ColoredString
import adrift.display.layout.AnnotatedColorString
import adrift.items.*
import adrift.items.Message.Provides
import org.lwjgl.glfw.GLFW.*

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

  // Also admits '.' to specify 'here'.
  object WithHere {
    def unapply(key: Int): Option[(Int, Int)] =
      key match {
        case DirectionKey((dx, dy)) => Some((dx, dy))
        case GLFW_KEY_PERIOD => Some((0, 0))
        case _ => None
      }
  }
}

class ExamineDirectionScreen(display: GLFWDisplay, state: GameState) extends Screen {
  override def key(key: Int, scancode: Int, action: Int, mods: Int): Unit = {
    if (action == GLFW_PRESS) {
      key match {
        case DirectionKey.WithHere((dx, dy)) =>
          display.popScreen()
          val location = state.player + (dx, dy)
          if (state.items.lookup(OnFloor(location)).nonEmpty)
            display.pushScreen(new ExamineScreen(display, state, location))
        case _ =>
      }
    }
  }

  override def render(renderer: GlyphRenderer): Unit = {
    renderer.frame(renderer.bounds.center._1 - 8, 0, 16, null, halfWidth = false, Seq("Examine where?"))
  }
}

object UI {
  object Color {
    import adrift.Color.fromBytes
    val lightGreen: Color = fromBytes(217, 255, 102)
    val disabledGreen: Color = fromBytes(77, 102, 0)
    val selectedGreen: Color = fromBytes(0, 140, 0)
    val darkGreen: Color = fromBytes(32, 64, 0)
    val red: Color = fromBytes(255, 51, 51)
    val disabledRed: Color = fromBytes(102, 0, 0)
    val markedBlue: Color = fromBytes(51, 153, 255)
  }
}

class ExamineScreen(display: GLFWDisplay, state: GameState, location: Location) extends Screen {
  import UI.Color.*
  var selected: Int = 0
  private var openStack: Seq[Item] = Seq.empty
  private def items = openStack.lastOption.map(_.parts).getOrElse(state.items.lookup(OnFloor(location)))
  private def missingParts = openStack.lastOption.map(getMissingParts).getOrElse(Seq.empty)
  private def getMissingParts(item: Item) = {
    val partCountByKind = item.parts.groupBy(_.kind).view.mapValues(_.size)
    for {
      ip <- item.kind.parts
      presentCount = partCountByKind.getOrElse(ip.kind, 0)
      if presentCount < ip.count
    } yield (ip.kind, ip.attachment, ip.count - presentCount)
  }

  sealed trait MenuEntry {
    def text: String
  }
  case class ItemEntry(item: Item) extends MenuEntry {
    override def text: String = item.kind.name
  }
  case class MissingItemEntry(kind: ItemKind, attachment: Option[ItemAttachment], count: Int) extends MenuEntry {
    override def text: String = s"${kind.name}${if (count > 1) s" x $count" else ""}"
  }

  private def menuEntries: Seq[MenuEntry] =
    items.map(ItemEntry.apply) ++
      missingParts.map { case (k, a, c) => MissingItemEntry(k, a, c) }

  case class Command(name: String, execute: () => Unit, available: Boolean = true) {
    private val pat = raw"(.*)\{(.)}(.*)".r
    private def color = if (available) disabledGreen else disabledRed
    private def highlightColor = if (available) lightGreen else red
    val (char, display) = name match {
      case pat(pre, char, post) =>
        (char, pre.withFg(color) + char.withFg(highlightColor) + post.withFg(color))
    }
    val key: Int = GLFW_KEY_A + char.charAt(0) - 'a'
  }

  def missingRemoveOp(parent: Item, item: Item): Option[ItemOperation] = {
    val attachment = parent.kind.parts.find(_.kind == item.kind).get.attachment
    val disassemblyOp = attachment.map(_.disassembly)
    disassemblyOp.flatMap { op =>
      if (state.nearbyItems.exists(i => state.sendMessage(i, Provides(op)).provides))
        None
      else
        Some(op)
    }
  }

  private def commands(menuEntry: MenuEntry): Seq[Command] = {
    import Option.when
    menuEntry match {
      case ItemEntry(item) =>
        val Message.IsDiagnosable(diagnosable, diagnoseOp) = state.sendMessage(item, Message.IsDiagnosable())
        val opAvailable = diagnosable && state.toolsProviding(diagnoseOp.get).nonEmpty
        Seq(
          when(openStack.isEmpty && state.sendMessage(item, Message.CanPickUp()).ok)
          (Command("{g}et", () => doGet(item))),

          when(item.parts.nonEmpty)
          (Command("{o}pen", () => doOpen(item))),

          when(diagnosable)
          (Command("{d}iagnose", () => doDiagnose(item), available = opAvailable)),

          when(openStack.nonEmpty)
          (Command("{r}emove", () => doRemove(openStack, item), available = missingRemoveOp(openStack.last, item).isEmpty)),
        ).flatten
      case MissingItemEntry(kind, attachment, count) =>
        val partAvailable = state.nearbyItems.find(_.kind == kind)
        val installable = attachment.isEmpty || attachment.exists(a => state.toolsProviding(a.assembly).nonEmpty)
        Seq(
          Some(Command("{i}nstall", () => doInstall(openStack.last, partAvailable.get), available = partAvailable.nonEmpty && installable))
        ).flatten
    }
  }

  private def doGet(item: Item): Unit = {
    state.receive(Action.PickUp(item))
    if (menuEntries.isEmpty) display.popScreen()
    selected = math.min(selected, menuEntries.size - 1)
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
    while ({
      selected = math.min(selected, ps.last.parts.size - 1)
      ps = ps.init
      selected < 0 && ps.nonEmpty
    }) {}
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
        case GLFW_KEY_GRAVE_ACCENT =>
          val selectedItem = menuEntries(selected)
          selectedItem match {
            case ItemEntry(item) =>
              val json = Serialization.encodeItem(item)
              println(io.circe.yaml.printer.print(json))
            case MissingItemEntry(kind, attachment, count) =>
              println(s"Missing $kind $attachment $count")
          }
        case k =>
          val selectedItem = menuEntries(selected)
          commands(selectedItem).find(c => c.available && c.key == k) foreach { _.execute() }
      }
    }
  }

  override def render(renderer: GlyphRenderer): Unit = {
    import layout.*

    val width = 24
    val Some((sx, sy)) = display.worldToScreen(state)(location.xy)
    val (char, fg, bg) = Appearance.charAtPosition(state, location)

    renderer.drawChar(sx, sy, char, fg, bg = selectedGreen)
    renderer.drawChar(sx + 1, sy, BoxDrawing.L_R_, fg = lightGreen)

    val w = Column({
      openStack.zipWithIndex.map { case (it, idx) =>
        Row(Seq(
          Flexible(
            Row(Seq(
              Text(if (idx == 0) "" else " " * (idx - 1) + "\u00c0", halfWidth = false),
              Flexible(Text(it.kind.name))
            ))
          ),
          Text(if (state.isKnownToBeNonFunctional(it)) "!".withFg(red) else "", halfWidth = false)
        ))
      } ++ {
        val entries = menuEntries
        val entry = entries(selected)
        (for (y <- entries.indices) yield {
          val bg = if (y == selected) selectedGreen else darkGreen
          val prefix = if (openStack.isEmpty) "" else " " * (openStack.size - 1) + (if (y == entries.size - 1) "\u00c0" else "\u00c3")
          val entry = entries(y)
          Row(Seq(
            Flexible(
              Row(Seq(
                Text(prefix, halfWidth = false),
                Flexible(Text(
                  entry.text.withFg(if (entry.isInstanceOf[ItemEntry]) lightGreen else disabledGreen)
                ))
              ))
            ),
            Text(entry match {
              case ItemEntry(item) =>
                if (state.isKnownToBeNonFunctional(item))
                  "!".withFg(red)
                else if (state.sendMessage(item, Message.IsDiagnosable()).diagnosable)
                  "?".withFg(disabledGreen)
                else
                  ""
              case _ => ""
            }, halfWidth = false)
          )).background(bg)
        }) ++ {
          entry match {
            case ItemEntry(item) =>
              (state.visibleConditions(item) match {
                case Seq() => Seq.empty
                case conditions =>
                  ConstrainedBox(BoxConstraints(minHeight = 1)) +:
                    conditions.map(c => Text(c.withFg(red)))
              }) ++ Seq(
                ConstrainedBox(BoxConstraints(minHeight = 1)),
                Text(item.kind.description.withFg(disabledGreen))
              ) ++ (state.sendMessage(item, Message.DescriptiveTraits()).traits match {
                case Seq() => Seq.empty
                case traits =>
                  ConstrainedBox(BoxConstraints(minHeight = 1)) +:
                    traits.map(c => Text(c.withFg(lightGreen)))
              }) ++ {
                val Message.IsDiagnosable(diagnosable, diagnoseOp) = state.sendMessage(item, Message.IsDiagnosable())
                val Message.IsDiagnosed(diagnosed) = state.sendMessage(item, Message.IsDiagnosed())
                if (diagnosable && !diagnosed) {
                  Seq(
                    ConstrainedBox(BoxConstraints(minHeight = 1)),
                    Text("You could diagnose this with ".withFg(lightGreen) + diagnoseOp.map(_.id).getOrElse("???").withFg(red) + ".".withFg(lightGreen))
                  )
                } else Seq.empty
              } ++ (
                for (parent <- openStack.lastOption; missingOp <- missingRemoveOp(parent, item)) yield {
                  Seq(
                    ConstrainedBox(BoxConstraints(minHeight = 1)),
                    Text(s"Requires ${missingOp.id} to remove.".withFg(red))
                  )
                }
              ).getOrElse(Seq.empty)
            case MissingItemEntry(kind, attachment, count) =>
              Seq(
                ConstrainedBox(BoxConstraints(minHeight = 1)),
                Text(
                  (count match {
                    case 1 => "This part is missing."
                    case n => s"$n of these are missing."
                  }).withFg(disabledGreen)
                )
              ) ++ (
                if (attachment.nonEmpty && state.toolsProviding(attachment.get.assembly).isEmpty)
                  Seq(
                    ConstrainedBox(BoxConstraints(minHeight = 1)),
                    Text(s"Requires ${attachment.get.assembly.id} to install.".withFg(red))
                  )
                else Seq.empty
              )
          }
        } ++ {
          val actionCS = commands(entry).map(_.display)
          if (actionCS.nonEmpty) {
            Seq(
              ConstrainedBox(BoxConstraints(minHeight = 1)),
              Text(actionCS.reduce(_ + " " + _))
            )
          } else Seq.empty
        }
      }
    })
      .background(darkGreen)
      .lrBorder(fg = lightGreen, bg = darkGreen)
    val r = w.inflate.asInstanceOf[RenderBox]
    r.layout(BoxConstraints(minWidth = width, maxWidth = width))
    r.paint(renderer, Offset(sx + 2, sy))
  }
}
