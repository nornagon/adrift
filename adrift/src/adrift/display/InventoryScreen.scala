package adrift.display

import adrift.display.GlyphRenderer.{ColoredString, wrapCS}
import adrift.items.Message.CanWear
import adrift.items.{Item, Message}
import adrift.{Action, GameState, InHands, Inside, OnFloor, Rect, Worn}
import org.lwjgl.glfw.GLFW._

class InventoryScreen(display: GLFWDisplay, state: GameState) extends Screen {
  import UI.Color._

  case class Command(name: String, execute: () => Unit, available: Boolean = true) {
    private val pat = raw"(.*)\{(.)}(.*)".r
    private def color = if (available) disabledGreen else disabledRed
    private def highlightColor = if (available) lightGreen else red
    val (char, display) = name match {
      case pat(pre, char, post) =>
        (char, ColoredString(pre).withFg(color) + ColoredString(char).withFg(highlightColor) + ColoredString(post).withFg(color))
    }
    val key: Int = GLFW_KEY_A + char.charAt(0) - 'a'
  }

  private def commands(item: Item): Seq[Command] = {
    import Option.when
    val location = state.items.lookup(item)
    Seq(
      when(
        (location match { case Inside(_) | Worn() | OnFloor(_) => true ; case _ => false }) &&
        state.sendMessage(item, Message.CanPickUp()).ok)
      (Command("{g}et", () => doGet(item))),

      when(location match { case Inside(_) | InHands() | Worn() => true ; case _ => false })
      (Command("{d}rop", () => doDrop(item))),

      Some(Command("{m}ark", () => doToggleMark(item))),

      when(marked.nonEmpty && state.sendMessage(item, Message.CanContainItems()).ok)
      (Command("{i}nsert", () => doInsert(item), available = !marked(item))),

      when(state.sendMessage(item, CanWear()).ok)
      (Command("{w}ear", () => doWear(item))),
    ).flatten
  }

  private def doDrop(item: Item): Unit =
    state.receive(Action.MoveItem(item, OnFloor(state.player)))
  private def doGet(item: Item): Unit =
    state.receive(Action.PickUp(item))
  private def doToggleMark(item: Item): Unit =
    if (marked(item))
      marked -= item
    else
      marked += item
  private def doInsert(container: Item): Unit =
    marked foreach { item =>
      state.receive(Action.MoveItem(item, Inside(container)))
      if (state.items.lookup(item) == Inside(container))
        marked -= item
    }
  private def doWear(item: Item): Unit =
    state.receive(Action.Wear(item))

  override def key(key: Int, scancode: Int, action: Int, mods: Int): Unit = {
    (action, key) match {
      case (GLFW_PRESS | GLFW_REPEAT, DirectionKey(dx, dy)) =>
        moveCursor(dy)
      case (GLFW_PRESS, k) =>
        selectedItem foreach { item =>
          commands(item).find(c => c.available && c.key == k) foreach { _.execute() }
        }
      case _ =>
    }
  }

  var cursor = 0
  var marked = Set.empty[Item]

  val sections = Seq(
    "HELD" -> InHands(),
    "WORN" -> Worn(),
    "NEARBY" -> OnFloor(state.player),
  )
  def contents(item: Item): Seq[Item] = state.items.lookup(Inside(item)).flatMap(i => i +: contents(i))
  def items: Seq[Seq[Item]] = sections.map(s => state.items.lookup(s._2).flatMap(i => i +: contents(i)))
  def moveCursor(d: Int): Unit = cursor = math.max(0, math.min(cursor + d, items.map(_.size).sum - 1))
  def selectedItem: Option[Item] = {
    val is = items.flatten
    if (cursor >= 0 && cursor < is.size) Some(is(cursor))
    else None
  }
  def countContainers(item: Item): Int =
    state.items.lookup(item) match {
      case Inside(other) => 1 + countContainers(other)
      case _ => 0
    }

  override def render(renderer: GlyphRenderer): Unit = {
    moveCursor(0) // if the item list has changed, this pushes the cursor back in range
    val bounds = Rect.centeredAt(renderer.bounds.center, 40, 40)

    import layout3._

    val actions = selectedItem map { commands }
    val actionGuide = actions match {
      case Some(actions) =>
        Seq(
          ConstrainedBox(BoxConstraints(minHeight = 1)),
          Text(actions.map(_.display).reduce(_ + " " + _))
        )
      case None =>
        Seq.empty
    }

    val w = LRBorder(
      fg = lightGreen, bg = darkGreen,
      content = Background(bg = darkGreen, content = Column(
        sections.zip(items).flatMap {
          case ((title, location), items) =>
            Seq(Text(title.withFg(disabledGreen))) ++ items.map { item =>
              val indentation = countContainers(item)
              val fg =
                if (selectedItem.contains(item)) selectedGreen
                else if (marked(item)) markedBlue
                else lightGreen
              Text((" " * indentation + state.itemDisplayName(item)).withFg(fg))
            }
        } ++ (selectedItem match {
          case Some(sel) =>
            Seq(
              ConstrainedBox(BoxConstraints(minHeight = 1)),
              Text(sel.kind.description.withFg(disabledGreen))
            )
          case None => Seq.empty
        }) ++ actionGuide
      ))
    )
    val r = w.inflate.asInstanceOf[RenderBox]
    r.layout(BoxConstraints(minWidth = bounds.width, maxWidth = bounds.width, maxHeight = bounds.height))
    r.paint(renderer, Offset(bounds.l, bounds.t))
  }
}
