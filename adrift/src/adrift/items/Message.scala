package adrift.items

trait Message

object Message {
  case class PlayerMove(x: Int, y: Int) extends Message
  case object Activate extends Message
  case object Deactivate extends Message
  case object Tick extends Message
  case class IsOpaque(var opaque: Boolean = false) extends Message
  case class IsWalkable(var walkable: Boolean = true) extends Message
  case class IsPermeable(var permeable: Boolean = true) extends Message
  case class PickUp(var ok: Boolean = true) extends Message
  case class Display(var display: String) extends Message

  case class VisibleConditions(var conditions: Seq[String] = Seq.empty) extends Message

  case class UseTool(op: ItemOperation, var ok: Boolean = false) extends Message
  case class ToolUsed(op: ItemOperation) extends Message

  case class IsFunctional(var functional: Boolean = true) extends Message

  case class DrawCharge(amount: Int, var ok: Boolean = true) extends Message
  case class ChargeAvailable(amount: Int, var ok: Boolean = false) extends Message

  case class PlugInto(item: Item) extends Message
  case class Unplugged() extends Message
  case class CanReceivePlug(plugShape: String, var ok: Boolean = false) extends Message
  case class CanPlugInto(item: Item, var ok: Boolean = false) extends Message

  case class Hauled(from: (Int, Int), to: (Int, Int)) extends Message

  case class PickedUp(var item: Item) extends Message
  case class Dropped() extends Message

  case class CanWear(var ok: Boolean = false) extends Message
  case class LoseHeat(var dq: Double) extends Message
}
