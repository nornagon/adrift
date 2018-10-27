package adrift.items

trait Message

object Message {
  case class PlayerMove(x: Int, y: Int) extends Message
  case object Activate extends Message
  case object Deactivate extends Message
  case object Tick extends Message
  case class IsOpaque(var opaque: Boolean = false) extends Message
  case class IsWalkable(var walkable: Boolean = true) extends Message
  case class PickUp(var ok: Boolean = true) extends Message
}
