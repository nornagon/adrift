package adrift
import adrift.items.{Item, ItemKind}
sealed trait Action {

}

object Action {
  case class PlayerMove(dx: Int, dy: Int) extends Action
  case class Disassemble(item: Item) extends Action
  case class Assemble(item: ItemKind, components: Seq[Item]) extends Action
  case class PickUp(item: Item) extends Action
  case class PutDown(item: Item) extends Action
  case class Plug(item: Item, into: Item) extends Action
  case class Wear(item: Item) extends Action
  case class TakeOff(item: Item) extends Action
  case class Wait() extends Action
  case object Quit extends Action
}
