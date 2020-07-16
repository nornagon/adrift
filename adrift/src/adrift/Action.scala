package adrift
import adrift.items.{Item, ItemKind, ItemOperation}
sealed trait Action

object Action {
  case class PlayerMove(dx: Int, dy: Int) extends Action
  case class AssemblyAction(tool: Item, part: Item, operation: ItemOperation)
  case class Assemble(item: ItemKind, operations: Seq[AssemblyAction]) extends Action
  case class PickUp(item: Item) extends Action
  case class PutDown(item: Item) extends Action
  case class Plug(item: Item, into: Item) extends Action
  case class Wear(item: Item) extends Action
  case class TakeOff(item: Item) extends Action
  case class Wait(durationSec: Int) extends Action
  case class Eat(item: Item) extends Action

  case object Quit extends Action
  case class ReloadData(data: Data) extends Action

  case object Regenerate extends Action

  case class Remove(parents: Seq[Item], item: Item) extends Action

  case class Diagnose(item: Item) extends Action

  case class Install(parent: Item, part: Item) extends Action

}
