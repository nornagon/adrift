package adrift

import adrift.Action._
import adrift.items.{Item, ItemKind}

import scala.collection.mutable

sealed trait ItemLocation
case class OnFloor(x: Int, y: Int/* TODO: level? */, index: Int) extends ItemLocation
case class InHands(index: Int) extends ItemLocation


case class Container(maxItems: Int, contents: mutable.Buffer[Item] = mutable.Buffer.empty)


class GameState(width: Int, height: Int) {
  val map: Grid[Terrain] = new Grid[Terrain](width, height)(Terrain.EmptySpace)
  val items: Grid[Seq[Item]] = new Grid[Seq[Item]](width, height)(Seq.empty)
  var player: (Int, Int) = (0, 0)
  val hands = Container(maxItems = 2)

  def receive(action: Action): Unit = {
    action match {
      case PlayerMove(dx, dy) =>
        if (map(player._1 + dx, player._2 + dy).walkable)
          player = (player._1 + dx, player._2 + dy)
      case Disassemble(location) =>
        val item = removeItem(location)
        items(player) ++= item.parts
      case PickUp(location) =>
        if (hands.contents.size < hands.maxItems) {
          val item = removeItem(location)
          hands.contents.append(item)
        }
      case PutDown(location) =>
        location match {
          case loc: InHands =>
            val item = removeItem(loc)
            items(player) :+= item
          case _ =>
            // Can't put that down.
        }
      case Quit =>
    }
  }

  def itemAtLocation(location: ItemLocation): Item = location match {
    case OnFloor(x, y, index) =>
      items(x, y)(index)
    case InHands(index) =>
      hands.contents(index)
  }

  def removeItem(location: ItemLocation): Item = location match {
    case OnFloor(x, y, index) =>
      val item = items(x, y)(index)
      items(x, y) = items(x, y).patch(index, Nil, 1)
      item
    case InHands(index) =>
      val item = hands.contents(index)
      hands.contents.remove(index)
      item
  }
}

object GameState {
  def generateWorld: GameState = {
    val state = new GameState(128, 128)
    val random = new scala.util.Random(42)
    GridUtils.filledCircle(64, 64, r = 50) { (x, y) =>
      state.map(x, y) = if (random.nextFloat() < 0.1) Terrain.Grass else Terrain.Floor
    }
    GridUtils.hollowCircle(64, 64, r = 50) { (x, y) => state.map(x, y) = Terrain.GlassWall }
    for ((x, y) <- state.map.indices) {
      if (state.map(x, y) == Terrain.Grass && random.nextFloat() < 1/6f)
        state.map(x, y) = if (random.nextFloat() < 0.3) Terrain.TreeFicus else Terrain.TreeOak
      if (state.map(x, y).walkable) {
        if (random.nextFloat() < 1/512f) {
          state.items(x, y) :+= generateItem(items.HoloNote)
        }
      }
    }
    state.player = (64, 32)
    state
  }

  def generateItem(itemKind: ItemKind): Item = {
    Item(itemKind, Seq.empty, itemKind.parts.flatMap { case (part, count) => Seq.fill(count)(generateItem(part)) })
  }
}