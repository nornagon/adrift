package adrift

import adrift.Action._
import adrift.items.{Item, ItemKind}

import scala.collection.mutable

sealed trait Terrain {
  def walkable: Boolean = true
}
case object EmptySpace extends Terrain {
  override val walkable = false
}
case object Floor extends Terrain
case object GlassWall extends Terrain {
  override val walkable = false
}
case object TreeOak extends Terrain {
  override val walkable = false
}
case object TreeFicus extends Terrain {
  override val walkable = false
}
case object Grass extends Terrain

/*
Items have some generic type (a battery, a screwdriver, a tomato) and some specific state (half charged, found in the
elevator, moldy).

Items are made out of other specific items, which in turn have their own generic type and specific state.

The kinds of things in an item's specific state differ based on the type. For example, it doesn't make sense for a
tomato to be half-charged, or for a battery to be moldy (unless it was some kind of bio-battery...).

Some items have one or more "functions", i.e. things you can do with them. They could be either direct functions (turn
on the heater, listen to the recording, saw the log in half) or indirect functions (use the plasma torch in crafting,
wear the haz-mat suit, provide light).

For items with a function, all the parts of that item must recursively be functional for the item itself to be
functional. If an item is comprised of other items, it is functional iff all its components are functional. An item
can't be made out of functional things and yet itself be non-functional.

Items can be damaged in a variety of ways. Some damage is trivially easy to repair (e.g. recharge a battery), while
other kinds of damage require specialty tools just to diagnose, let alone repair.

=> so not only do items have specific statuses, the player might also have a varying amount of information about an
item's status. Do they know that the turbolift isn't working because one of the power relays has a broken fuse? Or can
they just tell that the power supply seems to be out of order? (Or just that the buttons don't seem to do anything?)


*****

Where can items be?
- on the ground
- in your hands
- in a container on the ground
- worn on your body
- in a container worn on your body
- inside your body (implants)
- part of another item

What can you do with items?
Simple:
- pick up
- put down
- store in container
Less simple:
- eat
- drink
- disassemble
- assemble
- wear

 */



class GameState(width: Int, height: Int) {
  val map: Grid[Terrain] = new Grid[Terrain](width, height)(EmptySpace)
  val items: Grid[Seq[Item]] = new Grid[Seq[Item]](width, height)(Seq.empty)
  var player: (Int, Int) = (0, 0)
  val inventory: mutable.Buffer[Item] = mutable.Buffer.empty
  def receive(action: Action): Unit = {
    action match {
      case PlayerMove(dx, dy) =>
        if (map(player._1 + dx, player._2 + dy).walkable)
          player = (player._1 + dx, player._2 + dy)
      case Quit =>
    }
  }
}

object GameState {
  def generateWorld: GameState = {
    val state = new GameState(128, 128)
    val random = new scala.util.Random(42)
    filledCircle(64, 64, r = 50) { (x, y) => state.map(x, y) = if (random.nextFloat() < 0.1) Grass else Floor }
    hollowCircle(64, 64, r = 50) { (x, y) => state.map(x, y) = GlassWall }
    for ((x, y) <- state.map.indices) {
      if (state.map(x, y) == Grass && random.nextFloat() < 1/6f)
        state.map(x, y) = if (random.nextFloat() < 0.3) TreeFicus else TreeOak
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


  def hollowCircle(x0: Int, y0: Int, r: Int)(putpixel: (Int, Int) => Unit): Unit = {
    var x: Int = r-1
    var y: Int = 0
    var dx: Int = 1
    var dy: Int = 1
    var err: Int = dx - (r << 1)
    while (x >= y) {
      putpixel(x0 + x, y0 + y)
      putpixel(x0 + y, y0 + x)
      putpixel(x0 - y, y0 + x)
      putpixel(x0 - x, y0 + y)
      putpixel(x0 - x, y0 - y)
      putpixel(x0 - y, y0 - x)
      putpixel(x0 + y, y0 - x)
      putpixel(x0 + x, y0 - y)

      if (err <= 0) {
        y += 1
        err += dy
        dy += 2
      }
      if (err > 0) {
        x -= 1
        dx += 2
        err += (-r << 1) + dx
      }
    }
  }
  def filledCircle(x0: Int, y0: Int, r: Int)(putpixel: (Int, Int) => Unit): Unit = {
    var x: Int = r-1
    var y: Int = 0
    var dx: Int = 1
    var dy: Int = 1
    var err: Int = dx - (r << 1)
    var lastY = -1
    while (x >= y) {
      if (y != lastY) {
        for (xi <- x0 - x to x0 + x) putpixel(xi, y0 + y)
        for (xi <- x0 - y to x0 + y) putpixel(xi, y0 + x)
        for (xi <- x0 - x to x0 + x) putpixel(xi, y0 - y)
        for (xi <- x0 - y to x0 + y) putpixel(xi, y0 - x)
      }
      lastY = y

      if (err <= 0) {
        y += 1
        err += dy
        dy += 2
      }
      if (err > 0) {
        x -= 1
        dx += 2
        err += (-r << 1) + dx
      }
    }
  }
}