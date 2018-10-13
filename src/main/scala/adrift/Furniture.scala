package adrift

sealed trait Furniture {
  def walkable: Boolean = true
  def opaque: Boolean = false
}

object Furniture {
  case object DoorClosed extends Furniture {
    override val walkable = false
    override val opaque = true
  }

  case object DoorOpen extends Furniture {
    override val walkable = true
  }

  case object Desk extends Furniture {
    override val walkable = true
  }
}
