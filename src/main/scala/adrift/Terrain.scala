package adrift

sealed trait Terrain {
  def walkable: Boolean = true
  def opaque: Boolean = false
}

object Terrain {
  case object EmptySpace extends Terrain {
    override val walkable = false
  }

  case object Floor extends Terrain

  case object Wall extends Terrain {
    override val walkable = false
    override val opaque = true
  }

  case object GlassWall extends Terrain {
    override val walkable = false
  }

  case object TreeOak extends Terrain {
    override val walkable = false
    override val opaque = true
  }

  case object TreeFicus extends Terrain {
    override val walkable = false
    override val opaque = true
  }

  case object Grass extends Terrain
}
