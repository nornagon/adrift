package adrift

sealed trait Terrain {
  def walkable: Boolean = true
}

object Terrain {
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
}
