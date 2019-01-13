package adrift

case class Terrain(
  name: String,
  walkable: Boolean,
  opaque: Boolean,
  connects: Boolean = false,
  display: String,
  heatCapacity: Double = 1d,
)
