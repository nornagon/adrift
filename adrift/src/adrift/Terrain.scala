package adrift

case class Terrain(
  name: String,
  walkable: Boolean,
  opaque: Boolean,
  permeable: Boolean = true,
  connects: Boolean = false,
  display: String,
  heatCapacity: Double = 1d,
  heatTransfer: Double = 1d,
) {
  require(0 <= heatTransfer && heatTransfer <= 1, s"heatTransfer must be between 0 and 1")
}
