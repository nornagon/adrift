package adrift

case class Terrain(
  name: String,
  walkable: Boolean,
  opaque: Boolean,
  permeable: Boolean = true,
  connects: Boolean = false,
  display: String,
  heatCapacity: Float = 1f,
  heatTransfer: Float = 1f,
) {
  require(0 <= heatTransfer && heatTransfer <= 1, s"heatTransfer must be between 0 and 1")
}
