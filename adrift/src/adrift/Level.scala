package adrift

import scala.util.Random

object Level {
  def emptyCylinder(data: Data, width: Int, height: Int)(implicit random: Random): Level =
    Level(
      terrain = new CylinderGrid(width, height)(data.terrain("floor")),
      powerCables = new CylinderGrid(width, height)(0),
      dataCables = new CylinderGrid(width, height)(0),
      fluidCables = new CylinderGrid(width, height)(0),
      atmosphere = {
        val atmo = new Array[Float](width * height * 4)
        val gc = GasComposition.earthLike
        for (y <- 0 until height; x <- 0 until width; i = (y * width + x) * 4) {
          atmo(i) = random.between(250f, 270f)
          atmo(i + 1) = gc.oxygen
          atmo(i + 2) = gc.carbonDioxide
          atmo(i + 3) = gc.nitrogen
        }
        atmo
      }
    )

  def emptySquare(data: Data, width: Int, height: Int)(implicit random: Random): Level =
    Level(
      terrain = new Grid(width, height)(data.terrain("floor")),
      powerCables = new Grid(width, height)(0),
      dataCables = new Grid(width, height)(0),
      fluidCables = new Grid(width, height)(0),
      atmosphere = {
        val atmo = new Array[Float](width * height * 4)
        val gc = GasComposition.earthLike
        for (y <- 0 until height; x <- 0 until width; i = (y * width + x) * 4) {
          atmo(i) = random.between(250f, 270f)
          atmo(i + 1) = gc.oxygen
          atmo(i + 2) = gc.carbonDioxide
          atmo(i + 3) = gc.nitrogen
        }
        atmo
      }
    )
}

case class Level(
  var terrain: Grid[Terrain],
  var powerCables: Grid[Int],
  var dataCables: Grid[Int],
  var fluidCables: Grid[Int],
  var atmosphere: Array[Float],
) {
  val width: Int = terrain.width
  val height: Int = terrain.height
  assert(atmosphere.size == width * height * 4)

  def temperature(xy: (Int, Int)): Float = temperature(xy._1, xy._2)

  def temperature(x: Int, y: Int): Float = atmosphere((y * width + terrain.normalizeX(x)) * 4)

  def setTemperature(x: Int, y: Int, v: Float): Unit = atmosphere((y * width + terrain.normalizeX(x)) * 4) = v

  def gasComposition(xy: (Int, Int)): GasComposition = gasComposition(xy._1, xy._2)

  def gasComposition(x: Int, y: Int): GasComposition = {
    val nx = terrain.normalizeX(x)
    if (!terrain.contains(nx, y))
      return GasComposition(0, 0, 0)
    val i = (y * width + nx) * 4
    GasComposition(atmosphere(i + 1), atmosphere(i + 2), atmosphere(i + 3))
  }

  def setGasComposition(x: Int, y: Int, v: GasComposition): Unit = {
    val nx = terrain.normalizeX(x)
    val i = (y * width + nx) * 4
    atmosphere(i + 1) = v.oxygen
    atmosphere(i + 2) = v.carbonDioxide
    atmosphere(i + 3) = v.nitrogen
  }

  private val atmoSim = new AtmoSim(width, height)

  def updateAtmoSimStatics(isPermeable: (Int, Int) => Boolean): Unit = {
    val grid = new Grid[Float](width, height)(0)
    for ((x, y) <- grid.indices) {
      grid(x, y) = terrain(x, y).heatTransfer
    }
    atmoSim.updateTransferTexture(grid, (x, y) => if (isPermeable(x, y)) 1 else 0)
    for ((x, y) <- grid.indices) {
      grid(x, y) = terrain(x, y).heatCapacity
    }
    atmoSim.updateHeatCapacityTexture(grid)
  }

  def updateAtmosphere(dt: Double = 1)(implicit random: Random): Unit = {
    atmoSim.step(atmosphere, cylindrical = terrain.isInstanceOf[CylinderGrid[_]])
  }
}