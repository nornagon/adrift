package adrift.worldgen

class ForceDirectedPlacement(
  numNodes: Int,
  neighbors: Int => IterableOnce[Int],
  desiredEdgeLength: (Int, Int) => Double,
  initialPosition: Int => (Double, Double),
  isFixedPosition: Int => Boolean = _ => false,
  diff: ((Double, Double), (Double, Double)) => (Double, Double) = (a, b) => (a._1 - b._1, a._2 - b._2),
) {
  private val neighborCache = Array.tabulate(numNodes) { u => neighbors(u).iterator.toArray }
  private val links = Array.tabulate(numNodes) { u => neighborCache(u).filter(_ > u) }
  private val (px, py) = Array.tabulate(numNodes)(initialPosition).unzip
  private val (vx, vy) = Array.fill(numNodes)((0d, 0d)).unzip

  def defaultStrength(u: Int, v: Int): Double =
    1d / math.min(neighborCache(u).length, neighborCache(v).length)

  def bias(u: Int, v: Int): Double =
    neighborCache(u).length.toDouble / (neighborCache(u).length + neighborCache(v).length)

  val velocityDecay = 0.6 // default from d3

  def position(i: Int): (Double, Double) = (px(i), py(i))
  def velocity(i: Int): (Double, Double) = (vx(i), vy(i))

  def positions: Seq[(Double, Double)] =
    (0 until numNodes).map(i => (px(i), py(i)))

  def alphaSequence(initialAlpha: Double = 1, alphaMin: Double = 0.001, alphaTarget: Double = 0): Iterator[Double] = {
    val alphaDecay = 1 - math.pow(alphaMin, 1 / 300d)
    Iterator.iterate(initialAlpha)(a => a + (alphaTarget - a) * alphaDecay).takeWhile(_ >= alphaMin)
  }

  def run(initialAlpha: Double = 1, alphaMin: Double = 0.001, alphaTarget: Double = 0, dt: Double = 1): Unit = {
    alphaSequence(initialAlpha, alphaMin, alphaTarget) foreach { alpha =>
      step(alpha, dt)
    }
  }

  def step(alpha: Double, dt: Double = 1): Unit = {
    // 1. apply forces (i.e. accelerations) to v
    // 1a. repulsion (~ 1/r)
    for (u <- 0 until numNodes; v <- (u + 1) until numNodes) {
      val (dx, dy) = diff(position(v), position(u))
      // TODO: jiggle to avoid glitches when coincident
      val l2 = dx * dx + dy * dy
      val strength = -30 // default from d3
      val w = strength * alpha / l2 * dt
      //   s*a / |d|^2 * d
      // = s*a / |d| * normalize(d)
      vx(u) += dx * w
      vy(u) += dy * w
      vx(v) -= dx * w
      vy(v) -= dy * w
    }

    // 1b. attraction (~ -r)
    for (u <- 0 until numNodes; v <- links(u)) {
      // NB. neighborCache contains each link only once, so we apply force on both ends
      val l0 = desiredEdgeLength(u, v)
      val strength = defaultStrength(u, v)

      val (dx, dy) = diff(position(v), position(u))
      val l = math.sqrt(dx * dx + dy * dy)

      val w = (l - l0) / l * alpha * strength * dt
      val dvx = dx * w
      val dvy = dy * w
      val bv = bias(u, v)
      val bu = 1 - bv
      vx(u) += dvx * bu
      vy(u) += dvy * bu
      vx(v) -= dvx * bv
      vy(v) -= dvy * bv
    }

    // 1c. bounding
    val bStr = 1
    for (u <- 0 until numNodes) {
      val uy = py(u)
      if (uy < 0) {
        vy(u) += -uy * bStr
      } else if (uy > 270) {
        vy(u) -= (uy - 270) * bStr
      }
    }


    // 2. update position based on v

    // p(t+dt) = p(t) + dt * v(t)
    for (i <- 0 until numNodes) {
      // decay velocity ("friction")
      vx(i) *= velocityDecay
      vy(i) *= velocityDecay

      // update position based on velocity
      if (!isFixedPosition(i)) {
        px(i) += vx(i) * dt
        py(i) += vy(i) * dt
      }
    }
  }
}
