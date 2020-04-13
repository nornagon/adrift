package adrift.worldgen
import java.util
import java.util.PriorityQueue

// Adapted from https://github.com/eclipse/elk/blob/9288fa764d49bbbaa998e2fb74392a1298b09d22/plugins/org.eclipse.elk.alg.force/src/org/eclipse/elk/alg/force/stress/StressMajorization.java
// Licensed EPL-2.0

/**
  * Implementation of stress minimizing layout as described by Gansner, Koren, and North.
  * <ul><li>
  * Emden Gansner, Yehuda Koren, and Stephen North. Graph drawing by stress majorization. <em>Graph Drawing</em>, 2005.
  * </li></ul>
  *
  * The implementation supports performing a layout in one dimension only, preserving the coordinates of the other
  * dimension. For this, set {@link StressOptions#DIMENSION} to either {@link Dimension#X} or {@link Dimension#Y}.
  * Furthermore, nodes can be fixed using the {@link StressOptions#FIXED} option.
  */
class StressMajorization {
  /** All pairs shortest path matrix. */
  private var apsp: Array[Array[Double]] = _
  /** Weights for each pair of nodes. */
  private var w: Array[Array[Double]] = _
  /** Epsilon for terminating the stress minimizing process. */
  private var epsilon = .0
  /** Maximum number of iterations (overrides the {@link #epsilon}). */
  private var iterationLimit = 0
  private var size = 0
  private var positions: Array[(Double, Double)] = _
  private var neighborCache: Array[Array[Int]] = _
  private var desiredEdgeLength: (Int, Int) => Double = _

  /**
    * Initialize all internal structures that are required for the subsequent iterative procedure..
    */
  def initialize(size: Int, iterationLimit: Int, epsilon: Double, desiredEdgeLength: (Int, Int) => Double, initialPosition: Int => (Double, Double), neighbors: Int => TraversableOnce[Int]): Unit = {
    if (size <= 1) return
    this.size = size
    this.iterationLimit = iterationLimit
    this.epsilon = epsilon
    this.desiredEdgeLength = desiredEdgeLength
    neighborCache = Array.tabulate(size) { u => neighbors(u).toArray }
    this.positions = Array.tabulate(size)(initialPosition)
    // all pairs shortest path
    val n = size
    apsp = Array.fill(n, n)(0)
    for (source <- 0 until size) {
      dijkstra(source, apsp(source))
    }
    // init weight matrix
    w = Array.fill(n, n)(0d)
    for (i <- 0 until n) {
      for (j <- 0 until n) {
        val dij = apsp(i)(j)
        val wij = 1.0 / (dij * dij)
        w(i)(j) = wij
      }
    }
  }

  /**
    * Execute the stress-minimizing iteration until a termination criterion is reached.
    */
  def execute(): Unit = {
    if (size <= 1) return
    var count = 0
    var prevStress = computeStress
    var curStress = Double.PositiveInfinity
    do {
      if (count > 0) prevStress = curStress
      for (u <- 0 until size) { // note that we do not use 'NO_LAYOUT' here,
        // since that option results in the node already
        // being excluded by the layout engine
        //if (u.getProperty(StressOptions.FIXED)) continue //todo: continue is not supported
        val newPos = computeNewPosition(u)
        setPosition(u, newPos)
      }
      curStress = computeStress
    } while (
      !done({ count += 1; count - 1 }, prevStress, curStress)
    )
  }

  /**
    * Performs Dijkstra's all pairs shortest path algorithm.
    */
  private def dijkstra(source: Int, dist: Array[Double]): Unit = {
    val nodes = new PriorityQueue[Int]((n1: Int, n2: Int) => java.lang.Double.compare(dist(n1), dist(n2)))
    val mark = new Array[Boolean](size)
    // init
    util.Arrays.fill(mark, false)
    dist(source) = 0
    for (n <- 0 until size) {
      if (n != source) dist(n) = Integer.MAX_VALUE
      nodes.add(n)
    }
    // find shortest paths
    while (!nodes.isEmpty) {
      val u = nodes.poll
      mark(u) = true
      for (v <- neighbors(u); if !mark(v)) {
        // get e's desired length
        //var el = .0
        //if (e.hasProperty(StressOptions.DESIRED_EDGE_LENGTH)) el = e.getProperty(StressOptions.DESIRED_EDGE_LENGTH)
        //else el = desiredEdgeLength
        val el = desiredEdgeLength(u, v)
        val d = dist(u) + el
        if (d < dist(v)) {
          dist(v) = d
          nodes.remove(v)
          nodes.add(v)
        }
      }
    }
  }

  /**
    * Done if either stress improvement is small than {@link StressOptions#EPSILON} or the
    * {@link StressOptions#ITERATION_LIMIT} is reached.
    */
  private def done(count: Int, prevStress: Double, curStress: Double) =
    prevStress == 0 || (((prevStress - curStress) / prevStress) < epsilon) || (count >= iterationLimit)

  private def distance(u: Int, v: Int): Double = {
    val pu = position(u)
    val pv = position(v)
    val dx = pu._1 - pv._1
    val dy = pu._2 - pv._2
    math.sqrt(dx * dx + dy * dy)
  }
  def position(u: Int): (Double, Double) = positions(u)
  private def setPosition(u: Int, pos: (Double, Double)): Unit = positions(u) = pos
  private def neighbors(u: Int): TraversableOnce[Int] = neighborCache(u)

  /**
    * @return the stress value of the current node positioning.
    */
  private def computeStress: Double = {
    var stress = 0d
    var u = 0
    while (u < size) {
      var v = u + 1
      while (v < size) {
        val eucDist = distance(u, v)
        val eucDisplacement = eucDist - apsp(u)(v)
        stress += w(u)(v) * eucDisplacement * eucDisplacement

        v += 1
      }

      u += 1
    }
    stress
  }

  /**
    * Computes a new position for the passed node. The procedure is described in
    * <em>Section 2.3 Localized optimization</em> of the paper.
    */
  private def computeNewPosition(u: Int): (Double, Double) = {
    var weightSum = 0d
    var xDisp = 0d
    var yDisp = 0d
    for (v <- 0 until size; if u != v) {
      val wij = w(u)(v)
      weightSum += wij
      val eucDist = distance(u, v)
      if (eucDist > 0) {
        val pv = position(v)
        val pu = position(u)
        xDisp += wij * (pv._1 + apsp(u)(v) * (pu._1 - pv._1) / eucDist)
        yDisp += wij * (pv._2 + apsp(u)(v) * (pu._2 - pv._2) / eucDist)
      }
    }
    (xDisp / weightSum, yDisp / weightSum)
  }
}