package adrift.worldgen
import java.util
import java.util.PriorityQueue

// Adapted from https://github.com/eclipse/elk/blob/9288fa764d49bbbaa998e2fb74392a1298b09d22/plugins/org.eclipse.elk.alg.force/src/org/eclipse/elk/alg/force/stress/StressMajorization.java
// Licensed EPL-2.0

// Here are some other interesting graph drawing resources:
//
// Drawing Graphs by Eigenvectors: Theory and Practice, Y. Koren 2004 https://www.sciencedirect.com/science/article/pii/S089812210500204X
// DIG-COLA: Directed Graph Layout through Constrained Energy Minimization, T. Dwyer & Y. Koren 2005 http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.183.451
// Force-Directed Drawing Algorithms, S. Kobourov 2013 https://cs.brown.edu/people/rtamassi/gdhandbook/chapters/force-directed.pdf
// Graph Drawing by Force-directed Placement, Fruchterman & Reingold
//    https://github.com/gitGNU/gnu_multigraph/blob/ad76473433f88fc47319df2e4f31751f4cf5483f/src/org/nongnu/multigraph/layout/ForceLayout.java
//    https://ptolemy.berkeley.edu/projects/embedded/Alumni/awrixon/route/graph/layout/ForceLayout.java
// https://github.com/anvaka/ngraph.forcelayout
// A Sparse Stress Model, M. Ortmann, M. Klimenta, U. Brandes 2016 https://arxiv.org/abs/1608.08909
//    https://github.com/MarkOrtmann/sparse-stress
//
// The paper which this is based on is http://www.graphviz.org/Documentation/GKN04.pdf

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
  private var distanceP: ((Double, Double), (Double, Double)) => Double = _
  private var diff: ((Double, Double), (Double, Double)) => (Double, Double) = _
  private var isFixedPosition: Int => Boolean = _


  /**
    * Initialize all internal structures that are required for the subsequent iterative procedure..
    */
  def initialize(
    numNodes: Int,
    neighbors: Int => IterableOnce[Int],
    iterationLimit: Int,
    epsilon: Double,
    desiredEdgeLength: (Int, Int) => Double,
    initialPosition: Int => (Double, Double),
    isFixedPosition: Int => Boolean,
    distance: ((Double, Double), (Double, Double)) => Double,
    diff: ((Double, Double), (Double, Double)) => (Double, Double)
  ): Unit = {
    if (numNodes <= 1) return
    this.size = numNodes
    this.iterationLimit = iterationLimit
    this.epsilon = epsilon
    this.desiredEdgeLength = desiredEdgeLength
    this.neighborCache = Array.tabulate(numNodes) { u => neighbors(u).iterator.toArray }
    this.positions = Array.tabulate(numNodes)(initialPosition)
    this.isFixedPosition = isFixedPosition
    this.distanceP = distance
    this.diff = diff
    // all pairs shortest path
    val n = numNodes
    apsp = Array.fill(n, n)(0)
    for (source <- 0 until numNodes) {
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
    while ({
      if (count > 0) prevStress = curStress
      for (u <- 0 until size) {
        if (!this.isFixedPosition(u)) {
          val newPos = computeNewPosition(u)
          setPosition(u, newPos)
        }
      }
      curStress = computeStress
      //assert(curStress <= prevStress, s"stress must never increase (was $prevStress, now $curStress)")
      !done({ count += 1; count - 1 }, prevStress, curStress)
    }) {}
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
      for (v <- neighbors(u).iterator; if !mark(v)) {
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

  private def isFullyConnected: Boolean = {
    val dist = Array.fill(size)(Integer.MAX_VALUE)
    val source = 0
    val nodes = new PriorityQueue[Int]((n1: Int, n2: Int) => java.lang.Double.compare(dist(n1), dist(n2)))
    val mark = new Array[Boolean](size)
    util.Arrays.fill(mark, false)
    dist(source) = 0
    var visited = 0
    nodes.add(source)
    while (!nodes.isEmpty) {
      val u = nodes.poll
      if (!mark(u)) visited += 1
      mark(u) = true
      for (v <- neighbors(u).iterator; if !mark(v)) {
        nodes.add(v)
      }
    }
    visited == size
  }

  /**
    * Done if either stress improvement is small than {@link StressOptions#EPSILON} or the
    * {@link StressOptions#ITERATION_LIMIT} is reached.
    */
  private def done(count: Int, prevStress: Double, curStress: Double) =
    prevStress == 0 || ((math.abs(prevStress - curStress) / prevStress) < epsilon) || (count >= iterationLimit)

  private def distance(u: Int, v: Int): Double = {
    val pu = position(u)
    val pv = position(v)
    distanceP(pu, pv)
  }
  def position(u: Int): (Double, Double) = positions(u)
  private def setPosition(u: Int, pos: (Double, Double)): Unit = positions(u) = pos
  private def neighbors(u: Int): IterableOnce[Int] = neighborCache(u)

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
        val (vx, vy) = position(v)
        val (ux, uy) = position(u)
        val (dx, dy) = diff((ux, uy), (vx, vy))
        xDisp += wij * (vx + apsp(u)(v) * dx / eucDist)
        yDisp += wij * (vy + apsp(u)(v) * dy / eucDist)
      }
    }
    (xDisp / weightSum, yDisp / weightSum)
  }
}
