package adrift.worldgen

import org.chocosolver.graphsolver.GraphModel
import org.chocosolver.graphsolver.search.strategy.GraphSearch
import org.chocosolver.solver._
import org.chocosolver.solver.constraints.extension.Tuples
import org.chocosolver.solver.exception.ContradictionException
import org.chocosolver.solver.search.SearchState
import org.chocosolver.solver.search.limits.FailCounter
import org.chocosolver.solver.search.loop.monitors.{IMonitorContradiction, IMonitorDownBranch, IMonitorOpenNode, ISearchMonitor}
import org.chocosolver.solver.search.strategy.Search
import org.chocosolver.solver.search.strategy.selectors.values.{IntDomainRandom, IntValueSelector}
import org.chocosolver.solver.search.strategy.selectors.variables.{FirstFail, VariableEvaluator, VariableSelector}
import org.chocosolver.solver.search.strategy.strategy.IntStrategy
import org.chocosolver.solver.trace.LogStatEveryXXms
import org.chocosolver.solver.variables.IntVar
import org.chocosolver.util.objects.graphs.UndirectedGraph
import org.chocosolver.util.objects.setDataStructures.SetType

import scala.collection.immutable.ArraySeq
import scala.collection.mutable
import scala.util.Random

object WaveFunctionCollapse {
  trait TileSet {
    def size: Int
    def propagator(dir: Int, t: Int): Set[Int]
  }

  /** constrain that the total number of tiles set to any of |ts| falls in [lb, ub] */
  case class CountConstraint(ts: Iterable[Int], lb: Int, ub: Int)

  trait GraphTileSet {
    def size: Int

    /** true if |left| can be placed to the left of |right| */
    def allowedHorizontal(left: Int, right: Int): Boolean
    /** true if |top| can be placed above |bottom| */
    def allowedVertical(top: Int, bottom: Int): Boolean
    /** true if the player can navigate from |left| to |right| */
    def connectedHorizontal(left: Int, right: Int): Boolean
    /** true if the player can navigate from |top| to |bottom| */
    def connectedVertical(top: Int, bottom: Int): Boolean

    def allowedAt(x: Int, y: Int, t: Int): Boolean

    def weight(t: Int): Double = 1

    def countConstraints: Iterable[CountConstraint] = Iterable.empty
  }

  val display = Map(
    // L      U      R      D
    (false, false, false, false) -> "o",
    (false, false, false, true ) -> "╷",
    (false, false, true , false) -> "╶",
    (false, false, true , true ) -> "┌",
    (false, true , false, false) -> "╵",
    (false, true , false, true ) -> "│",
    (false, true , true , false) -> "└",
    (false, true , true , true ) -> "├",
    (true , false, false, false) -> "╴",
    (true , false, false, true ) -> "┐",
    (true , false, true , false) -> "─",
    (true , false, true , true ) -> "┬",
    (true , true , false, false) -> "┘",
    (true , true , false, true ) -> "┤",
    (true , true , true , false) -> "┴",
    (true , true , true , true ) -> "┼",
  )

  def graphSolve(
    gts: GraphTileSet,
    width: Int,
    height: Int,
    random: Random,
    mustConnect: ((Int, Int), (Int, Int)) => Boolean = (_, _) => false,
    noisy: Boolean = false,
    decisionCallback: (((Int, Int) => Int), Boolean) => Unit = null,
  ): Option[Seq[Seq[Int]]] = {
    val model = new GraphModel()
    // lb is the lower bound of the graph, i.e. everything in |lb| must be in the final graph
    val lb = new UndirectedGraph(
      model,
      width * height,
      SetType.SMALLBIPARTITESET, // experimentally much faster than the default BITSET
      /* allNodes = */ true
    )
    for (y <- 0 until height) {
      for (x <- 0 until width) {
        if (x < width && mustConnect((x, y), ((x+1)%width, y)))
          lb.addEdge(y*width+x, y*width+(x+1)%width)
        if (y < height - 1 && mustConnect((x, y), (x, y+1)))
          lb.addEdge(y*width+x, (y+1)*width+x)
      }
    }
    // ub is the upper bound, i.e. no edge or node that isn't in |ub| can be in the final graph
    val ub = new UndirectedGraph(
      model,
      width * height,
      SetType.SMALLBIPARTITESET,
      /* allNodes = */ true
    )
    // adjacency: at most, each room is connected to all its neighbors.
    for (y <- 0 until height) {
      for (x <- 0 until width) {
        if (x < width)
          ub.addEdge(y*width+x, y*width+(x+1)%width)
        if (y < height - 1)
          ub.addEdge(y*width+x, (y+1)*width+x)
      }
    }
    // this variable will hold the connectivity of our map
    val connectivity = model.graphVar("map", lb, ub)

    // this enforces that every tile is connected to every other tile
    // (i.e. number of connected components == 1)
    model.nbConnectedComponents(connectivity, model.intVar(1)).post()

    // these variables represent what tile is placed at each location
    val tiles = Array.tabulate(width * height) { i =>
      val (y, x) = (i / width, i % width)
      model.intVar(s"T[$x,$y]", 0, gts.size)
    }

    // this represents the allowed tuples of (left, right, isConnected)
    // e.g. if a tuple (0, 3, true) is present in the set, it means tile 3
    // is allowed to be to the right of tile 0, and those tiles are
    // connected horizontally.
    val allowedHorizontal = new Tuples
    for (t1 <- 0 until gts.size; t2 <- 0 until gts.size; if gts.allowedHorizontal(t1, t2)) {
      val connectedHorizontally = gts.connectedHorizontal(t1, t2)
      allowedHorizontal.add(t1, t2, if (connectedHorizontally) 1 else 0)
    }
    assert(allowedHorizontal.isFeasible, "No tiles could be placed next to each other horizontally")
    // same for vertically.
    val allowedVertical = new Tuples
    for (t1 <- 0 until gts.size; t2 <- 0 until gts.size; if gts.allowedVertical(t1, t2)) {
      val connectedVertically = gts.connectedVertical(t1, t2)
      allowedVertical.add(t1, t2, if (connectedVertically) 1 else 0)
    }
    assert(allowedVertical.isFeasible, "No tiles could be placed next to each other vertically")

    val leftEdgeAllowed: Array[Int] = (for (t <- 0 until gts.size; if gts.allowedHorizontal(-1, t)) yield t).to(Array)
    val rightEdgeAllowed: Array[Int] = (for (t <- 0 until gts.size; if gts.allowedHorizontal(t, -1)) yield t).to(Array)
    val topEdgeAllowed: Array[Int] = (for (t <- 0 until gts.size; if gts.allowedVertical(-1, t)) yield t).to(Array)
    val bottomEdgeAllowed: Array[Int] = (for (t <- 0 until gts.size; if gts.allowedVertical(t, -1)) yield t).to(Array)

    // restrict the tile choice to those that can be placed next to each other, and
    // enforce that the connectivity map matches the tile choice
    for (y <- 0 until height; x <- 0 until width) {
      if (y == 0)
        model.member(tiles(y*width+x), topEdgeAllowed).post()
      if (y == height - 1)
        model.member(tiles(y*width+x), bottomEdgeAllowed).post()
      if (x == 0)
        model.member(tiles(y*width+x), leftEdgeAllowed).post()
      if (x == width - 1)
        model.member(tiles(y*width+x), rightEdgeAllowed).post()

      val allowedInThisSector: Array[Int] = (for (t <- 0 until gts.size; if gts.allowedAt(x, y, t)) yield t).to(Array)
      assert(allowedInThisSector.length > 0, s"Zero allowed tiles at $x,$y")
      model.member(tiles(y * width + x), allowedInThisSector).post()
      val connectedRight = model.boolVar(s"edge([$x,$y] - [${(x+1)%width},$y])")
      model.edgeChanneling(connectivity, connectedRight, y*width+x, y*width+(x+1)%width).post()
      if (x < width - 1) {
        model.table(
          Array(tiles(y * width + x), tiles(y * width + (x + 1)), connectedRight),
          allowedHorizontal,
          "GACSTR+"
        ).post()
      }
      if (y < height - 1) {
        val connectedDown = model.boolVar(s"edge([$x,$y] - [$x,${y+1}])")
        model.edgeChanneling(connectivity, connectedDown, y*width+x, (y+1)*width+x).post()
        model.table(
          Array(tiles(y * width + x), tiles((y+1) * width + x), connectedDown),
          allowedVertical,
          "GACSTR+"
        ).post()
      }
    }

    val counts = mutable.Map.empty[Int, IntVar]

    for (cc <- gts.countConstraints) {
      val relevantCounts: Array[IntVar] = cc.ts.map(t => counts.getOrElseUpdate(t, {
        val count = model.intVar(s"Count of $t", 0, width * height)
        model.count(t, tiles, count).post()
        count
      })).to(Array)

      assert(cc.lb <= cc.ub)
      val count = model.intVar(s"Sum counts: [${cc.ts.mkString(", ")}]", math.max(0, cc.lb), math.min(cc.ub, width * height))
      model.sum(relevantCounts, "=", count).post()
    }

    val solver = model.getSolver
    solver.setSearch(
      Search.conflictOrderingSearch(
        Search.sequencer(
          // this strategy is fairly close to Wave Function Collapse:
          // 1. pick a random tile to fix among those tiles that have the smallest number of options available,
          // 2. set it to a random value
          new IntStrategy(
            tiles,
            new SmallDomainRandom(model, random.nextLong()),
            new IntDomainWeightedRandom(random.nextLong(), gts.weight)
          ),
          new GraphSearch(connectivity).useLastConflict().configure(GraphSearch.MIN_P_DEGREE),
        )
      )
    )

    def printGraph(v: Array[Array[Boolean]]): Unit = {
      var s = ""
      for (y <- 0 until height) {
        for (x <- 0 until width) {
          val connectedRight = v(y * width + x)(y * width + (x + 1) % width)
          val connectedDown = y < height - 1 && v(y * width + x)((y + 1) * width + x)
          val connectedLeft = v(y * width + x)(y * width + (x + width - 1) % width)
          val connectedUp = y > 0 && v(y * width + x)((y - 1) * width + x)
          val tile = tiles(y * width + x)
          if (tile.getDomainSize > 1) {
            s += ((tile.getDomainSize / gts.size.toFloat) * 10).toInt.toString
          } else
            s += display(connectedLeft, connectedUp, connectedRight, connectedDown)
        }
        s += "\n"
      }
      print(s)
    }


    if (noisy)
      solver.plugMonitor(new LogStatEveryXXms(solver, 1000))
    if (false) {
      solver.plugMonitor(new ISearchMonitor with IMonitorOpenNode {
        var i = 0

        override def afterOpenNode(): Unit = {
          if (i % 100 == 0)
            printGraph(connectivity.getValue)
          i += 1
        }
      })
    }
    if (decisionCallback != null) {
      solver.plugMonitor(new ISearchMonitor with IMonitorDownBranch {
        override def afterDownBranch(left: Boolean): Unit = {
          decisionCallback((x: Int, y: Int) => {
            val intVar = tiles(y * width + x)
            if (intVar.getDomainSize == 1)
              intVar.getValue
            else
              -1
          }, false)
        }
      })
      solver.plugMonitor(new ISearchMonitor with IMonitorContradiction {
        override def onContradiction(cex: ContradictionException): Unit = {
          decisionCallback((x: Int, y: Int) => {
            val intVar = tiles(y * width + x)
            if (intVar.getDomainSize == 1)
              intVar.getValue
            else
              -1
          }, true)
        }
      })
    }
    //solver.setGeometricalRestart(2, 1.5, new FailCounter(model, 5000), 100)
    //solver.limitTime("30s")
    solver.setLubyRestart(200, new FailCounter(model, 0), 500)
    solver.limitNode(100 * width * height)

    if (solver.solve()) {
      if (noisy) {
        solver.printStatistics()
        printGraph(connectivity.getValue)
      }
      Some(Seq.tabulate(width, height) { (x, y) => tiles(y * width + x).getValue })
    } else {
      println(s"Failed to solve - ${if (solver.getSearchState == SearchState.STOPPED) "Limit reached" else "Contradiction"}")
      solver.printStatistics()
      if (solver.getSearchState != SearchState.STOPPED)
        println(solver.getContradictionException)
      None
    }
  }

  def solve(tileset: TileSet, width: Int, height: Int, random: Random)(
    restrict: (Int, Int) => Option[Set[Int]] = (_, _) => None
  ): Option[Seq[Seq[Int]]] = {
    val model = new Model
    val horizontallyAllowedPairs = new Tuples
    for (t1 <- 0 until tileset.size; t2 <- 0 until tileset.size; if tileset.propagator(2, t1)(t2)) {
      horizontallyAllowedPairs.add(t1, t2)
    }
    val verticallyAllowedPairs = new Tuples
    for (t1 <- 0 until tileset.size; t2 <- 0 until tileset.size; if tileset.propagator(1, t1)(t2)) {
      verticallyAllowedPairs.add(t1, t2)
    }

    val grid = Array.tabulate(width * height) { i =>
      model.intVar(s"${i % width},${i / width}", 0, tileset.size)
    }

    for (y <- 0 until height; x <- 0 until width) {
      if (x < width - 1) {
        model.table(grid(y * width + x), grid(y * width + x + 1), horizontallyAllowedPairs).post()
      }
      if (y < height - 1) {
        model.table(grid(y * width + x), grid((y + 1) * width + x), verticallyAllowedPairs).post()
      }
      restrict(x, y) foreach { restriction =>
        val v = grid(y * width + x)
        model.member(v, restriction.toArray).post()
      }
    }

    val solver = model.getSolver
    solver.limitTime("10s")
    // NOTE: this does terribly at generating navigable maps. Perhaps a better variable selector
    // could improve on this? e.g. try to select a cell next to one that's already been decided,
    // on the side that's navigable.
    solver.setSearch(Search.intVarSearch(
      new FirstFail(model),
      new IntDomainRandom(random.nextLong()),
      grid: _*
    ))
    if (solver.solve()) {
      Some(Seq.tabulate(width, height) { (x, y) => grid(y * width + x).getValue })
    } else None
  }
}


class IntDomainWeightedRandom(seed: Long, weighting: Int => Double) extends IntValueSelector {
  private val random = new Random(seed)
  import adrift.RandomImplicits._
  override def selectValue(iv: IntVar): Int = {
    val vals = new Array[Int](iv.getDomainSize)
    val ub = iv.getUB
    var i = iv.getLB
    var j = 0
    while (i <= ub) {
      vals(j) = i
      i = iv.nextValue(i)
      j += 1
    }
    random.chooseFrom(vals)(weighting)
  }
}

class SmallDomainRandom(model: Model, seed: Long) extends VariableSelector[IntVar] with VariableEvaluator[IntVar] {
  import adrift.RandomImplicits._
  private val random: Random = new Random(seed)

  override def getVariable(variables: Array[IntVar]): IntVar = {
    val uninstantiated = variables.filterNot(_.isInstantiated)
    if (uninstantiated.isEmpty) return null
    val smallestDomain = uninstantiated.view.map(_.getDomainSize).min
    val candidates = uninstantiated.filter(_.getDomainSize == smallestDomain)
    random.pick(ArraySeq.unsafeWrapArray(candidates))
  }

  override def evaluate(variable: IntVar): Double = variable.getDomainSize
}
