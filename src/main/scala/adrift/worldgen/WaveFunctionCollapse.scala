package adrift.worldgen

import org.chocosolver.graphsolver.GraphModel
import org.chocosolver.graphsolver.search.strategy.GraphSearch
import org.chocosolver.solver._
import org.chocosolver.solver.constraints.extension.Tuples
import org.chocosolver.solver.search.loop.monitors.{IMonitorOpenNode, ISearchMonitor}
import org.chocosolver.solver.search.strategy.Search
import org.chocosolver.solver.search.strategy.selectors.values.IntDomainRandom
import org.chocosolver.solver.search.strategy.selectors.variables.FirstFail
import org.chocosolver.solver.search.strategy.strategy.IntStrategy
import org.chocosolver.solver.trace.LogStatEveryXXms
import org.chocosolver.util.objects.graphs.UndirectedGraph
import org.chocosolver.util.objects.setDataStructures.SetType

import scala.util.Random

object WaveFunctionCollapse {
  trait TileSet {
    def size: Int
    def propagator(dir: Int, t: Int): Set[Int]
  }

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

  def graphSolve(gts: GraphTileSet, width: Int, height: Int, random: Random): Option[Seq[Seq[Int]]] = {
    val model = new GraphModel()
    // lb is the lower bound of the graph, i.e. everything in |lb| must be in the final graph
    val lb = new UndirectedGraph(
      model,
      width * height,
      SetType.SMALLBIPARTITESET, // experimentally much faster than the default BITSET
      /* allNodes = */ true
    )
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
        if (x < width - 1) {
          ub.addEdge(y*width+x, y*width+x+1)
        }
        if (y < height - 1) {
          ub.addEdge(y*width+x, (y+1)*width+x)
        }
      }
    }
    // this variable will hold the connectivity of our map
    val connectivity = model.graphVar("map", lb, ub)

    // this enforces that every tile is connected to every other tile
    // (i.e. number of connected components == 1)
    model.nbConnectedComponents(connectivity, model.intVar(1)).post()

    // these variables represent what tile is placed at each location
    val tiles = Array.tabulate(width * height) { _ =>
      model.intVar(0, gts.size)
    }

    /*
    for (x <- 0 until width) {
      for (y <- 0 until height / 2 - 1) {
        val oppY = height - 1 - y
        val northV = model.isEdge(connectivity, y*width+x, (y+1)*width+x)
        val southV = model.isEdge(connectivity, oppY*width+x, (oppY-1)*width+x)
        model.arithm(northV, "=", southV).post()

        if (x < width-1) {
          val northH = model.isEdge(connectivity, y * width + x, y * width + x + 1)
          val southH = model.isEdge(connectivity, oppY * width + x, oppY * width + x + 1)
          model.arithm(northH, "=", southH).post()
        }
      }
    }
    */

    // this represents the allowed tuples of (left, right, isConnected)
    // e.g. if a tuple (0, 3, true) is present in the set, it means tile 3
    // is allowed to be to the right of tile 0, and those tiles are
    // connected horizontally.
    val allowedHorizontal = new Tuples
    for (t1 <- 0 until gts.size; t2 <- 0 until gts.size; if gts.allowedHorizontal(t1, t2)) {
      val connectedHorizontally = gts.connectedHorizontal(t1, t2)
      allowedHorizontal.add(t1, t2, if (connectedHorizontally) 1 else 0)
    }
    // same for vertically.
    val allowedVertical = new Tuples
    for (t1 <- 0 until gts.size; t2 <- 0 until gts.size; if gts.allowedVertical(t1, t2)) {
      val connectedVertically = gts.connectedVertical(t1, t2)
      allowedVertical.add(t1, t2, if (connectedVertically) 1 else 0)
    }

    // restrict the tile choice to those that can be placed next to each other, and
    // enforce that the connectivity map matches the tile choice
    for (y <- 0 until height; x <- 0 until width) {
      if (x < width - 1) {
        val connectedRight = model.isEdge(connectivity, y*width+x, y*width+x+1)
        model.table(
          Array(tiles(y * width + x), tiles(y * width + x + 1), connectedRight),
          allowedHorizontal
        ).post()
      }
      if (y < height - 1) {
        val connectedDown = model.isEdge(connectivity, y*width+x, (y+1)*width+x)
        model.table(
          Array(tiles(y * width + x), tiles((y+1) * width + x), connectedDown),
          allowedVertical
        ).post()
      }
    }

    val solver = model.getSolver
    solver.setSearch(
      new IntStrategy(
        tiles,
        new FirstFail(model),
        new IntDomainRandom(random.nextLong)
      ),
      new GraphSearch(connectivity).useLastConflict().configure(GraphSearch.MIN_P_DEGREE),
      //new GraphStrategy(connectivity)
    )

    def printGraph(v: Array[Array[Boolean]]): Unit = {
      for (y <- 0 until height) {
        for (x <- 0 until width) {
          val connectedRight = x < width - 1 && v(y * width + x)(y * width + x + 1)
          val connectedDown = y < height - 1 && v(y * width + x)((y + 1) * width + x)
          val connectedLeft = x > 0 && v(y * width + x)(y * width + x - 1)
          val connectedUp = y > 0 && v(y * width + x)((y - 1) * width + x)
          print(display(connectedLeft, connectedUp, connectedRight, connectedDown))
        }
        println()
      }
    }


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
    //solver.setLubyRestart(500, new FailCounter(model, 1000), 500)
    solver.limitTime("10s")

    if (solver.solve()) {
      solver.printStatistics()
      printGraph(connectivity.getValue)
      Some(Seq.tabulate(width, height) { (x, y) => tiles(y * width + x).getValue })
    } else {
      println("Failed to solve")
      solver.printStatistics()
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
