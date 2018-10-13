package adrift.worldgen

import org.chocosolver.graphsolver.GraphModel
import org.chocosolver.graphsolver.search.strategy.GraphStrategy
import org.chocosolver.graphsolver.search.strategy.arcs.RandomArc
import org.chocosolver.graphsolver.search.strategy.nodes.RandomNode
import org.chocosolver.solver._
import org.chocosolver.solver.constraints.extension.Tuples
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

  ////////////////// EXPERIMENTAL //////////////////
  trait GraphTileSet

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
  val tileCons = Seq(
    // L      U      R      D
    (false, false, false, false),
    (false, false, false, true ),
    (false, false, true , false),
    (false, false, true , true ),
    (false, true , false, false),
    (false, true , false, true ),
    (false, true , true , false),
    (false, true , true , true ),
    (true , false, false, false),
    (true , false, false, true ),
    (true , false, true , false),
    (true , false, true , true ),
    (true , true , false, false),
    (true , true , false, true ),
    (true , true , true , false),
    (true , true , true , true ),
  )

  def graphSolve(gts: GraphTileSet, width: Int, height: Int, random: Random): Unit = {
    val model = new GraphModel()
    val lb = new UndirectedGraph(model, width * height, SetType.SMALLBIPARTITESET, true)
    val ub = new UndirectedGraph(model, width * height, SetType.SMALLBIPARTITESET, true)
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
    val map = model.graphVar("map", lb, ub)
    model.nbConnectedComponents(map, model.intVar(2)).post()
    val solver = model.getSolver
    //solver.setSearch(new GraphStrategy(map, 1234))
    val tiles = Array.tabulate(width * height) { _ =>
      model.intVar(0, 15)
    }
    for (y <- 0 until height; x <- 0 until width) {
      val tile = tiles(y * width + x)

      val connectedLeft = if (x > 0) {
        model.isEdge(map, y*width+x-1, y*width+x)
      } else {
        model.boolVar(true)
      }
      val ltuples = new Tuples
      for (((l, _, _, _), i) <- tileCons.zipWithIndex) {
        ltuples.add(i, if (l) 1 else 0)
      }
      model.table(tile, connectedLeft, ltuples).post()

      val connectedUp = if (y > 0) {
        model.isEdge(map, (y-1)*width+x, y*width+x)
      } else {
        model.boolVar(true)
      }
      val utuples = new Tuples
      for (((_, u, _, _), i) <- tileCons.zipWithIndex) {
        utuples.add(i, if (u) 1 else 0)
      }
      model.table(tile, connectedUp, utuples).post()

      val connectedRight = if (x < width - 1) {
        model.isEdge(map, y*width+x, y*width+x+1)
      } else {
        model.boolVar(true)
      }
      val rtuples = new Tuples
      for (((_, _, r, _), i) <- tileCons.zipWithIndex) {
        rtuples.add(i, if (r) 1 else 0)
      }
      model.table(tile, connectedRight, rtuples).post()

      val connectedDown = if (y < height - 1) {
        model.isEdge(map, y*width+x, (y+1)*width+x)
      } else {
        model.boolVar(true)
      }
      val dtuples = new Tuples
      for (((_, _, _, d), i) <- tileCons.zipWithIndex) {
        dtuples.add(i, if (d) 1 else 0)
      }
      model.table(tile, connectedDown, dtuples).post()
    }
    solver.setSearch(
      new IntStrategy(
        tiles,
        new org.chocosolver.solver.search.strategy.selectors.variables.Random(43),
        //new FirstFail(model),
        new IntDomainRandom(42)
      ),
      new GraphStrategy(
        map,
        new RandomNode(map, 43),
        new RandomArc(map, 42),
        GraphStrategy.NodeArcPriority.ARCS
      ),
    )
    solver.plugMonitor(new LogStatEveryXXms(solver, 1000))
    /*solver.plugMonitor(new ISearchMonitor with IMonitorOpenNode {
      override def afterOpenNode(): Unit = {
        val v = map.getValue
        for (y <- 0 until height) {
          for (x <- 0 until width) {
            val connectedRight = x < width-1 && v(y*width + x)(y*width+x+1)
            val connectedDown = y < height-1 && v(y*width + x)((y+1)*width+x)
            val connectedLeft = x > 0 && v(y*width + x)(y*width+x-1)
            val connectedUp = y > 0 && v(y*width + x)((y-1)*width+x)
            print(display(connectedLeft, connectedUp, connectedRight, connectedDown))
          }
          println()
        }
      }
    })*/

    solver.limitTime("100s")
    if (solver.solve()) {
      val v = map.getValue
      for (y <- 0 until height) {
        for (x <- 0 until width) {
          val connectedRight = x < width-1 && v(y*width + x)(y*width+x+1)
          val connectedDown = y < height-1 && v(y*width + x)((y+1)*width+x)
          val connectedLeft = x > 0 && v(y*width + x)(y*width+x-1)
          val connectedUp = y > 0 && v(y*width + x)((y-1)*width+x)
          print(display(connectedLeft, connectedUp, connectedRight, connectedDown))
        }
        println()
      }
    } else {
      println("womp womp")
    }
    solver.printStatistics()
  }

  ////////////////// </EXPERIMENTAL> //////////////////

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
