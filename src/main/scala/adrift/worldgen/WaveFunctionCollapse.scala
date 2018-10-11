package adrift.worldgen

import scala.util.Random

import org.chocosolver.solver._
import org.chocosolver.solver.constraints.extension.Tuples
import org.chocosolver.solver.search.strategy.Search
import org.chocosolver.solver.search.strategy.selectors.values.IntDomainRandom
import org.chocosolver.solver.search.strategy.selectors.variables.FirstFail

object WaveFunctionCollapse {
  trait TileSet {
    def size: Int
    def propagator(dir: Int, t: Int): Set[Int]
  }

  def solve(tileset: TileSet, width: Int, height: Int, random: Random): Option[Seq[Seq[Int]]] = {
    val model = new Model
    val horizontallyAllowedPairs = new Tuples
    for (t1 <- 0 until tileset.size; t2 <- 0 until tileset.size; if tileset.propagator(0, t1)(t2)) {
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
    }

    val solver = model.getSolver()
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
