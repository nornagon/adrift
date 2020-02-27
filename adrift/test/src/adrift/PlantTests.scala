package adrift

import java.nio.file.Paths

import utest.{TestSuite, Tests, assert, test}

import scala.util.Random

object PlantTests extends TestSuite {
  private lazy val data = Data.parse(Paths.get("data"))
  def tests: Tests = Tests {
    test("a basil seed should grow") {
      val state = new GameState(data, 5, 5, new Random(1))
      for (y <- 0 until 5; x <- 0 until 5) {
        state.terrain(x, y) = if (x == 0 || x == 4 || y == 0 || y == 4) data.terrain("wall") else data.terrain("floor")
        state.temperature(x, y) = 290
      }
      val plant = data.items("basil seed").generateItem()
      state.items.put(plant, OnFloor(2, 2))
      assert(state.items.all.size == 1)
      state.elapse(1000)
      assert(state.items.all.size == 1)
      state.elapse(60*8*2)
      println(state.items.all.head.parts)
      println(state.gasComposition)
      assert(state.items.all.size > 1)
    }
  }
}
