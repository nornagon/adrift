package adrift

import java.nio.file.Paths

import utest.{TestSuite, Tests, assert, test}

import scala.util.Random

object PlantTests extends TestSuite {
  private lazy val data = Data.parse(Paths.get("data"))
  def tests: Tests = Tests {
    test("a basil seed should grow") {
//      val state = new GameState(data, 5, 5, new Random(1))
//      for (y <- 0 until 5; x <- 0 until 5) {
//        state.terrain(x, y) = if (x == 0 || x == 4 || y == 0 || y == 4) data.terrain("wall") else data.terrain("floor")
//        state.temperature(x, y) = 290
//      }
//      val plant = data.items("basil seed").generateItem()
//      state.items.put(plant, OnFloor(2, 2))
//      assert(state.items.all.size == 1)
//      state.elapse(1000)
//      assert(state.items.all.size == 1)
//      state.elapse(60*8*2)
//      println(state.items.all.head.parts)
//      println(state.gasComposition)
//      assert(state.items.all.size > 1)
    }

    test("disjoint set") {
      val ds = new DisjointSet[Int]
      assert(!ds.contains(3))
      assert(ds.find(3).isEmpty)
      ds.makeSet(3)
      assert(ds.contains(3))
      assert(ds.find(3).contains(3))
      ds.makeSet(4)
      assert(ds.contains(4))
      assert(ds.find(4).contains(4))
      ds.union(3, 4)
      assert(ds.find(3).nonEmpty)
      assert(ds.find(4).nonEmpty)
      assert(ds.find(3) == ds.find(4))

      assert(ds.component(3).toSet == Set(3, 4))
    }

    test("disjoint set - children") {
      val ds = new DisjointSet[Int]
      ds.makeSet(1)
      ds.makeSet(2)
      ds.makeSet(3)
      ds.makeSet(4)

      ds.union(1, 2)
      ds.union(3, 4)
      assert(ds.component(1).toSet == Set(1, 2))
      assert(ds.component(2).toSet == Set(1, 2))
      assert(ds.component(3).toSet == Set(3, 4))
      assert(ds.component(4).toSet == Set(3, 4))

      ds.union(2, 4)
      assert(ds.component(1).toSet == Set(1, 2, 3, 4))
      assert(ds.component(2).toSet == Set(1, 2, 3, 4))
      assert(ds.component(3).toSet == Set(1, 2, 3, 4))
      assert(ds.component(4).toSet == Set(1, 2, 3, 4))
    }
  }
}
