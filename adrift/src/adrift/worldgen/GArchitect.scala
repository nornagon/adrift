package adrift.worldgen
import adrift.Grid

import util.Random
import adrift.RandomImplicits._
import adrift.worldgen.NEATArchitect.RoomTypeId
import adrift.worldgen.RoomType.RoomType

import scala.collection.mutable
import scala.collection.mutable.ListBuffer


class GArchitect {
// A genetic algorithm Architect.
  // For all GAs, the first real step is to define the genome.
  // Our genome will be a 2d graph of spaces, with associated area, with lines being connectivity between them.
  // we may start with one each of a given type of space (say, habitation),
  // then add connectivity to however many different other spaces (say, dining, or each of the 'work' spaces)
  // It may be possible to have multiple 'copies' of a particular type of space, or maybe only one copy.
  // For example, it may be reasonable to restrict the genome to have only a single copy of the 'Engine' space
  // or maybe it's reasonable to have up to (say) four copies of that space.
  // One node in teh genome may be part of the genometry, so 'aft' might be a node that ends up having 0 sq ft of
  // area allocated, but is connected to the 'engine' space, making it desirable that the engines be close to the aft
  // section of the vehicle.
  // Users will probably define a list of spaces, possible percentage allocation of space (with bounds -
  // habitation can be 5-15% of total space), maximum and minimum number of those 'spaces' and desired connectivity with
  // some weighting / reward factor.  So Engines might be weighted as '10' for 'aft' and have connectivity to
  // powerplant at 8, while connectivity to habitation would be 1.  The GA would get rewarded for putting the engine(s)
  // at the back and closer to the power plant(s).

  // The GA should in one stage take the available space and partition it into geometric sections.  Then an evaluation
  // can occur and some amount of reward can be allocated.
  // This process may be fractal - within each chunk of allocated space  the available space may get broken up further.
  // For example 'engineering' may get broken up into industrial fabrication, recycling, etc.  These sections may be
  // further subdivided arbitrarily.

}

object NEATArchitect {
  case class RoomId(value: Int) extends AnyVal
  case class ConnectionId(id: Int) extends AnyVal
  case class RoomTypeId(id: Int) extends AnyVal

  case class RoomGene(
    id: RoomId,
    roomType: RoomTypeId
  )

  case class ConnectionGene(
    id: ConnectionId,

    // Ids of the rooms this gene connects
    a: RoomId,
    b: RoomId,

    weight: Float,

    enabled: Boolean,
  )

  case class Genome(
    rooms: Seq[RoomGene],
    connections: Seq[ConnectionGene],
    mutationRate: Int = 3
  ) {
    //lazy val adjacency = ??? /* ... lazily compute adjacency matrix ... */

    def connectionIdForRooms(a: RoomId, b: RoomId): ConnectionId = {
      if (a.value > b.value) return connectionIdForRooms(b, a)
      assert(b.value < (1 << 16), "Make it a long")
      ConnectionId(a.value << 16 + b.value)
    }

    def mutateAddConnection()(implicit random: Random): Genome = {
      val roomA = random.pick(rooms)
      val roomB = random.pick(rooms)
      if (roomA.id == roomB.id) return this
      val (rA, rB) = if (roomA.id.value < roomB.id.value) (roomA, roomB) else (roomB, roomA)
      if (connections.exists(g => g.a == rA.id && g.b == rB.id)) return this

      val newConnection: ConnectionGene = ConnectionGene(
        id = connectionIdForRooms(rA.id, rB.id),
        a = rA.id,
        b = rB.id,
        weight = 1f,
        enabled = true
      )
      copy(connections = connections :+ newConnection)
    }

    def mutateRandomConnection(f: ConnectionGene => ConnectionGene)(implicit random: Random): Genome = {
      val i = random.between(0, connections.size)
      copy(connections = connections.patch(i, Seq(f(connections(i))), 1))
    }

    def mutateDisableConnection()(implicit random: Random): Genome = mutateRandomConnection(_.copy(enabled = false))
    def mutateEnableConnection()(implicit random: Random): Genome = mutateRandomConnection(_.copy(enabled = true))
    def mutateConnectionWeight()(implicit random: Random): Genome = mutateRandomConnection { c =>
      c.copy(weight = c.weight * random.between(0.9f, 1.1f))
    }

    def mutateAddRoom()(implicit random: Random): Genome = {
      val newRoomTypeId = random.pick(RoomType.byId.keys)
      val newRoomType = RoomType.byId(newRoomTypeId)
      val existingCount = rooms.count(_.roomType == newRoomTypeId)
      if (existingCount >= newRoomType.maxQuantity) return this

      val roomGene = RoomGene(RoomId(rooms.maxBy(_.id.value).id.value + 1), newRoomTypeId)

      val otherRooms = random.nOf(random.between(1, 3), rooms).distinct

      val connectionGenes = otherRooms.map { r =>
        ConnectionGene(
          id = connectionIdForRooms(r.id, roomGene.id),
          a = r.id,
          b = roomGene.id,
          weight = 1,
          enabled = true
        )
      }

      copy(rooms = rooms :+ roomGene, connections = connections ++ connectionGenes)
    }

    def mutateMutationRate()(implicit random: Random): Genome =
      copy(mutationRate = math.max(1, mutationRate + random.between(-1, 1)))

    val mutationFunctions = Seq(
      ((g: Genome, r: Random) => g.mutateAddRoom()(r), 1f),
      ((g: Genome, r: Random) => g.mutateEnableConnection()(r), 1f),
      ((g: Genome, r: Random) => g.mutateDisableConnection()(r), 1f),
      ((g: Genome, r: Random) => g.mutateAddConnection()(r), 1f),
      ((g: Genome, r: Random) => g.mutateConnectionWeight()(r), 1f),
      ((g: Genome, r: Random) => g.mutateMutationRate()(r), 1f),
    )

    def mutate()(implicit random: Random): Genome = {
      val mutations = random.nFrom(random.between(0, mutationRate), mutationFunctions)(_._2).map(_._1)
      mutations.foldLeft(this)((genome, mutate) => mutate(genome, random))
    }

    def crossover(other: Genome): Genome = ???
  }

  def newGenome()(implicit random: Random): Genome = {
    var nextRoomId = 0
    val roomGenes: Seq[RoomGene] = (for {
      (rtId, rt) <- RoomType.byId
      num = random.between(rt.minQuantity, rt.maxQuantity)
      _ <- 1 to num
    } yield {
      nextRoomId += 1
      RoomGene(RoomId(nextRoomId), rtId)
    })(collection.breakOut)

    var n = 0
    val connections = mutable.Buffer.empty[ConnectionGene]
    for (i <- 1 until roomGenes.size) {
      val id = ConnectionId({ n += 1; n })
      connections += ConnectionGene(id, roomGenes(random.between(0, i)).id, roomGenes(i).id, 1, enabled = true)
      if (random.oneIn(10)) {
        connections += ConnectionGene(id, roomGenes(random.between(0, i)).id, roomGenes(i).id, 1, enabled = true)
      }
    }

    Genome(roomGenes, connections)
  }

  case class RoomLayout(roomPositions: Map[RoomId, (Double, Double)]) {}

  def layout(g: Genome, iterationLimit: Int = 50)(implicit random: Random): RoomLayout = {
    val sm = new StressMajorization()
    val idxToRoomId: Map[Int, RoomId] = g.rooms.zipWithIndex.map { case (r, i) => i -> r.id }(collection.breakOut)
    val roomIdToIdx = idxToRoomId.map { case (k, v) => v -> k }
    def neighbs(i: Int): TraversableOnce[Int] = {
      val needle = g.rooms(i).id
      g.connections.flatMap {
        case c: ConnectionGene if c.a == needle => Some(roomIdToIdx(c.b))
        case c: ConnectionGene if c.b == needle => Some(roomIdToIdx(c.a))
        case _ => None
      }
    }
    val lengths: Map[(Int, Int), Double] = g.connections.map { c =>
      val idxA = roomIdToIdx(c.a)
      val idxB = roomIdToIdx(c.b)
      val rtA = RoomType.byId(g.rooms(idxA).roomType)
      val rtB = RoomType.byId(g.rooms(idxB).roomType)
      (idxA, idxB) -> math.sqrt(rtA.spaceWeight + rtB.spaceWeight)*4
    }(collection.breakOut)
    val cylinderCircumference = 1000d
    def normalizeX(x: Double): Double =
      if (x >= 0 && x < cylinderCircumference) x
      else ((x % cylinderCircumference) + cylinderCircumference) % cylinderCircumference
    def normalizePosition(p: (Double, Double)): (Double, Double) = (normalizeX(p._1), p._2)
    sm.initialize(
      g.rooms.size,
      iterationLimit,
      epsilon = 0.001,
      desiredEdgeLength = (u, v) => lengths(if (u < v) (u, v) else (v, u)),
      initialPosition = _ => (random.between(-1d, 1d), random.between(-1d, 1d)),
      neighbors = neighbs,
      distance = (a, b) => {
        val na = normalizePosition(a)
        val nb = normalizePosition(b)
        val dx = math.min(math.abs(na._1 - nb._1), cylinderCircumference - math.abs(na._1 - nb._1))
        val dy = na._2 - nb._2
        math.sqrt(dx * dx + dy * dy)
      },
      diff = (p1, p2) => {
        val dx = {
          val x1 = p1._1
          val x2 = p2._1
          val nx1 = normalizeX(x1)
          val nx2 = normalizeX(x2)
          val dx = math.abs(nx1 - nx2)
          math.min(dx, cylinderCircumference - dx) * math.signum(x1 - x2)
        }
        (dx, p1._2 - p2._2)
      })
    sm.execute()
    val roomPositions: Map[RoomId, (Double, Double)] = g.rooms.indices.map({ i =>
      g.rooms(i).id -> normalizePosition(sm.position(i))
    })(collection.breakOut)
    RoomLayout(roomPositions)
  }
}

object RoomType {
  case class RoomType(
    name: String,
    spaceWeight: Double,
    minQuantity:Int,
    maxQuantity:Int,
  )
  val all: Seq[RoomType] = Seq(
    RoomType("command", spaceWeight = 10, minQuantity = 1, maxQuantity = 1),

    RoomType("engine room", spaceWeight = 100, minQuantity = 1, maxQuantity = 5),

    RoomType("recycling", spaceWeight = 50, minQuantity = 1, maxQuantity = 5),

    RoomType("fabrication", spaceWeight = 50, minQuantity = 5, maxQuantity = 10),

    RoomType("crew quarters", spaceWeight = 200, minQuantity = 1, maxQuantity = 500),
    RoomType("promenade", spaceWeight = 100,minQuantity = 1,maxQuantity = 1),
    RoomType("dining", spaceWeight = 50,minQuantity = 4,maxQuantity = 10),
    RoomType("holo suite", spaceWeight = 10,minQuantity = 5,maxQuantity = 10),
    RoomType("lounge", spaceWeight = 10,minQuantity = 10,maxQuantity = 20),
  )

  val byId: Map[RoomTypeId, RoomType] = all.zipWithIndex.map {
    case (rt, id) => RoomTypeId(id) -> rt
  }(collection.breakOut)
}
case class Coordinates(x:Int,y:Int)
case class Room(roomType: RoomTypeId, coords:Coordinates) {}

/*
case object GArchitect {
  type Layout = Seq[Room]
  case class PopulationReport(p: Layout, rawMetrics:Seq[Int], scaledMetrics: Double)
  val random = new Random(1)
  val SC_vertical = 100
  val SC_horizontal = 100 // note that as a toroid the map wraps around the horizontal axis.

  def distance(c1:Coordinates,c2:Coordinates):Double = {
    if (c1==c2) {
      0.0
    } else {
      if (c1.x == c2.x) {
        math.abs(c2.y-c1.y).toDouble
      } else {
        // because this is a toroid, we have to figure out whether it's closer to go to the right or left
        val dx = math.min(math.abs(c2.x-c1.x),math.abs(c2.x-c1.x + SC_horizontal))
        if (c1.y == c2.y) {
          dx.toDouble
        } else {
          math.sqrt(math.pow(dx,2)+math.pow(c2.y-c1.y,2))
        }
      }
    }
  }

  def wrapX(x: Int): Int = (x + SC_horizontal) % SC_horizontal

  def spaceAllocation(roomlist:Seq[Room]):Int = {
    // provide reward in proportion to distance from roomtypes that have a large space allocation
    def roomDistances(roomlist: Seq[Room]): Map[Room,Map[Room,Double]] = {
      val distmap = roomlist.map(r1 => {
        val distances = roomlist.map(r2 => {
          distance(r1.coords,r2.coords)
        })
        (roomlist zip distances).toMap
      })
      (roomlist zip distmap).toMap
    }
    val roomdistances = roomDistances(roomlist)
    // find the 5 closest rooms
    // multiply by the space allocation value for the roomtype divided by the number of rooms of that type there are.
    val rtypes = roomlist map(r=> r.roomType)
    roomlist.map(r => {
      val numRooms = rtypes.count(p => p eq r.roomType)
      roomdistances(r).values.toSeq.sorted.slice(0,5).sum * r.roomType.spaceWeight / numRooms
    }).sum.toInt
    // thats our reward.  Yar.
  }

  def spaceAllocation2(roomlist: Seq[Room]): Int = {
    val totalSpaceOnTheShip = SC_vertical * SC_horizontal
    val totalSpace = roomlist.map(_.roomType).distinct.map(_.spaceWeight).sum
    val realSpacePerSpaceWeight = totalSpaceOnTheShip / totalSpace
    val realSpacePerRoomByType = roomlist.groupBy(_.roomType).map {
      case (rt, rooms) => rt -> rt.spaceWeight / rooms.size * realSpacePerSpaceWeight
    }
    val grid = new Grid(SC_horizontal, SC_vertical)(collection.mutable.Buffer.empty[Room])
    roomlist.foreach { room => grid(room.coords.x, room.coords.y).append(room) }
    def distanceToClosest(x: Int, y: Int): Double = {
      var r = 1
      while (true) {
        val nearby = for {
          tx <- x - r to x + r
          ty <- y - r to y + r
          if tx != x && ty != y
          if ty >= 0 && ty < SC_vertical
          room <- grid(wrapX(tx), ty)
        } yield room
        if (nearby.nonEmpty) return r
        r += 1
      }
      Int.MaxValue
    }

    -roomlist.map { room =>
      val d = distanceToClosest(room.coords.x, room.coords.y) * 2 + 1
      val d2 = d * d
      val delta = math.abs(realSpacePerRoomByType(room.roomType) - d2)
      delta
    }.sum.round.toInt
  }

  def linedup(roomlist:Seq[Room]):Int = {
    // provide reward based on the number of rooms that share x or y coordinates - that are lined up
    // largest possible number of unique rows or columns is the length of the roomlist.
    // smallest possible value is 1 if all rooms are at the same point.
    val xs = roomlist.map( r=> r.coords.x)
    val ys = roomlist.map( r=> r.coords.y)
    roomlist.length*2 - xs.distinct.length - ys.distinct.length
  }

  def nooverlap(roomlist: Seq[Room]): Int = {
    val roomsPerCoordinate = new collection.mutable.HashMap[Coordinates, Int]()
    for (room <- roomlist) {
      val pre = roomsPerCoordinate.getOrElseUpdate(room.coords, 0)
      roomsPerCoordinate.put(room.coords, pre + 1)
    }
    roomlist.size - roomsPerCoordinate.values.count(_ > 1)
  }

  private val metrics = Seq(
    linedup _ -> 1d,
    spaceAllocation2 _ -> 1d,
    nooverlap _ -> 2d,
    //((x: Seq[Room]) => 1) -> 1d
  )

  def listAdd(a:Seq[Double],b:Seq[Double]): Seq[Double] = {
    for (i <- a.indices) yield {
      a(i) + b(i)
    }
  }

//  def evaluate(population:Seq[Seq[Room]], metrics:Seq[(Seq[Room]=>Int,Double)]): Seq[Seq[Int]] = {
//    // produce a list of evaluations according to each metric (3 metrics?  3 lists)
//    metrics.map(m=> population.map(rl => m._1(rl)))
//  }
//
//  def rescaleEvaluation(rawEvaluation:Seq[Seq[Int]], metrics:Seq[(Seq[Room]=>Int,Double)]): Seq[Double] = {
//    // now lets normalize and reweight our metrics.
//    // basically, right now each metric is some integer, but some metrics might be orders of magnitude larger or smaller than others
//    // and that will cause them to disproportionately affect our selection.  So we will normalize this population so the best are '1' and the worst is '0' for each metric
//    // then we can scale each based on how important we want it to be in our final mating evaluation.
//    val weights = metrics.map(m=> m._2)
//    rawEvaluation.zip(weights).map(ev => {
//      // there's probably a cleaner / more idiomatic way to do this.
//      val e = ev._1
//      val w = ev._2
//      e.map(v => {
//        if (e.min != e.max) {
//          ((v - e.min).toDouble / (e.max - e.min).toDouble)  * w
//        } else {
//          1.0 * w
//        }
//      })
//    }).reduce(listAdd)
//  }

  def evaluate(population:Seq[Seq[Room]], metrics:Seq[(Seq[Room]=>Int,Double)]): Seq[Seq[Int]] = {
    // produce a list of evaluations for each population unit.  (pop of 30?  30 lists)
    population.par.map(rl=> metrics.map(m => m._1(rl))).seq
  }

  def rescaleEvaluation(rawEvaluation:Seq[Seq[Int]], metrics:Seq[(Seq[Room]=>Int,Double)]): Seq[Double] = {
    // now lets normalize and reweight our metrics.
    // basically, right now each metric is some integer, but some metrics might be orders of magnitude larger or smaller than others
    // and that will cause them to disproportionately affect our selection.  So we will normalize this population so the best are '1' and the worst is '0' for each metric
    // then we can scale each based on how important we want it to be in our final mating evaluation.
    val weights = metrics.map(m=> m._2)
    rawEvaluation.transpose.zip(weights).map(ev => {
      // there's probably a cleaner / more idiomatic way to do this.
      val e = ev._1
      val w = ev._2
      e.map(v => {
        if (e.min != e.max) {
          ((v - e.min).toDouble / (e.max - e.min).toDouble)  * w
        } else {
          1.0 * w
        }
      })
    }).reduce(listAdd)
  }


  def mutate(population:Seq[Seq[Room]], rate:Int): Seq[Seq[Room]] ={
    // in this case, rate is just the maximum distance we might move a room around.  Likely when we run this we'll start
    // with a high rate and gradually reduce it, simulated annealing style
    def scoot(coords:Coordinates,amount:Int): Coordinates = {
      if (!random.oneIn(10)) return coords
      val deltaX = random.between(-amount, amount + 1)
      val deltaY = random.between(-amount, amount + 1)
      val newX = wrapX(coords.x + deltaX)
      val newY = if (coords.y + deltaY >= SC_vertical) {
        SC_vertical - 1
      } else if (coords.y + deltaY < 0) {
        0
      } else {
        coords.y + deltaY
      }
      Coordinates(newX,newY)
    }
    population.map { individual =>
      individual.map { room =>
        room.copy(coords = scoot(room.coords, rate))
      }
    }
  }

  object Reporter {
    var generation = 0
    var report = List[(Int,PopulationReport)]()
    def setGeneration(g:Int) = {generation = g}
    def addReport(p:PopulationReport) = {report = report ++ List((generation,p))}
  }

  val reporter = Reporter

  def crossover(population:Seq[Seq[Room]], metrics:Seq[(Seq[Room]=>Int,Double)]): Seq[Layout] = {
    // individuals in the population who are more fit are more likely to reproduce, so we need some metrics.
    val evaluations = evaluate(population, metrics)
    val scaledEvaluations = rescaleEvaluation(evaluations, metrics)
    val parents = random.nFrom(population.length/2, scaledEvaluations.zip(population))(_._1)
    for (i <- population.indices) yield {
      reporter.addReport(PopulationReport(population(i),evaluations(i),scaledEvaluations(i)))
    }
    parents.flatMap {
      parent1 => {
        val parent2 = random.chooseFrom(parents.filter(_ ne parent1))(p => p._1)
        mate3(parent1._2, parent2._2)
//        Seq(mate(parent1._2, parent2._2), mate(parent1._2, parent2._2))
      }
    }//.flatten
  }

  def mate3(parent1: Seq[Room], parent2: Seq[Room]): Seq[Seq[Room]] = {
    def makeChild = if (random.nextBoolean()) parent1 else parent2
    Seq(makeChild, makeChild)
  }

  def mate2(parent1: Seq[Room], parent2: Seq[Room]): Seq[Seq[Room]] = {
    def makeChild = {
      val minRooms = math.min(parent1.size, parent2.size)
      val maxRooms = math.max(parent1.size, parent2.size)
      val childNumRooms = random.between(minRooms, maxRooms)
      val remainingParent1Rooms = collection.mutable.ListBuffer.empty[Room]
      remainingParent1Rooms ++= parent1
      val p1coords = remainingParent1Rooms.map(r => r.coords)
      val remainingParent2Rooms = collection.mutable.ListBuffer.empty[Room]
      remainingParent2Rooms ++= parent2.filter(r => !p1coords.contains(r.coords) )
      val childRooms = Seq.fill(childNumRooms) {
        val parentRooms = if (remainingParent1Rooms.isEmpty)
          remainingParent2Rooms
        else if (remainingParent2Rooms.isEmpty)
          remainingParent1Rooms
        else if (random.nextBoolean()) remainingParent1Rooms else remainingParent2Rooms
        val idx = random.between(0, parentRooms.size)
        val room = parentRooms.remove(idx)
        room
      }
      childRooms
    }
    Seq(makeChild, makeChild)
  }

  def mate(parent1:Seq[Room], parent2:Seq[Room]): Seq[Seq[Room]] = {
    val roomtypes = parent1.map(r => r.roomType).distinct
    List(1,2).map(_ => roomtypes.flatMap(rt => {
      if (random.nextBoolean())
        parent1.filter(r=> r.roomType==rt)
      else
        parent2.filter(r=> r.roomType==rt)
      /*
      val numNewRooms = random.between(p1Rooms.length,p2Rooms.length)
      val roomPool = p1Rooms ++ p2Rooms
      random.nOf(numNewRooms,roomPool)
       */
    }))
  }

  def placeRooms(roomTypes: Seq[RoomType]): Seq[Room] = {
    // For each roomtype in the weighting list
    // create a list of some number of room instances between the min quantity and the max quantity (random)
    // assign each one a random position in the vehicle
    roomTypes.flatMap((r: RoomType) => {
      val numRooms = if (r.maxQuantity != r.minQuantity) {
        Random.nextInt(r.maxQuantity - r.minQuantity) + r.minQuantity
      } else {
        r.maxQuantity
      }
      (1 to numRooms).map { _ => {
        Room(r, Coordinates(random.nextInt(SC_horizontal), random.nextInt(SC_vertical)))
      }
      }
    })
  }
  def runGeneration(pop:Seq[Seq[Room]]): Seq[Seq[Room]] = {
    val mutated = mutate(pop,1)
    crossover(mutated, metrics)
  }

  def runGenerations(numGenerations:Int) = {
    var gennum = 0
    var pop = Seq.fill(10)(placeRooms(roomTypes))

    while(gennum < numGenerations ) {
      reporter.setGeneration((gennum))
      val mutated = mutate(pop, 5)
      pop = crossover(pop, metrics)
      gennum = gennum + 1
      print("Generation")
    }
    reporter.report
    pop
  }
}

 */