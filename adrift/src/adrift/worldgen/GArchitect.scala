package adrift.worldgen
import adrift.{CylinderGrid, Grid}

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
  case class HistoricalId(value: Int) extends AnyVal
  case class RoomTypeId(id: Int) extends AnyVal

  trait Gene {
    val id: HistoricalId
  }

  case class RoomGene(
    id: HistoricalId,
    roomType: RoomTypeId,
  ) extends Gene

  case class ConnectionGene(
    id: HistoricalId,

    // Ids of the rooms this gene connects
    a: HistoricalId,
    b: HistoricalId,

    weight: Float,

    enabled: Boolean,
  ) extends Gene

  case class Population(s: Seq[Species])
  case class Species(representative: Genome, members: Seq[Genome])

  def newPopulation(num:Int, speciationDelta: Int = 2): Population = {
    val individuals = Seq.fill(num)(newGenome())
    def assignSpecies(): Seq[Species] = {Seq.empty}
    // if no species exists, create one with the first genome.
    // go through genomes
    // compare delta between current genome and current species representative.
    // if within threshold, assign genome to species.
    // if not, create new species and assign this genome as representative.

    // Then go through all species
    // evaluate all members and assign the highest evaluated member as the representative.
    Population(assignSpecies())
  }


  class GAContext() {
    var historicalId = 0
    def newId(): HistoricalId = {
      val id = HistoricalId(historicalId)
      historicalId += 1
      id
    }
    var species: Seq[Species] = Seq.empty
  }

  case class Genome(
    rooms: Seq[RoomGene],
    connections: Seq[ConnectionGene],
    mutationRate: Int = 3,
    context: GAContext
  ) {
    def evaluate(): Double = {
      // Several evaluation metrics are important.
      // We should check and penalize if a genome doesn't have correct room quantities.
      val roomTypeCoefficient = 100d
      RoomType.all.map(rt => {
        val relevantRooms = rooms.count(r => RoomType.byId(r.roomType) == rt)
        if (relevantRooms > rt.maxQuantity) {
          relevantRooms - rt.maxQuantity
        } else if (relevantRooms < rt.minQuantity) {
          rt.minQuantity - relevantRooms
        } else {
          0
        }
      }).sum * -1 * roomTypeCoefficient
      // we should check room affinities, probably - reward rooms for being close to / having tight edge weights to rooms they 'want' to be near (as we determine it)
    }
    //lazy val adjacency = ??? /* ... lazily compute adjacency matrix ... */
    def mutateAddConnection(idGen: () => HistoricalId)(implicit random: Random): Genome = {
      val roomA = random.pick(rooms)
      val roomB = random.pick(rooms)
      if (roomA.id == roomB.id) return this
      val (rA, rB) = if (roomA.id.value < roomB.id.value) (roomA, roomB) else (roomB, roomA)
      if (connections.exists(g => g.a == rA.id && g.b == rB.id)) return this
      val newConnection: ConnectionGene = ConnectionGene(
        id = idGen(),
//        id = connectionIdForRooms(rA.id, rB.id),
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

    def mutateAddRoom(idGen: () => HistoricalId)(implicit random: Random): Genome = {
      val newRoomTypeId = random.pick(RoomType.byId.keys)
      val newRoomType = RoomType.byId(newRoomTypeId)
      val existingCount = rooms.count(_.roomType == newRoomTypeId)
      if (existingCount >= newRoomType.maxQuantity) return this

      val roomGene = RoomGene(id = idGen(), newRoomTypeId)

      val otherRooms = random.nOf(random.between(1, 3), rooms).distinct

      val connectionGenes = otherRooms.map { r =>
        ConnectionGene(
          id = idGen(),
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
      ((g: Genome, r: Random) => g.mutateAddRoom(context.newId)(r), 1f),
      ((g: Genome, r: Random) => g.mutateEnableConnection()(r), 1f),
      ((g: Genome, r: Random) => g.mutateDisableConnection()(r), 1f),
      ((g: Genome, r: Random) => g.mutateAddConnection(context.newId)(r), 1f),
      ((g: Genome, r: Random) => g.mutateConnectionWeight()(r), 1f),
      ((g: Genome, r: Random) => g.mutateMutationRate()(r), 1f),
    )

    def mutate()(implicit random: Random): Genome = {
      val mutations = random.nFrom(random.between(0, mutationRate), mutationFunctions)(_._2).map(_._1)
      mutations.foldLeft(this)((genome, mutate) => mutate(genome, random))
    }

    def delta(other:Genome): Double = {
      // NEAT paper says that they used disjoint / excess coefficient of 1
      def disjointCoefficient = 1
      // NEAT paper says that they used weight coefficient of .4 for some problems and 3 for others that are more 'sensitive'
      def weightCoefficient = .4
      val localSize = rooms.size + connections.size
      val otherSize = other.rooms.size + other.connections.size
      val localGeneIds: Set[HistoricalId] = (rooms.map(_.id) ++ connections.map(_.id)).toSet
      val otherGeneIds: Set[HistoricalId] = (other.rooms.map(_.id) ++ other.connections.map(_.id)).toSet
      val disjointGeneIds = localGeneIds.diff(otherGeneIds).union(otherGeneIds.diff(localGeneIds))
      val disjointGeneCount = disjointGeneIds.size
      def weightDelta(other: Genome): Double= {
        val localConnectionIds = connections.map(_.id)
        val otherConnectionIds = other.connections.map(_.id)
        val intersectingIds = localConnectionIds.intersect(otherConnectionIds)
        val intersectingWeights = connections.filter(c => intersectingIds.contains(c.id)).map(g => {
          math.abs(other.connections.find(p => p.id == g.id).get.weight - g.weight)
        }).sum
        intersectingWeights/intersectingIds.length.toDouble
      }
      weightDelta(other) * weightCoefficient + disjointGeneCount * disjointCoefficient / Seq(localSize, otherSize).max.toDouble
    }

    def crossover(other: Genome): Genome = ???
  }

  def newGenome(numForeAft: Int = 3): Genome = {
    val context = new GAContext()
    def idGen = context.newId()
    val foreRoomTypeid = RoomType.all.indexWhere(r => r.name == "fore")
    val aftRoomTypeid = RoomType.all.indexWhere(r => r.name == "aft")
    val roomGenes: Seq[RoomGene] =
      Seq.fill(numForeAft)(RoomGene(idGen, RoomTypeId(foreRoomTypeid))) ++ Seq.fill(numForeAft)(RoomGene(idGen, RoomTypeId(aftRoomTypeid)))

    val connections = mutable.Buffer.empty[ConnectionGene]

    // Connect the Fore 'rooms' in a ring
    for (i <- 0 until numForeAft - 1) {
      connections += ConnectionGene(idGen, HistoricalId(i), HistoricalId(i+1), 1, enabled = true)
    }
    connections += ConnectionGene(idGen, HistoricalId(0), HistoricalId(numForeAft - 1), 1, enabled = true)

    // Connect the Aft 'rooms' in a ring
    for (i <- numForeAft until numForeAft * 2 - 1) {
      connections += ConnectionGene(idGen, HistoricalId(i), HistoricalId(i+1), 1, enabled = true)
    }
    connections += ConnectionGene(idGen, HistoricalId(numForeAft), HistoricalId(numForeAft * 2 - 1), 1, enabled = true)

    // Connect each Fore room to its corresponding Aft room.
    for (i <- 0 until numForeAft) {
      connections += ConnectionGene(idGen, HistoricalId(i), HistoricalId(i + numForeAft), 1, enabled = true)
    }
    val mutationRate = 3
    Genome(roomGenes, connections, mutationRate, context)
  }
  def newPopulation(num: Int): Seq[Genome]
  case class Rect(t: Int, r: Int, b: Int, l: Int)
  case class RoomLayout(roomCenters: Map[HistoricalId, (Double, Double)], roomRects: Map[HistoricalId, Rect]) {}

  def layout(g: Genome, iterationLimit: Int = 50, growthIterationLimit: Int = Int.MaxValue)(implicit random: Random): RoomLayout = {
    val sm = new StressMajorization()
    val idxToRoomId: Map[Int, HistoricalId] = g.rooms.zipWithIndex.map { case (r, i) => i -> r.id }(collection.breakOut)
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
      neighbors = neighbs,
      iterationLimit,
      epsilon = 0.001,
      desiredEdgeLength = (u, v) => lengths(if (u < v) (u, v) else (v, u)),
      initialPosition = _ => (random.between(0d, 1000d), random.between(0d, 1000d)),
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
    val roomCenters: Map[HistoricalId, (Double, Double)] = g.rooms.indices.map({ i =>
      g.rooms(i).id -> normalizePosition(sm.position(i))
    })(collection.breakOut)


    // Room growth, inspired by A Constrained Growth Method for Procedural Floor Plan Generation
    // R. Lopes, T. Tutenel, R. M. Smelik,  K. J. de Kraker, R. Bidarra
    // http://graphics.tudelft.nl/~rafa/myPapers/bidarra.GAMEON10.pdf
    // See also,
    // A Survey on the Procedural Generation of Virtual Worlds, J. Freiknecht, W. Effelsberg
    // https://www.researchgate.net/publication/320722498_A_Survey_on_the_Procedural_Generation_of_Virtual_Worlds
    val roomGrid = new CylinderGrid[Option[HistoricalId]](1000, 1000)(None)

    @scala.annotation.tailrec
    def findNearbyEmpty(x: Int, y: Int): (Int, Int) = {
      if (y < 0) findNearbyEmpty(x, 0)
      else if (y >= roomGrid.height) findNearbyEmpty(x, roomGrid.height - 1)
      else {
        if (roomGrid(x, y).isEmpty)
          (x, y)
        else {
          if (random.nextBoolean())
            findNearbyEmpty(x + random.oneOf(1, -1), y)
          else
            findNearbyEmpty(x, y + random.oneOf(1, -1))
        }
      }
    }

    // Initialize rooms
    val roomRects = mutable.Map.empty[HistoricalId, Rect]
    roomCenters.foreach {
      case (id, pos) =>
        val gridPos = findNearbyEmpty(pos._1.round.toInt, pos._2.round.toInt)
        roomRects += (id -> Rect(gridPos._2, gridPos._1 + 1, gridPos._2 + 1, gridPos._1))
        roomGrid(gridPos) = Some(id)
    }

    // grow
    def allEmpty(x1: Int, y1: Int, x2: Int, y2: Int): Boolean = {
      for (x <- x1 until x2; y <- y1 until y2) { if (roomGrid(x, y).nonEmpty) return false }
      true
    }
    def fill(x1: Int, y1: Int, x2: Int, y2: Int, value: Option[HistoricalId]): Unit =
      for (x <- x1 until x2; y <- y1 until y2) roomGrid(x, y) = value
    def isGrowable(rect: Rect, dir: Int): Boolean = {
      dir match {
        case 0 => // t
          rect.t > 0 && allEmpty(rect.l, rect.t - 1, rect.r, rect.t)
        case 1 => // r
          allEmpty(rect.r, rect.t, rect.r + 1, rect.b)
        case 2 => // b
          rect.b < roomGrid.height - 1 && allEmpty(rect.l, rect.b, rect.r, rect.b + 1)
        case 3 => // l
          allEmpty(rect.l - 1, rect.t, rect.l, rect.b)
      }
    }
    def isGrowableAnyDir(rect: Rect): Boolean = (0 until 4).exists(isGrowable(rect, _))
    def findGrowableRoom(): Option[HistoricalId] = {
      random.maybePick(roomRects.keys.view.filter { k => isGrowableAnyDir(roomRects(k)) })
    }

    def growRoomInDir(roomId: HistoricalId, dir: Int): Unit = {
      val rect = roomRects(roomId)
      dir match {
        case 0 => // t
          fill(rect.l, rect.t - 1, rect.r, rect.t, Some(roomId))
          roomRects(roomId) = rect.copy(t = rect.t - 1)
        case 1 => // r
          fill(rect.r, rect.t, rect.r + 1, rect.b, Some(roomId))
          roomRects(roomId) = rect.copy(r = rect.r + 1)
        case 2 => // b
          fill(rect.l, rect.b, rect.r, rect.b + 1, Some(roomId))
          roomRects(roomId) = rect.copy(b = rect.b + 1)
        case 3 => // l
          fill(rect.l - 1, rect.t, rect.l, rect.b, Some(roomId))
          roomRects(roomId) = rect.copy(l = rect.l - 1)
      }
    }
    def growRoom(roomId: HistoricalId): Unit = {
      val rect = roomRects(roomId)
      val dir = random.pick((0 until 4).filter(isGrowable(rect, _)))
      growRoomInDir(roomId, dir)
    }

    var i = 0
    var done = false
    while (!done && i < growthIterationLimit) {
      // find a room that can grow
      findGrowableRoom() match {
        case Some(roomId) =>
          growRoom(roomId)
        case None =>
          done = true
      }
      i += 1
    }


    RoomLayout(roomCenters, roomRects.toMap)
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

    RoomType("fore", spaceWeight = 100, minQuantity = 1, maxQuantity = 1),
    RoomType("aft", spaceWeight = 100, minQuantity = 1, maxQuantity = 1),
  )

  val byId: Map[RoomTypeId, RoomType] = all.zipWithIndex.map {
    case (rt, id) => RoomTypeId(id) -> rt
  }(collection.breakOut)
}
case class Coordinates(x:Int,y:Int)
case class Room(roomType: RoomTypeId, coords:Coordinates) {}

