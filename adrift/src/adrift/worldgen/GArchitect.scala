package adrift.worldgen
import adrift.{CylinderGrid, Grid}

import util.Random
import adrift.RandomImplicits._
import adrift.worldgen.NEATArchitect.RoomTypeId
import adrift.worldgen.RoomType.RoomType

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

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


  // General GA process:
  // create population
  // Repeate:
  //   mutate
  //   evaluate
  //   kill
  //   mate & [speciate]

  def runGeneration(pNew:Population): Unit = {
    val pMutated = pNew.mutate()
    val evaluations = pMutated.evaluate()
    pMutated.mate(evaluations)
  }

  case class Species(representative: Genome)
  case class Population(species: Seq[Species], members: Seq[Genome], speciationDelta: Double)(implicit random: Random) {

    def mutate(): Population = copy(species, members = members.map(_.mutate()), speciationDelta)

    def evaluate(): Map[Genome, Double] = members.zip(members.map(_.evaluate())).toMap

    def mate(evaluations: Map[Genome, Double]): Population = {
      // allocate fitness to species, kill off individuals and mate to produce new ones, then pick representatives for new species.
      val popSize = members.length

      // allocate fitness to species
      val totalFitness = evaluations.values.sum
      val speciesMembers = species.zip(species.map(s => members.filter(_.delta(s.representative) < speciationDelta))).toMap

      val speciesFitness = species.zip(species.map(s => {
        speciesMembers(s).map(g => evaluations(g)).sum
      })).toMap
      val newSpeciesSizes = species.zip(species.map(s => (speciesFitness(s) / totalFitness * popSize).round.toInt)).toMap

      // produce new individuals
      def speciesMate(species: Species): Genome = {
        val potentialParents = speciesMembers(species)
        if (potentialParents.length < 2) return species.representative
        val parent1 = random.chooseFrom(potentialParents)(evaluations(_))
        val parent2 = random.chooseFrom(potentialParents.dropWhile(_ == parent1))(evaluations(_))
        if (evaluations(parent1) > evaluations(parent2)) {
          parent1.crossover(parent2)
        } else {
          parent2.crossover(parent1)
        }
      }
      val newIndividuals = species.flatMap(s => {
        Seq.fill(newSpeciesSizes(s))(speciesMate(s))
      })
      // make new species for those individuals, trying to keep them as close to the old ones as possible, maybe?
      val representatives: Seq[Genome] = species.dropWhile(newSpeciesSizes(_)<1).map(_.representative)
      val newSpecies = representatives.map(rep => Species(newIndividuals.sortBy(g => g.delta(rep)).head))
      Population(newSpecies, newIndividuals, speciationDelta)
    }
  }

  def newPopulation(num:Int, speciationDelta: Double = 3d)(implicit random: Random): Population = {
    // Other implementations use a speciation threshold of 2-10 depending on the problem and ??? This is a guess.
    val individuals = Seq.fill(num)(newGenome())
    Population(Seq(Species(individuals.head)), individuals, speciationDelta)
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
      // This evaluate might create a room layout and perform evaluations on that layout.
      // Several evaluation metrics are important.
      // We should check and penalize if a genome doesn't have correct room quantities.
      val roomTypeCoefficient = 1d
      val rtEval = RoomType.all.map(rt => {
        val relevantRooms = rooms.count(r => RoomType.byId(r.roomType) == rt).toDouble
        if (relevantRooms > rt.maxQuantity) {
          rt.maxQuantity.toDouble/relevantRooms
        } else if (relevantRooms < rt.minQuantity) {
          relevantRooms/rt.minQuantity.toDouble
        } else {
          1
        }
      }).sum * roomTypeCoefficient
      // we should check room affinities, probably - reward rooms for being close to / having tight edge weights to rooms they 'want' to be near (as we determine it)
      // val affiinityEval = ???
      Math.max(0,rtEval)
    }
    val roomIdMap: Map[HistoricalId, RoomGene] = rooms.map(r => r.id).zip(rooms).toMap
    val connectionIdMap: Map[HistoricalId, ConnectionGene] = connections.map(c => c.id).zip(connections).toMap

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
      // in cannonical NEAT this has some chance to become a new random value, and some other chance to bump up / down slightly
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

    def crossover(other: Genome)(implicit random: Random): Genome = {
      // all genes with the same historicalID are conserved, with the value coming randomly from either parent.
      // all disjoint genes from more fit parent are populated.  no disjoint genes from less fit parent are populated.
      // Assume that the less fit parent is passed in as 'other'
      // this means that the child is basically this genome, but with 'shared' genes having weights overwritten occasionally by the 'other'
      val localRooms = rooms.map(_.id)
      val localConnections = connections.map(_.id)
      val otherRooms = other.rooms.map(_.id)
      val otherConnections = other.connections.map(_.id)
      val commonRooms = localRooms.intersect(otherRooms)
      val commonConnections = localConnections.intersect(otherConnections)

      val newRooms: Seq[RoomGene] = localRooms.map(r => {
        if (commonRooms.contains(r)) {
          if (random.nextBoolean()) {roomIdMap(r)} else {other.roomIdMap(r)}
        } else {
          roomIdMap(r)
        }
      })
      val newConnections: Seq[ConnectionGene] = localConnections.map(c => {
        if (commonConnections.contains(c)) {
          if (random.nextBoolean()) {connectionIdMap(c)} else {other.connectionIdMap(c)}
        } else {
          connectionIdMap(c)
        }
      })
      Genome(newRooms, newConnections, mutationRate, context)
    }
  }

  def newGenome(numForeAft: Int = 3): Genome = {
    val context = new GAContext()
    def idGen = context.newId()
    val foreRoomTypeId = RoomType.byName("fore")
    val aftRoomTypeId = RoomType.byName("aft")
    val roomGenes: Seq[RoomGene] =
      Seq.fill(numForeAft)(RoomGene(idGen, foreRoomTypeId)) ++ Seq.fill(numForeAft)(RoomGene(idGen, aftRoomTypeId))

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
  def newPopulation(num: Int): Seq[Genome] = ???

  case class Rect(t: Int, r: Int, b: Int, l: Int) {
    require(r > l)
    require(b > t)
    def area: Int = (r - l) * (b - t)
  }
  case class RoomLayout(
    roomCenters: Map[HistoricalId, (Double, Double)],
    roomRects: Map[HistoricalId, Rect],
    roomGrid: CylinderGrid[Option[HistoricalId]]
  )

  def layout(
    g: Genome,
    iterationLimit: Int = 50,
    growthIterationLimit: Int = Int.MaxValue
  )(implicit random: Random): RoomLayout = {
    val cylinderCircumference = 360
    val cylinderLength = 270

    val foreRoomTypeId = RoomType.byName("fore")
    val aftRoomTypeId = RoomType.byName("aft")
    val foreRooms = g.rooms.filter(_.roomType == foreRoomTypeId).sortBy(_.id.value)
    val aftRooms = g.rooms.filter(_.roomType == aftRoomTypeId).sortBy(_.id.value)
    // arrange the fore rooms in a circle around the top, and the aft in a circle around the bottom
    val forePositions = foreRooms.indices.map { i => (cylinderCircumference.toDouble / foreRooms.size * i, 0d) }
    val aftPositions = aftRooms.indices.map { i => (cylinderCircumference.toDouble / foreRooms.size * i, cylinderLength.toDouble) }
    val fixedPositionRooms: Map[HistoricalId, (Double, Double)] =
      foreRooms.zip(forePositions).map { case (r, p) => r.id -> p }.toMap ++
        aftRooms.zip(aftPositions).map { case (r, p) => r.id -> p }

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
    def normalizeX(x: Double): Double =
      ((x % cylinderCircumference) + cylinderCircumference) % cylinderCircumference
    def normalizeXI(x: Int): Int =
      ((x % cylinderCircumference) + cylinderCircumference) % cylinderCircumference
    def normalizePosition(p: (Double, Double)): (Double, Double) = (normalizeX(p._1), p._2)
    sm.initialize(
      g.rooms.size,
      neighbors = neighbs,
      iterationLimit,
      epsilon = 0.001,
      desiredEdgeLength = (u, v) => lengths(if (u < v) (u, v) else (v, u)),
      initialPosition = i => {
        val roomId = idxToRoomId(i)
        fixedPositionRooms.getOrElse(roomId, (random.between(0d, cylinderCircumference), random.between(0d, cylinderLength)))
      },
      isFixedPosition = u => fixedPositionRooms.contains(idxToRoomId(u)),
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
    val roomGrid = new CylinderGrid[Option[HistoricalId]](cylinderCircumference, cylinderLength)(None)

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

    def seek(s0: Int, d: Int, f: Int => Boolean): Int = {
      var s = s0
      while (f(s))
        s += d
      s - d
    }

    def findBigEmptyRect(cell: (Int, Int)): Rect = {
      def isEmpty(x: Int, y: Int): Boolean = roomGrid.contains(x, y) && roomGrid(x, y).isEmpty
      def isEmptyRow(l: Int, r: Int)(y: Int): Boolean = allEmpty(l, y, r, y + 1)
      def isEmptyCol(t: Int, b: Int)(x: Int): Boolean = allEmpty(x, t, x + 1, b)
      val (cx, cy) = cell
      // find the longest horizontal line of empty cells which includes this cell
      val l = seek(cx, -1, isEmpty(_, cy))
      val r = seek(cx, 1, isEmpty(_, cy)) + 1
      // expand the horizontal line upwards and downwards as far as possible to make a rect
      val rt = seek(cy, -1, isEmptyRow(l, r))
      val rb = seek(cy, 1, isEmptyRow(l, r)) + 1
      val r1 = Rect(rt, r, rb, l)

      // same but vertical
      val t = seek(cy, -1, isEmpty(cx, _))
      val b = seek(cy, 1, isEmpty(cx, _)) + 1
      val rl = seek(cx, -1, isEmptyCol(t, b))
      val rr = seek(cx, 1, isEmptyCol(t, b)) + 1
      val r2 = Rect(t, rr, b, rl)

      // take the bigger one
      if (r1.area > r2.area) r1 else r2
    }

    // fill in the gaps
    val emptyCells = mutable.Set.empty[(Int, Int)]
    emptyCells ++= roomGrid.indices.filter(p => roomGrid(p).isEmpty)
    val adjs = Seq((-1, 0), (0, -1), (1, 0), (0, 1))
    while (emptyCells.nonEmpty && i < growthIterationLimit) {
      val cell = emptyCells.find {
        c => adjs.exists { d => roomGrid.get(c._1 + d._1, c._2 + d._2).exists(_.nonEmpty) }
      }.getOrElse(throw new RuntimeException("How can there be no empty cell that's adjacent to something??"))
      assert(roomGrid(cell).isEmpty)

      val adjCell = adjs.find(a => roomGrid.get(cell._1 + a._1, cell._2 + a._2).exists(_.nonEmpty)).get
      val adjVal = roomGrid(cell._1 + adjCell._1, cell._2 + adjCell._2)
      val rect = findBigEmptyRect(cell)
      fill(rect.l, rect.t, rect.r, rect.b, adjVal)
      for (x <- rect.l until rect.r; y <- rect.t until rect.b) emptyCells.remove((normalizeXI(x), y))

      i += 1
    }


    RoomLayout(roomCenters, roomRects.toMap, roomGrid)
  }

  def make()(implicit random: Random): RoomLayout = {
    // TODO: like... run the GA
    layout(newGenome())
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

  val byName: Map[String, RoomTypeId] = byId.map { case (k, v) => v.name -> k }
}
case class Coordinates(x:Int,y:Int)
case class Room(roomType: RoomTypeId, coords:Coordinates) {}

