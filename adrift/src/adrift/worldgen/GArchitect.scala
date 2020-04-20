package adrift.worldgen
import adrift.CylinderGrid
import adrift.RandomImplicits._
import adrift.worldgen.NEATArchitect.RoomTypeId

import scala.collection.mutable
import scala.util.Random

object NEATArchitect {
  case class HistoricalId(value: Int) extends AnyVal
  case class RoomTypeId(id: Int) extends AnyVal

  case class RoomGene(
    id: HistoricalId,
    roomType: RoomTypeId,
  )

  case class ConnectionGene(
    id: HistoricalId,

    // Ids of the rooms this gene connects
    a: HistoricalId,
    b: HistoricalId,

    weight: Double,

    enabled: Boolean,
  ) {
    require(a != b, "can't connect a room to itself")
    require(a.value < b.value, "edges must always be constructed with the lower id first")
  }

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

  // General GA process:
  // create population
  // Repeate:
  //   mutate
  //   evaluate
  //   kill
  //   mate & [speciate]

  def runGeneration(pop: Population, targetPopSize: Int)(implicit random: Random): Population = {
    val ret = pop.mate(targetPopSize)
    println(s"#${ret.generationNumber} Best fitness: ${ret.best.fitness} / pop size: ${ret.members.size} / ${ret.species.size} species")
    ret
  }

  def runGenerations(initial: Population, numGenerations: Int)(implicit random: Random): Population =
    Iterable.iterate(initial, numGenerations)(runGeneration(_, initial.members.size)).last

  def make()(implicit random: Random): RoomLayout = {
    val winner = runGenerations(newPopulation(10), 10).best
    layout(winner)
  }

  case class Species(representative: Genome)
  case class Population(
    species: Seq[Species],
    members: Seq[Genome],
    speciationDelta: Double,
    generationNumber: Int,
    nextId: Int
  ) {
    {
      val maxId = members.flatMap(m => m.rooms.view.map(_.id.value) ++ m.connections.view.map(_.id.value)).max
      assert(nextId > maxId, "next id should definitely be higher than the max id in the genes present")
    }

    {
      // every member of the population should belong to a species
      assert(members.forall(m => species.exists(_.representative.delta(m) < speciationDelta)))
    }

    def best: Genome = members.maxBy(_.fitness)

    def mate(targetPopSize: Int)(implicit random: Random): Population = {
      var next = nextId
      def newId: HistoricalId = { next += 1; HistoricalId(next - 1) }

      // allocate fitness to species
      val totalFitness = members.map(_.fitness).sum

      def findSpeciesOf(genome: NEATArchitect.Genome): Species =
        species.minByOption(_.representative.delta(genome)).filter(_.representative.delta(genome) < speciationDelta)
          .getOrElse(throw new Exception(s"member of no species??? closest was ${species.view.map(_.representative.delta(genome)).min}"))

      val speciesMembers = members.map { m => (m, findSpeciesOf(m)) }.groupMap(_._2)(_._1)

      val speciesFitness = species.map { s => s -> speciesMembers(s).view.map(_.fitness).sum }.toMap
      val newSpeciesSizes = species.map { s => s -> (speciesFitness(s) / totalFitness * targetPopSize).round.toInt }.toMap

      // produce new individuals
      def speciesMate(species: Species): Genome = {
        val potentialParents = speciesMembers(species)
        if (potentialParents.length < 2) return species.representative
        val parent1 = random.chooseFrom(potentialParents)(_.fitness)
        val parent2 = random.chooseFrom(potentialParents.filterNot(_ eq parent1))(_.fitness)
        if (parent1.fitness > parent2.fitness) {
          parent1.crossover(parent2)
        } else {
          parent2.crossover(parent1)
        }.mutate(newId)
      }

      val nonEmptySpecies = species.filter(newSpeciesSizes(_) > 0)

      val newIndividuals = nonEmptySpecies.flatMap(s => {
        Seq.fill(newSpeciesSizes(s))(speciesMate(s))
      })

      val newSpecies = mutable.Buffer.empty[Species]

      // Create initial species in the new generation by matching new individuals to the previous species
      newSpecies ++= nonEmptySpecies.map(s => Species(newIndividuals.minBy(_.delta(s.representative))))

      // Ensure that there's a species for each individual in the new generation, by creating a new species for any
      // individual which doesn't closely match an existing species.
      for (individual <- newIndividuals) {
        val acceptableSpecies = newSpecies.find(s => individual.delta(s.representative) < speciationDelta)
        if (acceptableSpecies.isEmpty) {
          // add new species
          newSpecies += Species(individual)
        }
      }

      copy(
        species = newSpecies.to(Seq),
        members = newIndividuals,
        generationNumber = generationNumber + 1,
        nextId = next
      )
    }
  }

  def newPopulation(num:Int, speciationDelta: Double = 0.5d)(implicit random: Random): Population = {
    // Other implementations use a speciation threshold of 2-10 depending on the problem and ??? This is a guess.
    var nextId = 0
    def newId: HistoricalId = { nextId += 1; HistoricalId(nextId - 1) }
    val g = newGenome(newId)
    val individuals = Seq.fill(num)(g.copy())
    // All the initial genomes are the same, so there's only one species.
    val species = Seq(Species(individuals.head))
    Population(species, individuals, speciationDelta, generationNumber = 0, nextId = nextId)
  }

  case class Genome(
    rooms: Seq[RoomGene],
    connections: Seq[ConnectionGene],
    mutationRate: Int = 3,
    seed: Int = 42
  ) {
    def asDot: String =
      s"graph {\n${rooms.map(_.id.value.toString).mkString(";\n")};\n${connections.map(c => s"${c.a.value} -- ${c.b.value}").mkString(";\n")};\n}"

    {
      val allIds = rooms.view.map(_.id) ++ connections.view.map(_.id)
      require(allIds.size == allIds.toSet.size, s"duplicate id(s): ${allIds.groupBy(identity).view.filter(_._2.size > 1).keys.mkString(", ")}")
    }
    lazy val fitness: Double = {
      // This evaluate might create a room layout and perform evaluations on that layout.
      // Several evaluation metrics are important.
      // We should check and penalize if a genome doesn't have correct room quantities.
      val roomTypeCoefficient = 1d
      val disallowed = Set("fore", "aft")
      val rtEval = RoomType.all.filterNot(rt => disallowed contains rt.name).map(rt => {
        val relevantRooms = rooms.count(r => RoomType.byId(r.roomType) == rt).toDouble
        if (relevantRooms > rt.maxQuantity) {
          rt.maxQuantity.toDouble/relevantRooms
        } else if (relevantRooms < rt.minQuantity) {
          relevantRooms/rt.minQuantity.toDouble
        } else {
          1
        }
      }).sum * roomTypeCoefficient

      val roomLayout = layout(this)(new Random(seed))
      val totalSpace = roomLayout.roomGrid.width * roomLayout.roomGrid.height
      val totalSpaceWeight = RoomType.all.map(_.spaceWeight).sum
      val idealSpaceProportionPerRoomType = RoomType.byId.view.mapValues(_.spaceWeight / totalSpaceWeight)
      val gridCellsPerRoomType: Map[RoomTypeId, Int] =
        roomLayout.roomGrid.indices.foldLeft(Map.empty[RoomTypeId, Int].withDefaultValue(0)) { (counts, idx) =>
          val roomId = roomLayout.roomGrid(idx).get
          val roomTypeId = rooms.find(_.id == roomId).get.roomType
          counts + (roomTypeId -> (counts(roomTypeId) + 1))
        }
      val actualSpaceProportionPerRoomType: Map[RoomTypeId, Double] = gridCellsPerRoomType.view.mapValues(_.toDouble / totalSpace).to(Map)
      val distanceFromIdeal: Double = RoomType.byId.keys.map { k =>
        math.abs(actualSpaceProportionPerRoomType.getOrElse(k, 0d) - idealSpaceProportionPerRoomType(k))
      }.sum

      val spaceWeightCoefficient = 1d
      // TODO: Why 2?
      val spaceWeightFactor = spaceWeightCoefficient * (2 - distanceFromIdeal)

      // we should check room affinities, probably - reward rooms for being close to / having tight edge weights to rooms they 'want' to be near (as we determine it)
      // val affiinityEval = ???
      math.max(0, rtEval) + math.max(0, spaceWeightFactor)
    }
    val roomIdMap: Map[HistoricalId, RoomGene] = rooms.map(r => r.id).zip(rooms).toMap
    val connectionIdMap: Map[HistoricalId, ConnectionGene] = connections.map(c => c.id).zip(connections).toMap

    def mutateAddConnection(idGen: => HistoricalId)(implicit random: Random): Genome = {
      val roomA = random.pick(rooms)
      val roomB = random.pick(rooms)
      if (roomA.id == roomB.id) return this
      val (rA, rB) = if (roomA.id.value < roomB.id.value) (roomA, roomB) else (roomB, roomA)
      if (connections.exists(g => g.a == rA.id && g.b == rB.id)) return this
      val newConnection: ConnectionGene = ConnectionGene(
        id = idGen,
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

    def mutateAddRoom(nextId: => HistoricalId)(implicit random: Random): Genome = {
      val newRoomTypeId = random.pick(RoomType.byId.keys)
      val newRoomType = RoomType.byId(newRoomTypeId)
      val existingCount = rooms.count(_.roomType == newRoomTypeId)
      if (existingCount >= newRoomType.maxQuantity) return this

      val roomGene = RoomGene(id = nextId, newRoomTypeId)

      val otherRooms = random.nOf(random.between(1, 3), rooms).distinct

      val connectionGenes = otherRooms.map { r =>
        val (a, b) = if (r.id.value < roomGene.id.value) (r, roomGene) else (roomGene, r)
        ConnectionGene(
          id = nextId,
          a = a.id,
          b = b.id,
          weight = 1,
          enabled = true
        )
      }

      copy(rooms = rooms :+ roomGene, connections = connections ++ connectionGenes)
    }

    def mutateMutationRate()(implicit random: Random): Genome =
      copy(mutationRate = math.max(1, mutationRate + random.between(-1, 1)))

    val mutationFunctions = Seq(
      ((g: Genome, r: Random, newId: () => HistoricalId) => g.mutateAddRoom(newId())(r), 1f),
      ((g: Genome, r: Random, newId: () => HistoricalId) => g.mutateEnableConnection()(r), 1f),
      ((g: Genome, r: Random, newId: () => HistoricalId) => g.mutateDisableConnection()(r), 1f),
      ((g: Genome, r: Random, newId: () => HistoricalId) => g.mutateAddConnection(newId())(r), 1f),
      ((g: Genome, r: Random, newId: () => HistoricalId) => g.mutateConnectionWeight()(r), 1f),
      ((g: Genome, r: Random, newId: () => HistoricalId) => g.mutateMutationRate()(r), 1f),
    )

    def mutate(newId: => HistoricalId)(implicit random: Random): Genome = {
      val mutations = random.nFrom(random.between(0, mutationRate), mutationFunctions)(_._2).map(_._1)
      mutations.foldLeft(this)((genome, mutate) => mutate(genome, random, newId _))
    }

    def delta(other: Genome): Double = {
      // NEAT paper says that they used disjoint / excess coefficient of 1
      def disjointCoefficient = 1d
      // NEAT paper says that they used weight coefficient of .4 for some problems and 3 for others that are more 'sensitive'
      def weightCoefficient = .4d
      val localSize = rooms.size + connections.size
      val otherSize = other.rooms.size + other.connections.size
      val localGeneIds: Set[HistoricalId] = (rooms.map(_.id) ++ connections.map(_.id)).toSet
      val otherGeneIds: Set[HistoricalId] = (other.rooms.map(_.id) ++ other.connections.map(_.id)).toSet
      val disjointGeneIds = localGeneIds.diff(otherGeneIds).union(otherGeneIds.diff(localGeneIds))
      val disjointGeneCount = disjointGeneIds.size
      def weightDelta(other: Genome): Double = {
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
      copy(rooms = newRooms, connections = newConnections)
    }
  }

  def newGenome(newId: => HistoricalId, numForeAft: Int = 3): Genome = {
    val foreRoomTypeId = RoomType.byName("fore")
    val aftRoomTypeId = RoomType.byName("aft")
    val roomGenes: Seq[RoomGene] =
      Seq.fill(numForeAft)(RoomGene(newId, foreRoomTypeId)) ++ Seq.fill(numForeAft)(RoomGene(newId, aftRoomTypeId))

    val connections = mutable.Buffer.empty[ConnectionGene]

    // Connect the Fore 'rooms' in a ring
    for (i <- 0 until numForeAft - 1) {
      connections += ConnectionGene(newId, HistoricalId(i), HistoricalId(i+1), 1, enabled = true)
    }
    connections += ConnectionGene(newId, HistoricalId(0), HistoricalId(numForeAft - 1), 1, enabled = true)

    // Connect the Aft 'rooms' in a ring
    for (i <- numForeAft until numForeAft * 2 - 1) {
      connections += ConnectionGene(newId, HistoricalId(i), HistoricalId(i+1), 1, enabled = true)
    }
    connections += ConnectionGene(newId, HistoricalId(numForeAft), HistoricalId(numForeAft * 2 - 1), 1, enabled = true)

    // Connect each Fore room to its corresponding Aft room.
    for (i <- 0 until numForeAft) {
      connections += ConnectionGene(newId, HistoricalId(i), HistoricalId(i + numForeAft), 1, enabled = true)
    }
    val mutationRate = 3
    Genome(roomGenes, connections.to(Seq), mutationRate)
  }

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
    val idxToRoomId: Map[Int, HistoricalId] = g.rooms.view.zipWithIndex.map { case (r, i) => i -> r.id }.to(Map)
    val roomIdToIdx: Map[HistoricalId, Int] = idxToRoomId.map { case (k, v) => v -> k }
    def neighbs(i: Int): IterableOnce[Int] = {
      val needle = g.rooms(i).id
      g.connections.flatMap {
        case c: ConnectionGene if c.enabled && c.a == needle => Some(roomIdToIdx(c.b))
        case c: ConnectionGene if c.enabled && c.b == needle => Some(roomIdToIdx(c.a))
        case _ => None
      }
    }
    val lengths: Map[(Int, Int), Double] = g.connections.view.map { c =>
      val idxA = roomIdToIdx(c.a)
      val idxB = roomIdToIdx(c.b)
      assert(idxA < idxB)
      (idxA, idxB) -> c.weight
    }.to(Map)
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
    val roomCenters: Map[HistoricalId, (Double, Double)] = g.rooms.indices.view.map({ i =>
      g.rooms(i).id -> normalizePosition(sm.position(i))
    }).to(Map)


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
      def isEmptyRow(l: Int, r: Int)(y: Int): Boolean = y >= 0 && y < roomGrid.height && allEmpty(l, y, r, y + 1)
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

    RoomType("fore", spaceWeight = 100, minQuantity = 1, maxQuantity = 5),
    RoomType("aft", spaceWeight = 100, minQuantity = 1, maxQuantity = 5),
  )

  val byId: Map[RoomTypeId, RoomType] = all.zipWithIndex.map {
    case (rt, id) => RoomTypeId(id) -> rt
  }.to(Map)

  val byName: Map[String, RoomTypeId] = byId.map { case (k, v) => v.name -> k }
}
case class Coordinates(x:Int,y:Int)
case class Room(roomType: RoomTypeId, coords:Coordinates) {}

