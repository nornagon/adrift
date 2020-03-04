package adrift.worldgen
import util.Random
import adrift.RandomImplicits._


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

case class RoomType(
  spaceWeight: Double,
  minQuantity:Int,
  maxQuantity:Int,
) {}
case class Coordinates(x:Int,y:Int)
case class Room(roomType: RoomType, coords:Coordinates) {}

case object GArchitect {
  val random = new Random(0)
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

  // Engineering
  val engineRoom:RoomType = new RoomType(100, 1, 5);
//  val recycling = RoomType(50, 1, 5)
 // val fabrication = RoomType(50, 5, 10)

  // Habitation
  val crewQuarters:RoomType = new RoomType(75, 1, 5000)
//  val promenade = RoomType(100,1,1)
//  val dining = RoomType(50,4,10)
//  val holosuite = RoomType(10,5,10)
//  val lounge = RoomType(10,10,20)

  // Science
  //  val astronomy = RoomType(10,1,3)

  // Command
  val command:RoomType = new RoomType(10,1,1)

  val fore:RoomType = new RoomType(0,1,1)
  val aft:RoomType = new RoomType(0,1,1)

  val roomTypes:List[RoomType] = List(
    command,
    engineRoom,
    crewQuarters
  )
  val roomWeighting:List[(RoomType,List[Int])] = List(
    (engineRoom,List(0,1,1,0,1)),
    (crewQuarters,List(1,4,1,0,0)),
    (command,List(1,2,0,0,0)),
    (fore,List(0,0,0,0,0)),
    (aft,List(0,0,0,0,0))
  )

  def PlaceRooms(roomTypes:List[RoomType]): List[Room] = {
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
        Room(r, Coordinates(Random.nextInt(SC_horizontal), Random.nextInt(SC_vertical)))
      }
      }
    })
  }

  def roomDistances(roomlist: Seq[Room]): Map[Room,Map[Room,Double]] = {
    val distmap = roomlist.map(r1 => {
      val distances = roomlist.map(r2 => {
        distance(r1.coords,r2.coords)
      })
      (roomlist zip distances).toMap
    })
    (roomlist zip distmap).toMap
  }

  def spaceAllocation(roomlist:Seq[Room]):Int = {
    // provide reward in proportion to distance from roomtypes that have a large space allocation
    val roomdistances = roomDistances(roomlist)
    // find the 5 closest rooms
    // multiply by the space allocation value for the roomtype divided by the number of rooms of that type there are.
    val rtypes = roomlist map(r=> r.roomType)
    roomlist.map(r => {
      val numRooms = rtypes.count(p => p == r.roomType)
      roomdistances(r).values.toSeq.sorted.slice(0,5).sum * r.roomType.spaceWeight / numRooms
    }).sum.toInt
    // thats our reward.  Yar.
  }

  def linedup(roomlist:Seq[Room]):Int = {
    // provide reward based on the number of rooms that share x or y coordinates - that are lined up
    // largest possible number of unique rows or columns is the length of the roomlist.
    // smallest possible value is 1 if all rooms are at the same point.
    val xs = roomlist.map( r=> r.coords.x)
    val ys = roomlist.map( r=> r.coords.y)
    roomlist.length*2 - xs.distinct.length - ys.distinct.length
  }

  def evaluate(population:Seq[Seq[Room]], metrics:Seq[(Seq[Room]=>Int,Double)]): Seq[Double] = {
    // first wec'll apply all our metric calculations to our population.
    val rawEvaluation = metrics.map(m=> {
      population.map(rl => m._1(rl))
    })
    // now lets normalize and reweight our metrics.
    // basically, right now each metric is some integer, but some metrics might be orders of magnitude larger or smaller than others
    // and that will cause them to disproportionately affect our selection.  So we will normalize this population so the best are '1' and the worst is '0' for each metric
    // then we can scale each based on how important we want it to be in our final mating evaluation.
    val weights = metrics.map(m=> m._2)
    rawEvaluation.zip(weights).map(ev => {
      // there's probably a cleaner / more idiomatic way to do this.
      val e = ev._1
      val v = ev._2
      e.map(v => (v-e.min).toDouble / (e.max - e.min).toDouble ).sum * v
    })
  }

  def mutate(population:Seq[Seq[Room]], rate:Int): Unit ={
    // in this case, rate is just the maximum distance we might move a room around.  Likely when we run this we'll start
    // with a high rate and gradually reduce it, simulated annealing style
    def scoot(coords:Coordinates,amount:Int) = {
      Coordinates(coords.x + random.nextInt(amount*2)-amount,
                 coords.y + random.nextInt(amount*2)-amount)
    }
    population.map( individual => individual.map( room => Room(room.roomType,scoot(room.coords,rate))))
  }

  def crossover(population:Seq[Seq[Room]], metrics:Seq[(Seq[Room]=>Int,Double)]): Seq[Seq[Room]] = {
    // individuals in the population who are more fit are more likely to reproduce, so we need some metrics.
    val parents = evaluate(population, metrics).zip(population).sortBy{ elem => math.pow(random.nextDouble(), 1 / elem._1) }.take(population.length/2)
    parents.flatMap {
      parent1 => {
        val parent2 = random.chooseFrom(parents)(p => p._1)
        mate(parent1._2, parent2._2)
      }
    }
  }
  def mate(parent1:Seq[Room], parent2:Seq[Room]): Seq[Seq[Room]] = {
    val roomtypes = parent1.map(r => r.roomType).distinct
    List(1,2).map(_ => roomtypes.flatMap(rt => {
      val p1Rooms = parent1.filter(r=> r.roomType==rt)
      val p2Rooms = parent2.filter(r=> r.roomType==rt)
      val numNewRooms = random.between(p1Rooms.length,p2Rooms.length)
      val roomPool = p1Rooms ++ p2Rooms
      roomPool.sortBy(_ => random.nextDouble()).take(numNewRooms)
    }))
  }
}