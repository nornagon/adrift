package adrift

object RandomImplicits {
  implicit class ExtendedRandom(r: scala.util.Random) {
    /** Returns an angle in the range [0, 2pi) */
    def angle: Double = r.nextDouble() * 2 * Math.PI
    /** Returns a double in the range [a, b) */
    def between(a: Double, b: Double): Double = r.nextDouble() * (b - a) + a
    /** Returns an integer in the range [a, b) */
    def between(a: Int, b: Int): Int = (r.nextDouble() * (b - a) + a).floor.toInt
    /** Returns true one time out of n (on average) */
    def oneIn(n: Int): Boolean = between(0, n) == 0

    def bilateral: Double = r.nextDouble() * 2 - 1
    def bilateral(k: Double): Double = k * (r.nextDouble() * 2 - 1)

    def oneOf[K](options: K*): K = pick(options)
    def nOf[K](n: Int, options: Seq[K]): Seq[K] = r.shuffle(options).take(n)
    def pick[K](options: Seq[K]): K = options(between(0, options.size).floor.toInt)
    def pick[K](options: TraversableOnce[K]): K = {
      var i = 0
      options.reduce((a, b) => { i += 1; if (oneIn(i + 1)) a else b })
    }

    // https://en.wikipedia.org/wiki/Reservoir_sampling#Algorithm_A-Res
    def chooseFrom[T](elems: TraversableOnce[T])(weight: T => Double): T =
      elems.maxBy { elem => math.pow(r.nextDouble(), 1 / weight(elem)) }

    def nFrom[T](k: Int, elems: TraversableOnce[T])(weight: T => Double): Seq[T] = {
      val h = new scala.collection.mutable.PriorityQueue[(Double, T)]()(Ordering.by(_._1))
      elems.foreach { e =>
        val rv = math.pow(r.nextDouble(), 1 / weight(e))
        if (h.size < k)
          h.enqueue((rv, e))
        else {
          if (rv > h.head._1) {
            h.dequeue()
            h.enqueue((rv, e))
          }
        }
      }
      h.map(_._2)(collection.breakOut)
    }
  }
}
