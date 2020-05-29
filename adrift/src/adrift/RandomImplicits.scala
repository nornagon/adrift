package adrift

object RandomImplicits {
  implicit class ExtendedRandom(r: scala.util.Random) {
    import Ordering.Double.TotalOrdering
    /** Returns an angle in the range [0, 2pi) */
    def angle: Double = r.nextDouble() * 2 * Math.PI
    /** Returns true one time out of n (on average) */
    def oneIn(n: Int): Boolean = r.between(0, n) == 0

    def bilateral: Double = r.nextDouble() * 2 - 1
    def bilateral(k: Double): Double = k * (r.nextDouble() * 2 - 1)

    def oneOf[K](options: K*): K = pick(options)
    def nOf[K](n: Int, options: Seq[K]): Seq[K] = r.shuffle(options).take(n)
    def pick[K](options: Seq[K]): K = options(r.between(0, options.size).floor.toInt)
    def pick[K](options: IterableOnce[K]): K = {
      var i = 1
      val iterator = options.iterator
      var x = iterator.next
      while (iterator.hasNext) {
        val b = iterator.next
        i += 1
        if (oneIn(i)) x = b
      }
      x
    }
    def maybePick[K](options: IterableOnce[K]): Option[K] = {
      var i = 1
      val iterator = options.iterator
      if (!iterator.hasNext) return None
      var x = iterator.next
      while (iterator.hasNext) {
        val b = iterator.next
        i += 1
        if (oneIn(i)) x = b
      }
      Some(x)
    }

    // https://en.wikipedia.org/wiki/Reservoir_sampling#Algorithm_A-Res
    def chooseFrom[T](elems: IterableOnce[T])(weight: T => Double): T =
      elems.iterator.maxBy { elem => math.pow(r.nextDouble(), 1 / weight(elem)) }

    def nFrom[T](k: Int, elems: IterableOnce[T])(weight: T => Double): Iterable[T] = {
      if (k == 0) return Seq.empty
      val h = new scala.collection.mutable.PriorityQueue[(Double, T)]()(Ordering.by(_._1))
      elems.iterator.foreach { e =>
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
      h.view.map(_._2)
    }
  }
}
