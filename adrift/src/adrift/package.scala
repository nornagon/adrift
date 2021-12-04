package object adrift {
  def timed[T](description: String)(f: => T): T = {
    val start = System.nanoTime()
    val ret = f
    println(f"$description took ${(System.nanoTime() - start) / 1e6}%.2f ms")
    ret
  }

  object animation {
    // https://nornagon.medium.com/math-for-game-developers-parameterised-easing-9336a50c816d
    def squash(k: Double, t: Double): Double =
      if (t >= 1) 1
      else if (t < -1) -1
      else if (t >= 0) 1 - scala.math.pow(1 - t, k)
      else scala.math.pow(1 + t, k) - 1
  }
}
