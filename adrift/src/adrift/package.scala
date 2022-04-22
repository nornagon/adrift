package object adrift {
  def timed[T](description: String)(f: => T): T = {
    val start = System.nanoTime()
    val ret = f
    println(f"$description took ${(System.nanoTime() - start) / 1e6}%.2f ms")
    ret
  }

  object animation {
    // https://nornagon.medium.com/math-for-game-developers-parameterised-easing-9336a50c816d
    def squash(k: Double, t: Double): Double = {
      if (t >= 1) 1
      else if (t < -1) -1
      else if (t >= 0) 1 - scala.math.pow(1 - t, k)
      else scala.math.pow(1 + t, k) - 1
    }
    def squash(k: Float, t: Float): Float = {
      if (t >= 1) 1
      else if (t < -1) -1
      else if (t >= 0) 1 - scala.math.pow(1 - t, k).toFloat
      else scala.math.pow(1 + t, k).toFloat - 1f
    }

    def squash01(k: Double, t: Double): Double =
      (squash(k, t * 2 - 1) + 1) / 2
    def squash01(k: Float, t: Float): Float =
      (squash(k, t * 2 - 1) + 1) / 2

    def lerp(a: Float, b: Float, t: Float): Float = a * (1 - t) + b * t
    def lerp(a: Double, b: Double, t: Double): Double = a * (1 - t) + b * t

    def smoothstep(t: Float): Float =
      if (t < 0) 0
      else if (t >= 1) 1
      else t * t * (3 - 2 * t)
  }
}
