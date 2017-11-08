package adrift

object FOV {
  def fovCircle(
    radius: Double,
    opaqueApply: Boolean,
    opaque: (Int, Int) => Boolean,
    applyLight: (Int, Int) => Unit,
  ): Unit = {
    fovOctant(+1, +1, flip = false)
    fovOctant(+1, +1, flip = true)
    fovOctant(+1, -1, flip = false)
    fovOctant(+1, -1, flip = true)
    fovOctant(-1, +1, flip = false)
    fovOctant(-1, +1, flip = true)
    fovOctant(-1, -1, flip = false)
    fovOctant(-1, -1, flip = true)

    def fovOctant(
      signx: Int,
      signy: Int,
      flip: Boolean,
      dx: Int = 1,
      startSlope: Double = 0,
      endSlope: Double = 1
    ): Unit = {
      if (dx == 0) {
        fovOctant(signx, signy, flip, dx = 1, startSlope, endSlope)
        return
      }
      if (math.abs(dx) > radius) {
        return
      }
      val applyEdge = signy == 1
      val applyDiag = !flip

      val dy0 = (0.5 + dx * startSlope).toInt
      var dy1 = (0.5 + dx * endSlope).toInt

      val p =
        if (flip)
          (signy * dy0, signx * dx)
        else
          (signx * dx, signy * dy0)

      if (!applyDiag && dy1 == dx) {
        dy1 -= 1
      }

      val h = math.sqrt(radius * radius - dx * dx).toInt

      if (math.abs(dy1) > h) {
        if (h == 0) return
        dy1 = h
      }

      var prevBlocked = -1
      var startSlopeT = startSlope
      for (dy <- dy0 to dy1) {
        val pt = if (flip)
          (signy * dy, p._2)
        else
          (p._1, signy * dy)
        if (opaque(pt._1, pt._2)) {
          if (opaqueApply && (applyEdge || dy > 0)) {
            applyLight(pt._1, pt._2)
          }
          if (prevBlocked == 0) {
            val endSlopeNext = fovSlope(dx + 0.5, dy - 0.5)
            fovOctant(signx, signy, flip, dx+1, startSlopeT, endSlopeNext)
          }
          prevBlocked = 1
        } else {
          if (applyEdge || dy > 0) {
            applyLight(pt._1, pt._2)
          }
          if (prevBlocked == 1) {
            startSlopeT = fovSlope(dx - 0.5, dy - 0.5)
          }
          prevBlocked = 0
        }
      }

      if (prevBlocked == 0)
        fovOctant(signx, signy, flip, dx+1, startSlopeT, endSlope)
    }
  }

  private def fovSlope(dx: Double, dy: Double): Double =
    if (dx <= -Float.MinPositiveValue || dx >= Float.MinPositiveValue)
      dy/dx
    else 0
}
