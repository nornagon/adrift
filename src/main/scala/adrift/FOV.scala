package adrift

object FOV {
  /**
    * Compute field of view by recursive shadowcasting. Based on bluepuyo's libfov
    * (https://code.google.com/archive/p/libfov/), which is in turn based on Björn Bergström's algorithm
    * (http://www.roguebasin.com/index.php?title=FOV_using_recursive_shadowcasting)
    *
    * @param radius Tiles further away than this (in Euclidean distance) will not be lit.
    * @param opaqueApply Whether to call `applyLight` on tiles which are opaque.
    * @param opaque Function describing which tiles are opaque.
    * @param applyLight Function called to signify that a tile should be lit.
    */
  def castShadows(
    radius: Double,
    opaqueApply: Boolean,
    opaque: (Int, Int) => Boolean,
    applyLight: (Int, Int) => Unit,
  ): Unit = {
    val opaqueT = opaque.tupled
    val applyT = applyLight.tupled
    octant((x, y) => (+x, +y), applyEdge = true, applyDiag = true)
    octant((x, y) => (+y, +x), applyEdge = true, applyDiag = false)
    octant((x, y) => (+x, -y), applyEdge = false, applyDiag = true)
    octant((x, y) => (-y, +x), applyEdge = false, applyDiag = false)
    octant((x, y) => (-x, +y), applyEdge = true, applyDiag = true)
    octant((x, y) => (+y, -x), applyEdge = true, applyDiag = false)
    octant((x, y) => (-x, -y), applyEdge = false, applyDiag = true)
    octant((x, y) => (-y, -x), applyEdge = false, applyDiag = false)

    /**
      * Internal function that computes one octant of the FOV. It's written as if this were always the first octant,
      * i.e. {x > 0; y > 0; y < x}, but the `transformCoord` function can conceptually make it apply to any of the 8
      * octants.
      *
      * `applyEdge` and `applyDiag` are supplied as parameters to prevent overlap between octants. If `applyEdge` is
      * true, tiles along y = 0 won't be lit. If `applyDiag` is true, tiles along y = x won't be lit. Since these two
      * cases describe the border between two octants, they should be true for one of the neighboring octants and false
      * for the other to ensure that `applyLight` is called exactly once per lit tile.
      *
      * Each call to this function computes one 'strip' of the light. In the first octant, this is one x-value.
      *
      * @param transformCoord Transforms coordinates before calling `opaque`/`applyLight`.
      * @param applyEdge Whether to call `applyLight` on the y = 0 edge.
      * @param applyDiag Whether to call `applyLight` on the y = x edge.
      * @param dx The x-value of the strip to compute.
      * @param startSlope The smaller (i.e. lower, if +y is up) slope of the sector of light entering this strip.
      * @param endSlope The larger (i.e. upper, if +y is up) slope of the sector of light entering this strip.
      */
    def octant(
      transformCoord: (Int, Int) => (Int, Int),
      applyEdge: Boolean,
      applyDiag: Boolean,
      dx: Int = 1,
      startSlope: Float = 0,
      endSlope: Float = 1
    ): Unit = {
      assert(dx >= 1)
      if (dx > radius) {
        return
      }

      val h = math.sqrt(radius * radius - dx * dx).toInt
      val dy0 = (dx * startSlope).round
      val dy1 = math.min(h, (dx * endSlope).round) match {
        case dy if dy == dx && !applyDiag => dy - 1
        case dy => dy
      }
      if (h == 0 && dy1 > 0) {
        return
      }

      var prevBlocked = -1
      var startSlopeNext = startSlope
      for (dy <- dy0 to dy1) {
        val p = transformCoord(dx, dy)
        if (opaqueT(p)) {
          if (opaqueApply && (applyEdge || dy > 0)) {
            applyT(p)
          }
          if (prevBlocked == 0) {
            val endSlopeNext = (dy - 0.5f) / (dx + 0.5f)
            octant(transformCoord, applyEdge, applyDiag, dx + 1, startSlopeNext, endSlopeNext)
          }
          prevBlocked = 1
        } else {
          if (applyEdge || dy > 0) {
            applyT(p)
          }
          if (prevBlocked == 1) {
            startSlopeNext = (dy - 0.5f) / (dx - 0.5f)
          }
          prevBlocked = 0
        }
      }

      if (prevBlocked == 0)
        octant(transformCoord, applyEdge, applyDiag, dx + 1, startSlopeNext, endSlope)
    }
  }
}
