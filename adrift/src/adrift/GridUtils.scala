package adrift

object GridUtils {
  def hollowCircle(x0: Int, y0: Int, r: Int)(putpixel: (Int, Int) => Unit): Unit = {
    var x: Int = r-1
    var y: Int = 0
    var dx: Int = 1
    var dy: Int = 1
    var err: Int = dx - (r << 1)
    while (x >= y) {
      putpixel(x0 + x, y0 + y)
      putpixel(x0 + y, y0 + x)
      putpixel(x0 - y, y0 + x)
      putpixel(x0 - x, y0 + y)
      putpixel(x0 - x, y0 - y)
      putpixel(x0 - y, y0 - x)
      putpixel(x0 + y, y0 - x)
      putpixel(x0 + x, y0 - y)

      if (err <= 0) {
        y += 1
        err += dy
        dy += 2
      }
      if (err > 0) {
        x -= 1
        dx += 2
        err += (-r << 1) + dx
      }
    }
  }

  def filledCircle(x0: Int, y0: Int, r: Int)(putpixel: (Int, Int) => Unit): Unit = {
    var x: Int = r-1
    var y: Int = 0
    var dx: Int = 1
    var dy: Int = 1
    var err: Int = dx - (r << 1)
    var lastY = -1
    while (x >= y) {
      if (y != lastY) {
        for (xi <- x0 - x to x0 + x) putpixel(xi, y0 + y)
        for (xi <- x0 - y to x0 + y) putpixel(xi, y0 + x)
        for (xi <- x0 - x to x0 + x) putpixel(xi, y0 - y)
        for (xi <- x0 - y to x0 + y) putpixel(xi, y0 - x)
      }
      lastY = y

      if (err <= 0) {
        y += 1
        err += dy
        dy += 2
      }
      if (err > 0) {
        x -= 1
        dx += 2
        err += (-r << 1) + dx
      }
    }
  }
}
