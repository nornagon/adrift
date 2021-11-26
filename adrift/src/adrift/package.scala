package object adrift {
  def timed[T](description: String)(f: => T): T = {
    val start = System.nanoTime()
    val ret = f
    println(f"$description took ${(System.nanoTime() - start) / 1e6}%.2f ms")
    ret
  }
}
