package adrift

object GasComposition {
  def zero: GasComposition = GasComposition(0, 0, 0)

  // pressure at sea level is ~ 100 kPa
  // 78% N2, 21% O2, 0.93% Ar, 0.04% CO2, + etc.
  def earthLike: GasComposition = GasComposition(oxygen = 21, carbonDioxide = 1, nitrogen = 78)
}

// partial pressure of each gas, in kPa
case class GasComposition(oxygen: Float, carbonDioxide: Float, nitrogen: Float) {
  def totalPressure(): Float = oxygen + carbonDioxide + nitrogen

  def -(gc: GasComposition): GasComposition =
    GasComposition(oxygen - gc.oxygen, carbonDioxide - gc.carbonDioxide, nitrogen - gc.nitrogen)
  def +(gc: GasComposition): GasComposition =
    GasComposition(oxygen + gc.oxygen, carbonDioxide + gc.carbonDioxide, nitrogen + gc.nitrogen)

  def *(s: Float): GasComposition =
    GasComposition(oxygen*s, carbonDioxide*s, nitrogen*s)

  def /(s: Float): GasComposition =
    GasComposition(oxygen/s, carbonDioxide/s, nitrogen/s)

  def minPressure(): Float =
    math.min(math.min(oxygen, carbonDioxide), nitrogen)

  override def toString: String = f"GC(O2 = $oxygen%.2f, CO2 = $carbonDioxide%.2f, N2 = $nitrogen%.2f)"
}
