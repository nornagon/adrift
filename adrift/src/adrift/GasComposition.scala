package adrift


case class GasComposition(oxygen:Double, carbonDioxide:Double, nitrogen:Double) {
    def totalPressure(): Double = {
        oxygen+carbonDioxide+nitrogen
    }
    def -(gc: GasComposition): GasComposition = {
        val doxygen = oxygen - gc.oxygen
        val dcarbonDioxide = carbonDioxide - gc.carbonDioxide
        val dnitrogen = nitrogen - gc.nitrogen
        GasComposition(doxygen,dcarbonDioxide,dnitrogen)
    }
    def *(s: Double): GasComposition = {
        GasComposition(oxygen*s, carbonDioxide*s, nitrogen*s)
    }
    def +(gc: GasComposition): GasComposition = {
        GasComposition(oxygen + gc.oxygen, carbonDioxide + gc.carbonDioxide, nitrogen + gc.nitrogen)
    }
}