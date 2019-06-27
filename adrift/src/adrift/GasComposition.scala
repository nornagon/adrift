package adrift

object GasComposition {
    def zero() = {
        GasComposition(0,0,0)
    }
    def oxygen(qty:Double) = {
        new OxygenGas(qty)
    }
    def nitrogen(qty:Double) = {
        new NitrogenGas(qty)
    }
    def carbonDioxide(qty:Double) = {
        new CarbonDioxideGas(qty)
    }
}

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
    def minPressure(): Double = {
        math.min(math.min(oxygen,carbonDioxide),nitrogen)
    }
}

sealed trait UnitGas
class OxygenGas(override val oxygen: Double) extends GasComposition(oxygen,carbonDioxide=0,nitrogen=0) with UnitGas {}
class NitrogenGas(override val nitrogen: Double) extends GasComposition(oxygen=0,carbonDioxide=0,nitrogen) with UnitGas {}
class CarbonDioxideGas(override val carbonDioxide: Double) extends GasComposition(oxygen=0,carbonDioxide,nitrogen=0) with UnitGas {}
