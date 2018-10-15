package adrift

import adrift.items._

sealed trait Furniture {
  def walkable: Boolean = true
  def opaque: Boolean = false

  def parts: Seq[(ItemKind, Int)]
  def description: String
}

object Furniture {
  case class AutomaticDoor(var open: Boolean = false) extends Furniture {
    override def walkable: Boolean = open
    override def opaque: Boolean = !open

    override val parts = Seq(
      ModularPanel.SizeD -> 2,
      Screw.TypeIIB -> 8,
      ModularRod.SizeD -> 4,
      StepperMotor.C -> 4,
      // still needs: control circuitry, power, presence detection
    )

    override val description: String = "An automatic door. It's supposed to detect when someone walks up to it."
  }

  case object Desk extends Furniture {
    override val walkable = true

    override def parts: Seq[(ItemKind, Int)] = Seq(/* todo */)

    override def description: String = "A regulation-size small wooden desk. It's absurd to you that they made these out of real dead trees, rather than the modular stuff everything else is made out of."
  }
}
