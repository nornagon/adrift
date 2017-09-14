package adrift.items

trait ItemKind {
  def name: String
  def description: String
  def parts: Seq[(ItemKind, Int)]
}

trait ItemCondition {
  def functional: Boolean = false
}

case class Item(
  kind: ItemKind,
  conditions: Seq[ItemCondition],
  parts: Seq[Item]
) {
  def functional: Boolean = conditions.forall(_.functional) && parts.forall(_.functional)
}

case class Charge(kwh: Double, maxKwh: Double) extends ItemCondition {
  override def functional: Boolean = kwh > 0
}
case class BrokenWire() extends ItemCondition
case class Cracked() extends ItemCondition
case class BurntOut() extends ItemCondition
case class Rusted() extends ItemCondition

case object CopperWire extends ItemKind {
  val name = "copper wire"
  val description = "A few meters of thin copper wire."
  val parts = Seq()
}

case object Magnet extends ItemKind {
  val name = "magnet"
  val description = "A permanent ferrite magnet. It'd stick to your refrigerator."
  val parts = Seq()
}

case object TinyDCMotor extends ItemKind {
  val name = "tiny DC motor"
  val description = "A small DC motor. Could be from a remote-controlled toy or something."
  val parts = Seq(CopperWire -> 2, Magnet -> 2)
}

case object Mirror extends ItemKind {
  val name = "mirror"
  val description = "A small mirror. Your face looks strange, as if it belongs to someone else."
  val parts = Seq()
}

case object LaserDiode extends ItemKind {
  val name = "laser diode"
  val description = "A weak laser diode. Probably still shouldn't look directly into it, though."
  val parts = Seq()
}

case object RechargeableBattery extends ItemKind {
  val name = "rechargeable battery"
  val description = "A rechargeable copper foam substrate battery. Popular in RC toys. Safe and long-lasting."
  val parts = Seq()
}

case object HolographicProjector extends ItemKind {
  val name = "holographic projector"
  val description = "A laser array and an assortment of spinning mirrors, capable of producing an ethereal full-color 3-dimensional image."
  val parts = Seq(LaserDiode -> 3, Mirror -> 1, TinyDCMotor -> 1)
}

case object SmallPlasticCasing extends ItemKind {
  val name = "small plastic casing"
  val description = "A small plastic box that can be screwed shut."
  val parts = Seq()
}

case object Microprocessor extends ItemKind {
  val name = "microprocessor"
  val description = "The part number is written on the chip: BCM2837RIFBG."
  val parts = Seq()
}

case object MRAMChip extends ItemKind {
  val name = "MRAM chip"
  val description = "A magnetic memory chip. You'd need to attach it to a computer to read its contents."
  val parts = Seq()
}

case object TypeIAScrew extends ItemKind {
  val name = "type IA screw"
  val description = "A carbon-printed type IA screw, like you'd find in any cheap device these days."
  val parts = Seq()
}

case object HoloNote extends ItemKind {
  val name = "holonote"
  val description = "A small cube that projects a 3D holographic message. A novelty, really, it's much easier to just write an e-mail."
  val parts = Seq(
    RechargeableBattery -> 1,
    HolographicProjector -> 1,
    SmallPlasticCasing -> 1,
    Microprocessor -> 1,
    MRAMChip -> 1,
    TypeIAScrew -> 6
  )
}

case object Fastener extends ItemKind {
  val name = "fastener"
  val description = "A small metal connector, with two parts easily attached and detached from one another by hand."
  val parts = Seq()
}

case object NylonThread extends ItemKind {
  val name = "nylon thread"
  val description = "A few meters of nylon thread."
  val parts = Seq()
}

case object SyntheticFabric extends ItemKind {
  val name = "synthetic fabric rag"
  val description = "A small section of synthetic fabric. It's extremely light and strong."
  val parts = Seq(NylonThread -> 8)
}

case object Backpack extends ItemKind {
  val name = "backpack"
  val description = "A light textile bag that straps close to the body. It has a lot of pockets."
  val parts = Seq(SyntheticFabric -> 4, NylonThread -> 2, Fastener -> 2)
}
