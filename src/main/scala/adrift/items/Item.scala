package adrift.items

/*
Items have some generic type (a battery, a screwdriver, a tomato) and some specific state (half charged, found in the
elevator, moldy).

Items are made out of other specific items, which in turn have their own generic type and specific state.

The kinds of things in an item's specific state differ based on the type. For example, it doesn't make sense for a
tomato to be half-charged, or for a battery to be moldy (unless it was some kind of bio-battery...).

Some items have one or more "functions", i.e. things you can do with them. They could be either direct functions (turn
on the heater, listen to the recording, saw the log in half) or indirect functions (use the plasma torch in crafting,
wear the haz-mat suit, provide light).

For items with a function, all the parts of that item must recursively be functional for the item itself to be
functional. If an item is comprised of other items, it is functional iff all its components are functional. An item
can't be made out of functional things and yet itself be non-functional.

Items can be damaged in a variety of ways. Some damage is trivially easy to repair (e.g. recharge a battery), while
other kinds of damage require specialty tools just to diagnose, let alone repair.

=> so not only do items have specific statuses, the player might also have a varying amount of information about an
item's status. Do they know that the turbolift isn't working because one of the power relays has a broken fuse? Or can
they just tell that the power supply seems to be out of order? (Or just that the buttons don't seem to do anything?)


*****

Where can items be?
- on the ground
- in your hands
- in a container on the ground
- worn on your body
- in a container worn on your body
- inside your body (implants)
- part of another item

What can you do with items?
Simple:
- pick up
- put down
- store in container
Less simple:
- eat
- drink
- disassemble
- assemble
- wear

 */

trait ItemKind {
  def name: String
  def description: String
  def parts: Seq[((ItemKind, Int),Operation)]
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


trait Tool {
  def provides: Operation
}
trait Operation{}
case class HandleOp() extends Operation
case class ScrewOp() extends Operation
case class BoltOp() extends Operation
case class PryOp() extends Operation
case class HammerOp() extends Operation
case class CutOp() extends Operation
case class SolderOp() extends Operation


//
// Tools
//

case object Screwdriver extends ItemKind with Tool {
  val name = "screwdriver"
  val description = "A standard K35 screwdriver, like you'd find in a koligan."
  val parts = Seq()
  val provides = ScrewOp()
}

case object HandHammer extends ItemKind with Tool {
  val name = "hand-driven hammer"
  val description = "A largely obsolete device, the hand-held hammer consists of a rigid handle and weighted head."
  val parts = Seq()
  val provides = HammerOp()
}

case object Crowbar extends ItemKind with Tool {
  val name = "crowbar"
  val description = "Used during the invasion by many as a last-ditch weapon. The weight is comforting, and it's a practical tool too."
  val parts = Seq()
  val provides = PryOp()
}

case object MultiWrench extends ItemKind with Tool {
  val name = "multi-wrench"
  val description = "twisting the knob adjusts the grip size.  This is pretty much only useful for adding or removing bolts."
  val parts = Seq()
  val provides = BoltOp()
}

case object PocketKnife extends ItemKind with Tool {
  val name = "BoxCutter"
  val description = "A sharp, virtually unbreakable tungsteel blade makes this a great tool or, in a pinch, a weapon."
  val parts = Seq()
  val provides = CutOp()
}

case object SolderingIron extends ItemKind with Tool {
  val name = "Soldering Iron"
  val description = "an RF powered insta-hot pocket-sized portable soldering iron makes it easy to add or remove soldered components"
  val parts = Seq()
  val provides = SolderOp()
}

//
// Raw Materials
//

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

case object SmallGlassPlate extends ItemKind {
  val name = "glass plate"
  val description = "A small glass plate. Maybe for a small window, display screen or mirror."
  val parts = Seq()
}

case object AluminumFoil extends ItemKind {
  val name = "aluminum foil"
  val description = "A small sheet of aluminum foil. Wrap a burrito or make something shiny."
  val parts = Seq()
}

case object SheetMetal extends ItemKind {
  val name = "sheet metal"
  val description = "A small sheet of some steel alloy. make a box or plate from it."
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
  val parts = Seq((NylonThread -> 8) -> CutOp())
}


//
// Optical components?
//

case class Lens(name: String, description: String, parts: Seq[((ItemKind,Int),Operation)] = Seq()) extends ItemKind
object Lens {
  def Z1L: Lens = Lens("Z1 Lens", "The smallest most concave module-compatible lens available")
  def Z2L: Lens = Lens("Z2 Lens", "A small, maximally concave module-compatible lens")
  def Z3L: Lens = Lens("Z3 Lens", "A medium sized, maximally concave module-compatible lens")
  def Z4L: Lens = Lens("Z4 Lens", "A large, maximally concave module-compatible lens")
  def Z5L: Lens = Lens("Z5 Lens", "A huge, maximally concave module-compatible lens")
  def Y1L: Lens = Lens("Y1 Lens", "A teeny-tiny, concave module-compatible lens available")
  def Y2L: Lens = Lens("Y2 Lens", "A small, concave module-compatible lens")
  def Y3L: Lens = Lens("Y3 Lens", "A medium sized, concave module-compatible lens")
  def Y4L: Lens = Lens("Y4 Lens", "A large, concave module-compatible lens")
  def Y5L: Lens = Lens("Y5 Lens", "A huge, concave module-compatible lens")
  def X1L: Lens = Lens("X1 Lens", "The smallest convex module-compatible lens available")
  def X2L: Lens = Lens("X2 Lens", "A small, convex module-compatible lens")
  def X3L: Lens = Lens("X3 Lens", "A medium sized, convex module-compatible lens")
  def X4L: Lens = Lens("X4 Lens", "A large, convex module-compatible lens")
  def X5L: Lens = Lens("X5 Lens", "A huge, convex module-compatible lens")
  def W1L: Lens = Lens("W1 Lens", "The smallest maximally convex module-compatible lens available")
  def W2L: Lens = Lens("W2 Lens", "A small, maximally convex module-compatible lens")
  def W3L: Lens = Lens("W3 Lens", "A medium sized, maximally convex module-compatible lens")
  def W4L: Lens = Lens("W4 Lens", "A large, maximally convex module-compatible lens")
  def W5L: Lens = Lens("W5 Lens", "A huge, maximally convex module-compatible lens")
}

case class Mirror(name: String, description: String, parts: Seq[((ItemKind,Int),Operation)] = Seq()) extends ItemKind
object Mirror {
  def Z1M: Mirror = Mirror("Z1 Mirror", "The smallest most concave module-compatible mirror available")
  def Z2M: Mirror = Mirror("Z2 Mirror", "A small, maximally concave module-compatible mirror")
  def Z3M: Mirror = Mirror("Z3 Mirror", "A medium sized, maximally concave module-compatible mirror")
  def Z4M: Mirror = Mirror("Z4 Mirror", "A large, maximally concave module-compatible mirror")
  def Z5M: Mirror = Mirror("Z5 Mirror", "A huge, maximally concave module-compatible mirror")
  def Y1M: Mirror = Mirror("Y1 Mirror", "A teeny-tiny, concave module-compatible mirror available")
  def Y2M: Mirror = Mirror("Y2 Mirror", "A small, concave module-compatible mirror")
  def Y3M: Mirror = Mirror("Y3 Mirror", "A medium sized, concave module-compatible mirror")
  def Y4M: Mirror = Mirror("Y4 Mirror", "A large, concave module-compatible mirror")
  def Y5M: Mirror = Mirror("Y5 Mirror", "A huge, concave module-compatible mirror")
  def X1M: Mirror = Mirror("X1 Mirror", "The smallest convex module-compatible mirror available")
  def X2M: Mirror = Mirror("X2 Mirror", "A small, convex module-compatible mirror")
  def X3M: Mirror = Mirror("X3 Mirror", "A medium sized, convex module-compatible mirror")
  def X4M: Mirror = Mirror("X4 Mirror", "A large, convex module-compatible mirror")
  def X5M: Mirror = Mirror("X5 Mirror", "A huge, convex module-compatible mirror")
  def W1M: Mirror = Mirror("W1 Mirror", "The smallest maximally convex module-compatible mirror available")
  def W2M: Mirror = Mirror("W2 Mirror", "A small, maximally convex module-compatible mirror")
  def W3M: Mirror = Mirror("W3 Mirror", "A medium sized, maximally convex module-compatible mirror")
  def W4M: Mirror = Mirror("W4 Mirror", "A large, maximally convex module-compatible mirror")
  def W5M: Mirror = Mirror("W5 Mirror", "A huge, maximally convex module-compatible mirror")
}

//
// Electronic parts
//

case object ShipPowerAdapter extends ItemKind {
  val name = "ship power adapter"
  val description = "a box of electronics to adapt from ship power to any other kind of power supply"
  val parts = Seq(
    (EBoard.A -> 1) -> ScrewOp(), 
    (FlexComponent -> 21) -> SolderOp(), 
    (Microprocessor -> 1) -> SolderOp()
    )
}

case class PowerSupply(name: String, description: String, parts: Seq[((ItemKind,Int),Operation)]) extends ItemKind
object PowerSupply {
  def PM3_A: PowerSupply = PowerSupply("M3A power supply", "Ptronics smallest power supply is perfect for a phone or cat toy", Seq())
  def PM5_A: PowerSupply = PowerSupply("M5A power supply", "Another small supply from Ptronics with more voltage and small amps", Seq())
  def PM7_A: PowerSupply = PowerSupply("M7A power supply", "A small high voltage power supply", Seq())
  def PM3_C: PowerSupply = PowerSupply("M3C power supply", "MegaMicro made this supply to fill a gap in Ptronics line of power supplies", Seq())
  def PM5_C: PowerSupply = PowerSupply("M5C power supply", "Ptronics most popular power supply. Used as bricks in some countries.", Seq())
  def PM7_C: PowerSupply = PowerSupply("M7C power supply", "If you touch this power supply wrong, you'll probably die.", Seq())
  def PM3_E: PowerSupply = PowerSupply("M3E power supply", "A low voltage, high amperage supply from your friends at Ptronics", Seq())
  def PM5_E: PowerSupply = PowerSupply("M5E power supply", "An off-brand copy of the popular Ptronics M5E unit... someone was trying to save money here.", Seq())
  def PM7_E: PowerSupply = PowerSupply("M7E power supply", "This beast of a power supply compensates for inefficiency with even more power", Seq())
}

case class EBoard(name: String, description: String, parts: Seq[((ItemKind,Int),Operation)] = Seq()) extends ItemKind
object EBoard {
  def A: EBoard = EBoard("micro e-board", "a small modular electronics substrate")
  def B: EBoard = EBoard("standard e-board", "a normal modular electronics substrate")
  def C: EBoard = EBoard("mega e-board", "a huge modular electronics substrate")
}

case object FlexComponent extends ItemKind {
  val name = "flex component"
  val description = "a single part that can serve as any of a huge number of possible electronic components depending on how it's configured"
  val parts = Seq()
}

case object TinyDCMotor extends ItemKind {
  val name = "tiny DC motor"
  val description = "A small DC motor. Could be from a remote-controlled toy or something."
  val parts = Seq((CopperWire -> 2) -> CutOp(), (Magnet -> 2) -> PryOp())
}

case object MRAMChip extends ItemKind {
  val name = "MRAM chip"
  val description = "A magnetic memory chip. You'd need to attach it to a computer to read or write its contents."
  val parts = Seq()
}

case object Microprocessor extends ItemKind {
  val name = "microprocessor"
  val description = "The part number is written on the chip: BCM2837RIFBG."
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

case object SmallPlasticCasing extends ItemKind {
  val name = "small plastic casing"
  val description = "A small plastic box that can be screwed shut."
  val parts = Seq()
}

// Fasteners

case class Screw(name: String, description: String, parts: Seq[((ItemKind,Int),Operation)] = Seq()) extends ItemKind
object Screw {
  def TypeIA: Screw = Screw("type IA screw", 
                            "A small, carbon-printed type IA screw, like you'd find in any cheap device these days.")
  def TypeIIA: Screw = Screw("type IIA screw", 
                             "A carbon-printed type IIA screw, mostly used for small high quality components.")
  def TypeIIIA: Screw = Screw("type IIIA screw", 
                              "A carbon-printed type IIIA screw, for ultimate precision in micro devices.")
  def TypeIB: Screw = Screw("type IB screw", 
                            "A carbon-printed type IB screw, used when you need to hold large things together, but not too well")
  def TypeIIB: Screw = Screw("type IIB screw", 
                             "A carbon-printed type IIB screw, the standard in afixing human-scale objects.")
  def TypeIIIB: Screw = Screw("type IIIB screw", 
                              "A carbon-printed type IIIB screw, for maximum strength and precision in large objects")
  def TypeIC: Screw = Screw("type IC screw", 
                            "A carbon-printed type IC screw, for bolting together huge plastic crap")
  def TypeIIC: Screw = Screw("type IIC screw", 
                             "A carbon-printed type IIC screw, for holding an engine in a car or similar applications")
  def TypeIIIC: Screw = Screw("type IIIC screw", 
                              "A carbon-printed type IIIC screw, for when you need to keep the wings on your supersonic figher")
}

case object Fastener extends ItemKind {
  val name = "fastener"
  val description = "A small metal connector, with two parts easily attached and detached from one another by hand."
  val parts = Seq()
}

// Modular Item pieces

case class ModularPanel(name: String, description: String, parts: Seq[((ItemKind,Int),Operation)] = Seq()) extends ItemKind
object ModularPanel{
  def SizeA: ModularPanel = ModularPanel("A-Size Modular panel",
                                     "A variable-size modular panel used in constructing micro-scale structural frames")
  def SizeB: ModularPanel = ModularPanel("B-Size Modular panel",
                                     "A variable-size modular panel used in constructing small structural frames")
  def SizeC: ModularPanel = ModularPanel("C-Size Modular panel",
                                     "A variable-size modular panel used in constructing breadbox-sized structural frames")
  def SizeD: ModularPanel = ModularPanel("D-Size Modular panel",
                                     "A variable-size modular panel used in constructing large structural frames")
  def SizeE: ModularPanel = ModularPanel("E-Size Modular panel",
                                     "A variable-size modular panel used in constructing colossal structural frames")
}

case class ModularRod(name: String, description: String, parts: Seq[((ItemKind,Int),Operation)] = Seq()) extends ItemKind
object ModularRod{
  def SizeA: ModularRod = ModularRod("A-Size Modular Rod",
                                     "A variable-length modular rod used in constructing micro-scale structural frames")
  def SizeB: ModularRod = ModularRod("B-Size Modular Rod",
                                     "A variable-length modular rod used in constructing small structural frames")
  def SizeC: ModularRod = ModularRod("C-Size Modular Rod",
                                     "A variable-length modular rod used in constructing breadbox-sized structural frames")
  def SizeD: ModularRod = ModularRod("D-Size Modular Rod",
                                     "A variable-length modular rod used in constructing large structural frames")
  def SizeE: ModularRod = ModularRod("E-Size Modular Rod",
                                     "A variable-length modular rod used in constructing colossal structural frames")
}

case class Frame(name: String, description: String, parts: Seq[((ItemKind,Int),Operation)]) extends ItemKind
object Frame {
  def SizeA: Frame = Frame("A-Size Frame",
                           "A micro-scale structural frame for holding mechanical and electrical components",
                           Seq((ModularRod.SizeA -> 12) -> ScrewOp(),
                               (ModularPanel.SizeA -> 6) -> ScrewOp())
                           )
  def SizeB: Frame = Frame("B-Size Frame",
                           "A small structural frame for holding mechanical and electrical components",
                           Seq((ModularRod.SizeB -> 12) -> ScrewOp(),
                               (ModularPanel.SizeB -> 6) -> ScrewOp())
                          )
  def SizeC: Frame = Frame("C-Size Frame",
                           "A structural frame for holding mechanical and electrical components",
                           Seq((ModularRod.SizeC -> 12) -> ScrewOp(),
                               (ModularPanel.SizeC -> 6) -> ScrewOp())
                          )
  def SizeD: Frame = Frame("D-Size Frame",
                           "A large structural frame for holding mechanical and electrical components",
                           Seq((ModularRod.SizeD -> 12) -> ScrewOp(),
                               (ModularPanel.SizeD -> 6) -> ScrewOp())
                          )
  def SizeE: Frame = Frame("E-Size Frame",
                           "A massive structural frame for holding mechanical and electrical components",
                           Seq((ModularRod.SizeE -> 12) -> ScrewOp(),
                               (ModularPanel.SizeE -> 6) -> ScrewOp())
                          )
}

// Composite Items/furniture likely found in Cryo/medical/crew quarters

case object LaserPump extends ItemKind {
  val name = "laser pump"
  val description = "a device that uses laser power to pressurize or depressurize a volume"
  val parts = Seq(
    (Frame.SizeA -> 2) -> ScrewOp(),
    (Frame.SizeB -> 1) -> ScrewOp(),
    (LaserDiode -> 6) -> ScrewOp(),
    (Lens.Z1L -> 2) -> ScrewOp(),
    (Mirror.Y1M -> 6) -> ScrewOp(),
    (Lens.Y1L -> 6) -> ScrewOp(),
    (Lens.W2L -> 6) -> ScrewOp(),
    (EBoard.A -> 2) -> ScrewOp(),
    (PowerSupply.PM5_C -> 1) -> ScrewOp(),
    (Screw.TypeIC -> 12) -> ScrewOp()
    )
}

case object Refrigerator extends ItemKind {
  val name = "refrigerator"
  val description = "This industrial cooler can suck a lot of heat out of an attached volume"
  val parts = Seq(
    (Frame.SizeC -> 1) -> ScrewOp(),
    (LaserPump -> 4) -> ScrewOp(),
    (Frame.SizeB -> 2) -> ScrewOp(),
    (ShipPowerAdapter -> 1) -> ScrewOp(),
    (Screw.TypeIIB -> 12) -> ScrewOp(),
    (Screw.TypeIB -> 18) -> ScrewOp()
    )
}

case object CryoCasket extends ItemKind {
   val name = "CryoCasket"
   val description = "A top-of-the-line system for maintaining a human alive at low temperature"
   val parts = Seq(
    (Refrigerator -> 2) -> ScrewOp(), 
    (Frame.SizeD -> 4) -> ScrewOp(), 
    (Screw.TypeIA -> 2) -> ScrewOp()
    )
}

case object HoloNote extends ItemKind {
  val name = "holonote"
  val description = "A small cube that projects a 3D holographic message. A novelty, really, it's much easier to just write an e-mail."
  val parts = Seq(
    (RechargeableBattery -> 1) -> ScrewOp(),
    (HolographicProjector -> 1) -> ScrewOp(),
    (SmallPlasticCasing -> 1) -> ScrewOp(),
    (Microprocessor -> 1) -> ScrewOp(),
    (MRAMChip -> 1) -> ScrewOp(),
    (Screw.TypeIA -> 6) -> ScrewOp()
  )
}

case object HolographicProjector extends ItemKind {
  val name = "holographic projector"
  val description = "A laser array and an assortment of spinning mirrors, capable of producing an ethereal full-color 3-dimensional image."
  val parts = Seq(
    (LaserDiode -> 3) -> ScrewOp(), 
    (FlatMirror -> 1) -> ScrewOp(), 
    (TinyDCMotor -> 1) -> ScrewOp())
}

case object Backpack extends ItemKind {
  val name = "backpack"
  val description = "A light textile bag that straps close to the body. It has a lot of pockets."
  val parts = Seq(
    (SyntheticFabric -> 4) -> CutOp(), 
    (NylonThread -> 2) -> CutOp(), 
    (Fastener -> 2) -> PryOp()
    )
}

case object FlatMirror extends ItemKind {
  val name = "mirror"
  val description = "A small mirror. Your face looks strange, as if it belongs to someone else."
  val parts = Seq()
}
