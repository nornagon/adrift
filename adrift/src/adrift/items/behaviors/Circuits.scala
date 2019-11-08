package adrift.items.behaviors

import adrift.GameState
import adrift.items.{Behavior, Item, Message}

trait CircuitPort {
    def setSignal(signal: CircuitSignal): Unit
    def getSignal: CircuitSignal
}

case class CircuitSignal(signal: Int) extends Message {
  def +(cs: CircuitSignal): CircuitSignal = CircuitSignal(cs.signal + signal).clamp
  def -(cs: CircuitSignal): CircuitSignal = CircuitSignal(signal - cs.signal).clamp
  def *(cs: CircuitSignal): CircuitSignal = CircuitSignal(cs.signal * signal).clamp
  def /(cs: CircuitSignal): CircuitSignal = CircuitSignal((signal.toDouble / cs.signal.toDouble).round.toInt).clamp
  def &(cs: CircuitSignal): CircuitSignal = CircuitSignal(this.toBin && cs.toBin)
  def |(cs: CircuitSignal): CircuitSignal = CircuitSignal(this.toBin || cs.toBin)
  // def ^(cs: CircuitSignal): CircuitSignal = BoolToCS(signal.toBin || cs.toBin)
  def clamp(value: Int): Int = {
    if (value > 255) {255}
    else if (value < 0){0}
    else {value.round.toInt}
  }
  def clamp(): CircuitSignal = CircuitSignal(clamp(signal))
  def SigToBin(value: Int): Boolean = {
    if(value > 127){true} 
    else {false}
  }
  def toBin(): Boolean = SigToBin(signal)
//   def avg(sig1: CircuitSignal, sig2: CircuitSignal): CircuitSignal = {
//     CircuitSignal(clamp((sig1.signal + sig2.signal)/2))
//   }
}

case object CircuitSignal {
  def apply(i: Int): CircuitSignal = CircuitSignal(i).clamp
  def apply(b: Boolean): CircuitSignal = BoolToSig(b)
  def BoolToSig(v: Boolean): CircuitSignal = {
    if (v) {CircuitSignal(255)}
    else {CircuitSignal(0)}
  }
}


case class CircuitBoard(
  inputs: List[CircuitPort],
  outputs: List[CircuitPort],
  sigBlocks: List[List[SignalBlock]]
  ) extends Behavior {
  override def receive(
    state: GameState,
    self: Item,
    message: Message
  ): Unit = message match {
    case d: CircuitSignal => {}
  }
}


case class CircuitBuffer(
  var iPort: Option[CircuitPort],
  var oPort: Option[CircuitPort],
  var signal: CircuitSignal) extends CircuitPort {
    def setSignal(sig: CircuitSignal): Unit = {signal = sig}
    def getSignal: CircuitSignal = signal
    def pull: Unit = iPort match {
      case None => throw new Exception("no input port to pull signal from")
      case Some(cp) => setSignal(cp.getSignal)
    }
    def push: Unit = oPort match {
      case None => throw new Exception("no output port to push signal to")
      case Some(cp) => cp.setSignal(signal)
    }
    def setIPort(p: CircuitPort) = {
      iPort = Some(p)
    }
    def setOPort(p: CircuitPort) = {
      oPort = Some(p)
    }
  }
case object CircuitBuffer {
  def apply() = new CircuitBuffer(None,None,new CircuitSignal(0))
}


trait SignalBlock {
  val inputBuffers: Seq[CircuitBuffer]
  val outputBuffers: Seq[CircuitBuffer]
  def evaluate: Unit
  def update: Unit = {
    inputBuffers.map(b => b.pull)
    evaluate
    outputBuffers.map(b => b.push)
  }
}

case class SigAdd() extends SignalBlock {
  val inputBuffers = Seq(CircuitBuffer(), CircuitBuffer())
  val outputBuffers = Seq(CircuitBuffer())
  def evaluate(): Unit = {
    outputBuffers(0).setSignal(inputBuffers(0).signal + inputBuffers(1).signal)
  }
}

case class SigSub() extends SignalBlock {
  val inputBuffers = Seq(CircuitBuffer(), CircuitBuffer())
  val outputBuffers = Seq(CircuitBuffer())
  def evaluate(): Unit = {
    outputBuffers(0).setSignal(inputBuffers(0).signal - inputBuffers(1).signal)
  }
}

case class SigAnd() extends SignalBlock {
  val inputBuffers = Seq(CircuitBuffer(), CircuitBuffer())
  val outputBuffers = Seq(CircuitBuffer())
  def evaluate(): Unit = {
    outputBuffers(0).setSignal(inputBuffers(0).signal & inputBuffers(1).signal)
  }
}

// case object SignalBlock {
//     def add(): SignalBlock(2,1,CircuitSignal.add)
//     def sub(): SignalBlock(2,1,CircuitSignal.sub)
//     def mul(): SignalBlock(2,1,CircuitSignal.mul)
//     def div(): SignalBlock(2,1,CircuitSignal.div)
//     def avg(): SignalBlock(2,1,CircuitSignal.avg)
//     def s_and(): SignalBlock(2,1,CircuitSignal.s_and)
//     def s_or(): SignalBlock(2,1,CircuitSignal.s_or)
//     def s_xor(): SignalBlock(2,1,CircuitSignal.s_xor)
//     def s_nand(): SignalBlock(2,1,CircuitSignal.s_nand)
//     def s_not(): SignalBlock(1,1,CircuitSignal.s_not)
//     def thru1(): SignalBlock(1,1,lambda x: x)
//     def thru2(): SignalBlock(2,2,lambda x,y: (x,y))
//     def thru3(): SignalBlock(3,3,lambda x,y,z: (x,y,z))
//     def fork(): SignalBlock(1,2,lambda x: (x,x))
//     def pfrk(): SignalBlock(1,3,lambda x: (x,x,x))
// }
