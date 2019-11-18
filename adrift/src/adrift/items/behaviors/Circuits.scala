package adrift.items.behaviors

import adrift.GameState
import adrift.items.{Behavior, Item, Message}

trait CircuitPort {
    def setSignal(signal: CircuitSignal): Unit
    def getSignal: CircuitSignal
}

case class CircuitSignal(signal: Int) extends Message {
  def +(cs: CircuitSignal): CircuitSignal = CircuitSignal(cs.signal + signal)
  def -(cs: CircuitSignal): CircuitSignal = CircuitSignal(signal - cs.signal)
  def *(cs: CircuitSignal): CircuitSignal = CircuitSignal(cs.signal * signal)
  def /(cs: CircuitSignal): CircuitSignal = CircuitSignal((signal.toDouble / cs.signal.toDouble).round.toInt)
  def &(cs: CircuitSignal): CircuitSignal = CircuitSignal(this.toBin && cs.toBin)
  def |(cs: CircuitSignal): CircuitSignal = CircuitSignal(this.toBin || cs.toBin)
  def ^(cs: CircuitSignal): CircuitSignal = CircuitSignal(this.toBin ^ cs.toBin)
  def not(): CircuitSignal = CircuitSignal(!this.toBin)
  def avg(cs: CircuitSignal): CircuitSignal = CircuitSignal((signal + cs.signal)/2)
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
  inputs: Seq[CircuitPort],
  outputs: Seq[CircuitPort],
  sigBlocks: Seq[Seq[SignalBlock]]
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

case class SigMul() extends SignalBlock {
  val inputBuffers = Seq(CircuitBuffer(), CircuitBuffer())
  val outputBuffers = Seq(CircuitBuffer())
  def evaluate(): Unit = {
    outputBuffers(0).setSignal(inputBuffers(0).signal * inputBuffers(1).signal)
  }
}

case class SigDiv() extends SignalBlock {
  val inputBuffers = Seq(CircuitBuffer(), CircuitBuffer())
  val outputBuffers = Seq(CircuitBuffer())
  def evaluate(): Unit = {
    outputBuffers(0).setSignal(inputBuffers(0).signal / inputBuffers(1).signal)
  }
}

case class SigAnd() extends SignalBlock {
  val inputBuffers = Seq(CircuitBuffer(), CircuitBuffer())
  val outputBuffers = Seq(CircuitBuffer())
  def evaluate(): Unit = {
    outputBuffers(0).setSignal(inputBuffers(0).signal & inputBuffers(1).signal)
  }
}

case class SigOr() extends SignalBlock {
  val inputBuffers = Seq(CircuitBuffer(), CircuitBuffer())
  val outputBuffers = Seq(CircuitBuffer())
  def evaluate(): Unit = {
    outputBuffers(0).setSignal(inputBuffers(0).signal | inputBuffers(1).signal)
  }
}

case class SigXor() extends SignalBlock {
  val inputBuffers = Seq(CircuitBuffer(), CircuitBuffer())
  val outputBuffers = Seq(CircuitBuffer())
  def evaluate(): Unit = {
    outputBuffers(0).setSignal(inputBuffers(0).signal ^ inputBuffers(1).signal)
  }
}

case class SigNand() extends SignalBlock {
  val inputBuffers = Seq(CircuitBuffer(), CircuitBuffer())
  val outputBuffers = Seq(CircuitBuffer())
  def evaluate(): Unit = {
    outputBuffers(0).setSignal((inputBuffers(0).signal & inputBuffers(1).signal).not)
  }
}

case class SigNot() extends SignalBlock {
  val inputBuffers = Seq(CircuitBuffer())
  val outputBuffers = Seq(CircuitBuffer())
  def evaluate(): Unit = {
    outputBuffers(0).setSignal(inputBuffers(0).signal.not)
  }
}

case class SigFork() extends SignalBlock {
  val inputBuffers = Seq(CircuitBuffer())
  val outputBuffers = Seq(CircuitBuffer(), CircuitBuffer())
  def evaluate(): Unit = {
    outputBuffers(0).setSignal(inputBuffers(0).signal)
    outputBuffers(1).setSignal(inputBuffers(0).signal)
  }
}

case class SigFork3() extends SignalBlock {
  val inputBuffers = Seq(CircuitBuffer())
  val outputBuffers = Seq(CircuitBuffer(), CircuitBuffer(), CircuitBuffer())
  def evaluate(): Unit = {
    outputBuffers(0).setSignal(inputBuffers(0).signal)
    outputBuffers(1).setSignal(inputBuffers(0).signal)
    outputBuffers(2).setSignal(inputBuffers(0).signal)
  }
}

case class SigThru() extends SignalBlock {
  val inputBuffers = Seq(CircuitBuffer())
  val outputBuffers = Seq(CircuitBuffer())
  def evaluate(): Unit = {
    outputBuffers(0).setSignal(inputBuffers(0).signal)
  }
}

case class SigThru2() extends SignalBlock {
  val inputBuffers = Seq(CircuitBuffer(), CircuitBuffer())
  val outputBuffers = Seq(CircuitBuffer(), CircuitBuffer())
  def evaluate(): Unit = {
    outputBuffers(0).setSignal(inputBuffers(0).signal)
    outputBuffers(1).setSignal(inputBuffers(1).signal)
  }
}

case class SigThru3() extends SignalBlock {
  val inputBuffers = Seq(CircuitBuffer(), CircuitBuffer(), CircuitBuffer())
  val outputBuffers = Seq(CircuitBuffer(), CircuitBuffer(), CircuitBuffer())
  def evaluate(): Unit = {
    outputBuffers(0).setSignal(inputBuffers(0).signal)
    outputBuffers(1).setSignal(inputBuffers(1).signal)
    outputBuffers(2).setSignal(inputBuffers(2).signal)
  }
}