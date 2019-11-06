package adrift.items.behaviors

import adrift.GameState
import adrift.items.{Behavior, Item, Message}

trait CircuitPort {
    def setSignal(signal: CircuitSignal): Unit
    def getSignal: CircuitSignal
}

case object CircuitSignal {
  def add(sig1: CircuitSignal, sig2: CircuitSignal): CircuitSignal = {
    CircuitSignal(clamp(sig1.signal + sig2.signal))
  }
  def sub(sig1: CircuitSignal, sig2: CircuitSignal): CircuitSignal = {
    CircuitSignal(clamp(sig1.signal - sig2.signal))
  }
  def mul(sig1: CircuitSignal, sig2: CircuitSignal): CircuitSignal = {
    CircuitSignal(clamp(sig1.signal * sig2.signal))
  }
  def div(sig1: CircuitSignal, sig2: CircuitSignal): CircuitSignal = {
    CircuitSignal(clamp((sig1.signal.toFloat / sig2.signal.toFloat).toInt))
  }
  def avg(sig1: CircuitSignal, sig2: CircuitSignal): CircuitSignal = {
    CircuitSignal(clamp((sig1.signal + sig2.signal)/2))
  }
  def clamp(value: Int): Int = {
    if (value > 255) {
      255
    }
    else if (value < 0){
      0
    }
    else {
      value.round.toInt
    }
  }

  def toBin(sig: CircuitSignal): Boolean = {
    if(sig.signal > 127){
      true
    } 
    else {
      false
    }
  }

  def toVal(v: Boolean): CircuitSignal = {
    if (v) {
      CircuitSignal(255)
    }
    else { 
      CircuitSignal(0)
    }
  }

  def s_and(sig1: CircuitSignal, sig2: CircuitSignal): CircuitSignal = {
    toVal(toBin(sig1) && toBin(sig2))
  }
  def s_or(sig1: CircuitSignal, sig2: CircuitSignal): CircuitSignal = {
    toVal(toBin(sig1) || toBin(sig2)) 
  }
  def s_xor(sig1: CircuitSignal, sig2: CircuitSignal): CircuitSignal = {
    toVal((toBin(sig1) ^ toBin(sig2)))
  }
  def s_nand(sig1: CircuitSignal, sig2: CircuitSignal): CircuitSignal = {
    toVal( ! (toBin(sig1) && toBin(sig2)))
  }
  def s_not(sig: CircuitSignal): CircuitSignal = {
    toVal( ! toBin(sig))
  }
}
case class CircuitSignal(signal: Int) extends Message

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
  iPort: CircuitPort,
  oPort: CircuitPort,
  var signal: CircuitSignal) extends CircuitPort {
    def setSignal(sig: CircuitSignal): Unit = {signal = sig}
    def getSignal: CircuitSignal = signal
    def pull: Unit = setSignal(iPort.getSignal)
    def push: Unit = oPort.setSignal(signal)
}

case class SignalBlock() {}