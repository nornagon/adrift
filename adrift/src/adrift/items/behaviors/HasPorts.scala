package adrift.items.behaviors

import adrift.items.{Behavior, Item, Message}
import adrift.{GameState, GasComposition, LevelId, Location, OnFloor}

import scala.collection.mutable

case class PortSpec(
  `type`: String,
  name: String
)

class LayerSet(val bits: Int) extends AnyVal { self =>
  def nonEmpty: Boolean = bits != 0

  def intersects(other: LayerSet): Boolean = (bits & other.bits) != 0
  def union(other: LayerSet): LayerSet = new LayerSet(bits | other.bits)
  def intersection(other: LayerSet): LayerSet = new LayerSet(bits & other.bits)
  def toggle(layer: Int): LayerSet = new LayerSet(bits ^ (1 << layer))
  def apply(layer: Int): Boolean = (bits & (1 << layer)) != 0

  def iterator: Iterator[Int] = (0 until 8).view.filter(self(_)).iterator
}

object LayerSet {
  def empty = new LayerSet(0)
  def all = new LayerSet(0xff)
  def apply(bits: Int) = new LayerSet(bits)
}

case class HasPorts(ports: Seq[PortSpec], var connections: Map[String, LayerSet] = Map.empty) extends Behavior {
  override def receive(
    state: GameState,
    self: Item,
    message: Message
  ): Unit = message match {
    case m @ Message.IsConnected(cableType, layers, _) =>
      m.connected ||= connections.exists {
        case (portName, connectedLayers) =>
          connectedLayers.intersects(layers) &&
            ports.find(_.name == portName).exists(_.`type` == cableType)
      }
    case m: Message.ChargeAvailable =>
      // TODO: this will infinite-loop if a device's power-in and power-out are connected to the same network.
      for (item <- connectedItems(state, self, portLayers("power-in"), "power-out"))
        state.sendMessage(item, m)
      // TODO: stop early if the charge is found?
    case m: Message.DrawCharge =>
      if (state.isFunctional(self))
        for (item <- connectedItems(state, self, portLayers("power-in"), "power-out"))
          state.sendMessage(item, m)

    case m: Message.SendDataPacket =>
      if (state.isFunctional(self)) {
        val outLayers = portLayers(p => p.`type` == "data-out" && p.name == m.outPort)
        for (item <- connectedItems(state, self, outLayers, "data-in")) {
          state.sendMessage(item, Message.DataPacket(outLayers, m.value))
        }
      }

    case m: Message.DataPacket =>
      if (state.isFunctional(self)) {
        for ((portName, layers) <- connections; if layers.intersects(m.layers)) {
          // got a packet on |portName|
          state.sendMessage(self, Message.ReceivedDataPacket(portName, m.value))
        }
      }

    case m: Message.PushFluid =>
      if (state.isFunctional(self)) {
        val outLayers = portLayers(p => p.`type` == "fluid-out" && p.name == m.outPort)
        val outs = connectedItems(state, self, outLayers, "fluid-in").toSeq
        val perOut = m.mixture.map { case (k, v) => k -> (v / outs.size) }
        val remainder = mutable.Map.empty[String, Float].withDefaultValue(0)
        for (item <- outs) {
          val remaining = state.sendMessage(item, Message.ReceiveFluid(outLayers, perOut)).mixture
          for ((k, v) <- remaining; if v != 0)
            remainder(k) += v
        }
        m.mixture = remainder.toMap
      }

    case m: Message.ReceiveFluid =>
      if (state.isFunctional(self)) {
        val ports = (for ((portName, layers) <- connections; if layers.intersects(m.layers)) yield portName).toSeq
        val mixturePerPort = m.mixture.map { case (k, v) => k -> (v / ports.size) }
        val remainder = mutable.Map.empty[String, Float].withDefaultValue(0)
        for (port <- ports) {
          val remaining = state.sendMessage(self, Message.ReceivedFluid(port, mixturePerPort)).mixture
          for ((k, v) <- remaining; if v != 0)
            remainder(k) += v
        }
        m.mixture = remainder.toMap
      }

    case m: Message.TotalPressure =>
      if (state.isFunctional(self)) {
        val ports =
          (for {
            (portName, layers) <- connections
            if layers.intersects(LayerSet(1 << m.layer))
            spec <- this.ports.find(p => p.name == portName)
            if spec.`type` == "fluid-in" || spec.`type` == "fluid-out"
          } yield portName).toSeq
        for (port <- ports) {
          val pressureOnThisPort = state.sendMessage(self, Message.GetPressure(port, None)).totalPressure
          pressureOnThisPort match {
            case Some((pressure, volume)) =>
              m.totalAmountOfSubstance += pressure * volume
              m.totalVolume += volume
            case None =>
              // port does not contribute to pressure network
          }
        }
      }

    case m: Message.AdjustPressure =>
      if (state.isFunctional(self)) {
        val ports =
          (for {
            (portName, layers) <- connections
              if layers.intersects(LayerSet(1 << m.layer))
              spec <- this.ports.find(p => p.name == portName)
              if spec.`type` == "fluid-in" || spec.`type` == "fluid-out"
          } yield portName).toSeq
        for (port <- ports) {
          state.sendMessage(self, Message.AdjustPressureOnPort(port, m.averagePressure, m.t))
        }
      }


    case _ =>
  }

  def modifyConnections(state: GameState, portName: String, modify: LayerSet => LayerSet): Unit = {
    val existingLayers = connections.getOrElse(portName, LayerSet.empty)
    val newLayers = modify(existingLayers)
    connections += portName -> newLayers
    state.recalculateConnections()
  }

  /** @return set of layers which match pred */
  def portLayers(pred: PortSpec => Boolean): LayerSet =
    ports.filter(pred).map {
      case PortSpec(_, name) => connections.getOrElse(name, LayerSet.empty)
    }.foldLeft(LayerSet.empty)(_ union _)

  def portLayers(connType: String): LayerSet = portLayers(_.`type` == connType)

  def connectedItems(state: GameState, self: Item, layers: LayerSet, connType: String): Iterator[Item] = {
    val location = state.getItemTile(self)

    val conns = connType match {
      case "data-in" | "data-out" => state.dataLayerConnections
      case "power-in" | "power-out" => state.powerLayerConnections
      case "fluid-in" | "fluid-out" => state.fluidLayerConnections
    }

    connectedLocations(conns, location, layers).flatMap {
      connectedLocation =>
        val itemsHere = state.items.lookup(OnFloor(Location(location.levelId, connectedLocation)))
        for {
          item <- itemsHere.view
          if item != self
          if state.sendMessage(item, Message.IsConnected(connType, layers)).connected
        } yield item
    }
  }

  private def connectedLocations(conns: Map[LevelId, Array[Map[(Int, Int), Set[(Int, Int)]]]], location: Location, layers: LayerSet): Iterator[(Int, Int)] =
    layers.iterator
      .flatMap(i => conns(location.levelId)(i).getOrElse(location.xy, Set.empty))
      .distinct
}
