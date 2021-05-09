package adrift.display

import adrift.items.{Item, Message}
import adrift.items.behaviors.{HasPorts, LayerSet, PortSpec}
import adrift.{Color, GameState, Location, OnFloor, Rect}
import org.lwjgl.glfw.GLFW._
import layout3._

object NumericKey {
  def unapply(key: Int): Option[Int] =
    key match {
      case n if n >= GLFW_KEY_0 && n <= GLFW_KEY_9 => Some(n - GLFW_KEY_0)
      case _ => None
    }
}

class CableScreen(display: GLFWDisplay, state: GameState) extends Screen {
  import UI.Color._

  private sealed trait DisplayingType {
    def next: DisplayingType
  }
  private case object Power extends DisplayingType { def next: DisplayingType = Data }
  private case object Data extends DisplayingType { def next: DisplayingType = Fluid }
  private case object Fluid extends DisplayingType { def next: DisplayingType = Power }
  private var displayedPowerLayers = LayerSet.all
  private var displayedDataLayers = LayerSet.all
  private var displayedFluidLayers = LayerSet.all
  private var displaying: DisplayingType = Power
  def queryTypes: Seq[String] = displaying match {
    case Power => Seq("power-in", "power-out")
    case Data => Seq("data-in", "data-out")
    case Fluid => Seq("fluid-in", "fluid-out")
  }
  def displayingLayers: LayerSet = displaying match {
    case Power => displayedPowerLayers
    case Data => displayedDataLayers
    case Fluid => displayedFluidLayers
  }

  private var cursor = state.player.xy

  val powerColor = Color(1, 0, 0, 1)
  val dataColor = Color(0, 1, 0, 1)
  val fluidColor = Color(0, 0, 1, 1)

  var connecting = false
  var connectingCursor = 0

  def portsHere = {
    val items = state.items.lookup(OnFloor(Location(state.player.levelId, cursor)))
    for {
      item <- items
      hp <- item.behaviors.find(_.isInstanceOf[HasPorts]).map(_.asInstanceOf[HasPorts])
      displayedPorts = hp.ports.filter(p => queryTypes.contains(p.`type`))
      if displayedPorts.nonEmpty
    } yield item -> (displayedPorts, hp.connections)
  }

  def editingPort: Option[(Item, String)] = {
    var prev = 0
    for ((item, (ports, connections)) <- portsHere) {
      if (connectingCursor - prev < ports.size)
        return Some((item, ports(connectingCursor - prev).name))
      prev += ports.size
    }
    None
  }

  private def connectingKey(key: Int, scancode: Int, action: Int, mods: Int): Unit = {
    (action, key) match {
      case (GLFW_PRESS, GLFW_KEY_ESCAPE) =>
        display.preventDefault()
        connecting = false
      case (GLFW_PRESS | GLFW_REPEAT, DirectionKey(0, dy)) =>
        connectingCursor = math.max(0, math.min(portsHere.map(_._2._1.size).sum - 1, connectingCursor + dy))
      case (GLFW_PRESS, NumericKey(n)) if n >= 1 && n <= 8 =>
        val Some((item, port)) = editingPort
        val Some(hp) = item.behaviors.find(_.isInstanceOf[HasPorts]).map(_.asInstanceOf[HasPorts])
        val existingLayers = hp.connections.getOrElse(port, LayerSet.empty)
        val newLayers =
          if ((mods & GLFW_MOD_SHIFT) != 0) {
            existingLayers.toggle(n - 1)
          } else {
            new LayerSet(1 << (n - 1))
          }
        hp.connections += port -> newLayers
        state.recalculateConnections()
      case _ =>
    }
  }

  override def key(key: Int, scancode: Int, action: Int, mods: Int): Unit = {
    if (connecting) {
      connectingKey(key, scancode, action, mods)
      return
    }
    (action, key) match {
      case (GLFW_PRESS, GLFW_KEY_C) if portsHere.exists(_._2._1.nonEmpty) =>
        connectingCursor = 0
        connecting = true
      case (GLFW_PRESS | GLFW_REPEAT, DirectionKey(dx, dy)) =>
        cursor = (cursor._1 + dx, cursor._2 + dy)
      case (GLFW_PRESS, GLFW_KEY_SPACE) =>
        val level = state.levels(state.player.levelId)
        val cables = displaying match {
          case Power => level.powerCables
          case Data => level.dataCables
          case Fluid => level.fluidCables
        }
        val displayedLayers = displaying match {
          case Power => displayedPowerLayers
          case Data => displayedDataLayers
          case Fluid => displayedFluidLayers
        }
        if (cables.contains(cursor)) {
          cables(cursor) ^= displayedLayers.bits
          state.recalculateConnections()
        }
      case (GLFW_PRESS, NumericKey(n)) if n >= 1 && n <= 8 =>
        if ((mods & GLFW_MOD_SHIFT) != 0) {
          displaying match {
            case Power =>
              displayedPowerLayers = displayedPowerLayers.toggle(n - 1)
            case Data =>
              displayedDataLayers = displayedDataLayers.toggle(n - 1)
            case Fluid =>
              displayedFluidLayers = displayedFluidLayers.toggle(n - 1)
          }
        } else {
          displaying match {
            case Power =>
              displayedPowerLayers = new LayerSet(1 << (n - 1))
            case Data =>
              displayedDataLayers = new LayerSet(1 << (n - 1))
            case Fluid =>
              displayedFluidLayers = new LayerSet(1 << (n - 1))
          }
        }
      case (GLFW_PRESS, GLFW_KEY_TAB) =>
        displaying = displaying.next
      case _ =>
    }
  }

  override def render(renderer: GlyphRenderer): Unit = {
    renderCableOverlay(renderer, display.screenRect)

    val sb = renderSidebar().inflate.asInstanceOf[RenderBox]
    sb.layout(BoxConstraints.tight(Size(display.sidebarRect.width, display.sidebarRect.height)))
    sb.paint(renderer, Offset(display.sidebarRect.l, display.sidebarRect.t))
  }

  def renderSidebar(): Widget = Column(Seq(
    Border(ConstrainedBox(BoxConstraints(minWidth = Int.MaxValue), Column(
      Seq(
        Text(" 12345678", halfWidth = false),
      ) ++ Seq(
        ("Power", displayedPowerLayers, powerColor, Power),
        ("Data", displayedDataLayers, dataColor, Data),
        ("Fluid", displayedFluidLayers, fluidColor, Fluid),
      ).map {
        case (title, displayedLayers, color, ty) =>
          Row(
            children = Seq(
              Text(
                (" " + (0 until 8).map(i => if (displayedLayers(i)) "\u00fe" else "\u00ff").mkString("")).withFg(color),
                halfWidth = false
              ),
              ConstrainedBox(BoxConstraints(minWidth = 1)),
              Text(title.withFg(if (displaying == ty) lightGreen else Color.White))
            )
          )
      }
    ))),
    Flexible(Border(ConstrainedBox(BoxConstraints(minWidth = Int.MaxValue), Column({
      var c = Seq.empty[Widget]
      var prev = 0
      for ((item, (ports, connections)) <- portsHere) {
        val displayedPorts = ports.filter(p => queryTypes.contains(p.`type`))
        if (displayedPorts.nonEmpty) {
          c :+= Text(state.itemDisplayName(item))
          for ((p, i) <- displayedPorts.zipWithIndex) {
            val ix = prev + i
            val cs = connections.getOrElse(p.name, LayerSet.empty)
            val intersectsWithCurrentView = cs intersects displayingLayers
            c :+= Row(
              children = Seq(
                Text(
                  ((if (connecting && ix == connectingCursor) "\u0010" else " ") +
                    (0 until 8).map(i => if (cs(i)) "\u00fe" else "\u00ff").mkString("")).withFg(
                      (p.`type` match {
                        case "data-in" | "data-out" => dataColor
                        case "power-in" | "power-out" => powerColor
                        case "fluid-in" | "fluid-out" => fluidColor
                      }).darken(if (intersectsWithCurrentView) 1.0f else 0.5f)
                    ),
                  halfWidth = false
                ),
                ConstrainedBox(BoxConstraints(minWidth = 1)),
                Flexible(ConstrainedBox(
                  BoxConstraints(maxHeight = 1),
                  Text(p.name.withFg(Color.White.darken(if (intersectsWithCurrentView) 1.0f else 0.5f)))
                ))
              )
            )
          }
          prev += displayedPorts.size
        }
      }
      if (portsHere.exists(_._2._1.nonEmpty) && !connecting)
        c :+ Spacer() :+ Text("c".withFg(lightGreen) + "onnect".withFg(disabledGreen))
      else
        c
    }))))
  ))

  def renderCableOverlay(renderer: GlyphRenderer, bounds: Rect): Unit = {
    val displayingCableColor = displaying match {
      case Power => powerColor
      case Data => dataColor
      case Fluid => fluidColor
    }

    val levelId = state.player.levelId
    val level = state.levels(levelId)

    for {
      sy <- bounds.l until bounds.r
      sx <- bounds.t until bounds.b
      (x, y) = display.screenToWorld(state)((sx, sy))
      if level.terrain.contains(x, y)
      if state.isVisible(Location(levelId, x, y))
    } {
      val cables = displaying match {
        case Power => level.powerCables
        case Data => level.dataCables
        case Fluid => level.fluidCables
      }
      val layers = new LayerSet(cables(x, y))
      // if there's something with ports here...
      val connected = queryTypes.exists(queryType =>
        state.broadcastToLocation(OnFloor(Location(levelId, x, y)), Message.IsConnected(queryType, LayerSet.all)).connected)
      if (connected) {
        // Is it connected to a cable?
        val color = if (layers.intersects(displayingLayers)) displayingCableColor else Color.White
        renderer.drawChar(bounds.l + sx, bounds.t + sy, 8, fg = color)
      } else if (layers.nonEmpty) {
        val connectLeft = cables.contains(x - 1, y) && ((cables(x - 1, y) & layers.bits) != 0)
        val connectUp = cables.contains(x, y - 1) && ((cables(x, y - 1) & layers.bits) != 0)
        val connectRight = cables.contains(x + 1, y) && ((cables(x + 1, y) & layers.bits) != 0)
        val connectDown = cables.contains(x, y + 1) && ((cables(x, y + 1) & layers.bits) != 0)
        val color = if (layers.intersects(displayingLayers)) displayingCableColor else Color(0.5f, 0.5f, 0.5f, 1.0f)
        renderer.drawChar(bounds.l + sx, bounds.t + sy, Appearance.charForConnection(connectLeft, connectUp, connectRight, connectDown), color)
      }
    }

    display.worldToScreen(state)(cursor) foreach { cursorScreenPos =>
      renderer.drawChar(cursorScreenPos._1, cursorScreenPos._2, 0xce, UI.Color.lightGreen)
    }
  }
}
