package adrift.display

import adrift.items.Message
import adrift.items.behaviors.{HasPorts, LayerSet}
import adrift.{Color, GameState, Location, OnFloor, Rect}
import org.lwjgl.glfw.GLFW._
import layout._

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

  private var cursor = state.player.xy

  val powerColor = Color(1, 0, 0, 1)
  val dataColor = Color(0, 1, 0, 1)
  val fluidColor = Color(0, 0, 1, 1)

  override def key(key: Int, scancode: Int, action: Int, mods: Int): Unit = {
    (action, key) match {
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
    draw(renderer, hbox(
      children = Seq(
        custom(renderCableOverlay),
        renderSidebar()
      ),
      bounds = display.screenRect
    ))
  }

  def renderSidebar(): Box = vbox(
    size = 20,
    children = Seq(
      frame(size = 5, contents = vbox(
        children = Seq(
          hbox(
            children = Seq(
              text(
                (0 until 8).map(i => if (displayedPowerLayers(i)) "\u00fe" else "\u00ff").mkString(""),
                size = 8,
                foreground = powerColor
              ),
              text(" "),
              htext("Power", size = 0, foreground = if (displaying == Power) lightGreen else null)
            )
          ),
          hbox(
            children = Seq(
              text(
                (0 until 8).map(i => if (displayedDataLayers(i)) "\u00fe" else "\u00ff").mkString(""),
                size = 8,
                foreground = dataColor
              ),
              text(" "),
              htext("Data", size = 0, foreground = if (displaying == Data) lightGreen else null)
            )
          ),
          hbox(
            children = Seq(
              text(
                (0 until 8).map(i => if (displayedFluidLayers(i)) "\u00fe" else "\u00ff").mkString(""),
                size = 8,
                foreground = fluidColor
              ),
              text(" "),
              htext("Fluid", size = 0, foreground = if (displaying == Fluid) lightGreen else null)
            )
          )
        )
      )),
      frame(contents = vbox(
        children = {
          val items = state.items.lookup(OnFloor(Location(state.player.levelId, cursor)))
          var c = Seq.empty[Box]
          for (item <- items) {
            val hp = item.behaviors.find(_.isInstanceOf[HasPorts]).map(_.asInstanceOf[HasPorts])
            hp match {
              case Some(hp) =>
                c :+= htext(state.itemDisplayName(item))
                for (p <- hp.ports) {
                  val cs = hp.connections.getOrElse(p.name, LayerSet.empty)
                  c :+= hbox(children = Seq(text(" " +
                    (0 until 8).map(i => if (cs(i)) "\u00fe" else "\u00ff").mkString(""),
                    foreground = p.`type` match {
                      case "data-in" | "data-out" => dataColor
                      case "power-in" | "power-out" => powerColor
                      case "fluid-in" | "fluid-out" => fluidColor
                    }
                  ), htext(s" ${p.name}")), size = 1)
                }
              case None =>
            }
          }
          c
        }
      ))
    )
  )

  def renderCableOverlay(renderer: GlyphRenderer, bounds: Rect): Unit = {
    val displayingLayers = displaying match {
      case Power => displayedPowerLayers
      case Data => displayedDataLayers
      case Fluid => displayedFluidLayers
    }
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
      val queryTypes = displaying match {
        case Power => Seq("power-in", "power-out")
        case Data => Seq("data-in", "data-out")
        case Fluid => Seq("fluid-in", "fluid-out")
      }
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
