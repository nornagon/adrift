package adrift.display

import adrift._
import adrift.display.CP437.BoxDrawing
import adrift.items.{Item, Message, behaviors}
import org.lwjgl.glfw.GLFW._

import scala.collection.mutable

object CP437 {
  object BoxDrawing {
    // The CP437 box drawing characters are arranged in this ridiculous way because on the original IBM PC MDA adapter,
    // characters were 8x8 but the display put a pixel of space between them. So David J. Bradley, Andy Saenz and Lew
    // Eggebrecht, in their infinite wisdom, decided to hardcode, _into the graphics card_, that when displaying
    // characters in the range 0xC0-0xDF, the rightmost column of pixels would be duplicated into the 9th column on the
    // screen, to avoid a gap in the line.
    val LURD = 0xc0 + 5
    val LUR_ = 0xc0 + 1
    val LU_D = 0xb0 + 4
    val L_RD = 0xc0 + 2
    val _URD = 0xc0 + 3
    val __RD = 0xd0 + 10
    val _UR_ = 0xc0 + 0
    val LU__ = 0xd0 + 9
    val L__D = 0xb0 + 15
    val L_R_ = 0xc0 + 4
    val _U_D = 0xb0 + 3
  }
  val Hearts = 3
  val Diamonds = 4
  val Clubs = 5
  val Spades = 6
  val Bullet = 7
  val LightShade = 11*16
  val Delta = 235
}

sealed trait Dir {
  def opposite: Dir
}
object Dir {
  case object N extends Dir { override def opposite: Dir = S }
  case object NE extends Dir { override def opposite: Dir = SW }
  case object E extends Dir { override def opposite: Dir = W }
  case object SE extends Dir { override def opposite: Dir = NW }
  case object S extends Dir { override def opposite: Dir = N }
  case object SW extends Dir { override def opposite: Dir = NE }
  case object W extends Dir { override def opposite: Dir = E }
  case object NW extends Dir { override def opposite: Dir = SE }

  /** dir of b from a */
  def from(a: (Int, Int), b: (Int, Int)): Dir = {
    if (a._1 < b._1) {
      if (a._2 < b._2) NW
      else if (a._2 == b._2) W
      else SW
    } else if (a._1 == b._1) {
      if (a._2 < b._2) N
      else S
    } else {
      if (a._2 < b._2) NE
      else if (a._2 == b._2) E
      else SE
    }
  }
}

object Appearance {
  def charForTerrain(state: GameState, terrain: Terrain): (Char, Color, Color) = {
    val (char, fg, bg, _) = state.data.display.getDisplay(terrain.display)
    (char, fg, bg)
  }

  def charForWall(center: Terrain, connectLeft: Boolean, connectUp: Boolean, connectRight: Boolean, connectDown: Boolean, viewingFrom: Dir): Char = {
    import CP437.BoxDrawing._
    import Dir._
    ((connectLeft, connectUp, connectRight, connectDown) match {
      case (false, false, false, false) => 254
      case (false, false, false, true)
        | (false, true, false, false)
        | (false, true, false, true) => _U_D
      case (false, false, true, false)
        | (true, false, false, false)
        | (true, false, true, false) => L_R_
      case (false, false, true, true) => __RD
      case (false, true, true, false) => _UR_
      case (true, true, false, false) => LU__
      case (true, false, false, true) => L__D
      case (true, true, true, false) =>
        viewingFrom match {
          case NE | N | E => _UR_
          case NW | W => LU__
          case SW | S | SE => L_R_
        }
      case (true, true, false, true) =>
        viewingFrom match {
          case NW | N | W => LU__
          case SW | S => L__D
          case E | NE | SE => _U_D
        }
      case (true, false, true, true) =>
        viewingFrom match {
          case SW | S | W => L__D
          case SE | E => __RD
          case N | NW | NE => L_R_
        }
      case (false, true, true, true) =>
        viewingFrom match {
          case NE | N | E => _UR_
          case SE | S => __RD
          case W | NW | SW => _U_D
        }
      case (true, true, true, true) =>
        viewingFrom match {
          case NE | N => _UR_
          case SE | E => __RD
          case NW | W => LU__
          case SW | S => L__D
        }
    }).toChar
  }

  def charForCable(state: GameState, unrolledItem: Item): (Char, Color, Color) = {
    val OnFloor(Location(_, x, y)) = state.items.lookup(unrolledItem)
    val unrolled = unrolledItem.behaviors.collectFirst { case u: behaviors.Unrolled => u }.get
    val prevDir = Dir.from(unrolled.fromCell, (x, y))
    val nextDir = unrolled.toCell.map(Dir.from(_, (x, y))).getOrElse(prevDir.opposite)
    import CP437.BoxDrawing._
    import Dir._
    val char = (prevDir, nextDir) match {
      case (N, S) | (S, N) => _U_D
      case (E, W) | (W, E) => L_R_
      case (N, E) | (E, N) => _UR_
      case (N, W) | (W, N) => LU__
      case (S, W) | (W, S) => L__D
      case (S, E) | (E, S) => __RD
      case _ => ???
    }
    val (_, fg, bg, _) = charForItem(state, unrolledItem)
    (char.toChar, fg, bg)
  }

  def canonicalCharForItem(state: GameState, item: Item): (Char, Color, Color, Int) =
    state.data.display.getDisplay(item.kind.display)
  def charForItem(state: GameState, item: Item): (Char, Color, Color, Int) =
    state.data.display.getDisplay(displayForItem(state, item))
  def displayForItem(state: GameState, item: Item): String =
    state.sendMessage(item, Message.Display(item.kind.display)).display

  def charAtPosition(state: GameState, x: Int, y: Int): (Char, Color, Color) =
    charAtPosition(state, Location(state.player.levelId, x, y))
  def charAtPosition(state: GameState, loc: Location): (Char, Color, Color) = {
    val levelId = loc.levelId
    val level = state.levels(levelId)
    val (x, y) = loc.xy
    if (loc == state.player) {
      val (char, fg, bg, _) = state.data.display.getDisplay("PLAYER")
      (char, fg, bg)
    } else if (level.terrain.contains(x, y)) {
      val items = state.items.lookup(OnFloor(Location(levelId, x, y)))
        .filter(item => displayForItem(state, item) != "INVISIBLE")
      if (items.nonEmpty) {
        // reversed so that we get the _last_ item in the list that has the highest layer instead of the _first_.
        // stuff you drop later should be displayed on top.
        val topItem = items.reverse
          .maxBy(charForItem(state, _)._4)
        if (topItem.behaviors.exists(_.isInstanceOf[behaviors.Unrolled]))
          charForCable(state, topItem)
        else {
          val (char, fg, bg, _) = charForItem(state, topItem)
          (char, fg, bg)
        }
      } else {
        val terrain = level.terrain(x, y)

        if (terrain.connects) {
          def apparent(x: Int, y: Int): Option[Terrain] = {
            if (state.broadcastToLocation(OnFloor(Location(levelId, x, y)), Message.DisplayConnectedTo(terrain)).connected)
              Some(terrain)
            else
              level.terrain.get(x, y)
          }

          val left = apparent(x - 1, y)
          val up = apparent(x, y - 1)
          val right = apparent(x + 1, y)
          val down = apparent(x, y + 1)
          val viewedFrom = Dir.from((state.player.x, state.player.y), (x, y))
          val (_, fg, bg, _) = state.data.display.getDisplay(terrain.display)
          (charForWall(terrain,
            terrain == left.orNull,
            terrain == up.orNull,
            terrain == right.orNull,
            terrain == down.orNull,
            viewedFrom), fg, bg)
        } else {
          charForTerrain(state, terrain)
        }
      }
    } else (' ', Color.Black, Color.Black)
  }

  def messageAtCell(state: GameState, location: Location): String = {
    val level = state.levels(location.levelId)
    val items = state.items.lookup(OnFloor(location))
    val debug =
      if (state.showTempDebug)
        f" (${level.temperature(location.x, location.y) - 273}%.1f C)"
      else if (state.showGasDebug)
        s" (${level.gasComposition(location.x, location.y)})"
      else
        ""
    (if (items.nonEmpty) {
      s"Here: ${items.last.kind.name}" + (if (items.size > 1) s" and ${items.size - 1} other thing${if (items.size == 1) "" else "s"}" else "")
    } else {
      level.terrain(location.x, location.y).name
    }) + debug
  }
}

trait Screen {
  def key(key: Int, scancode: Int, action: Int, mods: Int): Unit
  def char(char: Int): Unit = {}
  def render(renderer: GlyphRenderer): Unit
}

class GLFWDisplay(val window: GLFWWindow, val font: Font) extends Display {
  private var lastState: GameState = _
  private val pendingActions = new java.util.concurrent.ConcurrentLinkedQueue[Action]
  val windowWidthChars = 80
  val windowHeightChars = 60
  private val screens = mutable.ListBuffer.empty[Screen]

  private [display] def pushScreen(screen: Screen): Unit = {
    screens.append(screen)
    render(lastState)
  }
  private [display] def popScreen(): Unit = {
    if (screens.nonEmpty) screens.remove(screens.size - 1)
    render(lastState)
  }
  private [display] def pushAction(a: Action): Unit = {
    pendingActions.add(a)
  }

  private var defaultPrevented = false
  def preventDefault(): Unit = defaultPrevented = true

  def init(): Unit = {
    window.init(width = windowWidthChars * font.tileWidth * font.scaleFactor, height = windowHeightChars * font.tileHeight * font.scaleFactor)
    window.onChar { char: Int =>
      if (screens.nonEmpty)
        screens.last.char(char)
    }
    window.onKey { (key: Int, scancode: Int, action: Int, mods: Int) =>
      if (screens.nonEmpty) {
        defaultPrevented = false
        screens.last.key(key, scancode, action, mods)
        if (key == GLFW_KEY_ESCAPE && action == GLFW_PRESS && !defaultPrevented) {
          popScreen()
        } else {
          render(lastState)
        }
      } else {
        if (action == GLFW_PRESS || action == GLFW_REPEAT) {
          key match {
            case GLFW_KEY_LEFT | GLFW_KEY_H => pushAction(Action.PlayerMove(-1, 0))
            case GLFW_KEY_DOWN | GLFW_KEY_J => pushAction(Action.PlayerMove(0, 1))
            case GLFW_KEY_UP | GLFW_KEY_K => pushAction(Action.PlayerMove(0, -1))
            case GLFW_KEY_RIGHT | GLFW_KEY_L => pushAction(Action.PlayerMove(1, 0))
            case GLFW_KEY_Y => pushAction(Action.PlayerMove(-1, -1))
            case GLFW_KEY_U => pushAction(Action.PlayerMove(1, -1))
            case GLFW_KEY_B => pushAction(Action.PlayerMove(-1, 1))
            case GLFW_KEY_N => pushAction(Action.PlayerMove(1, 1))
            case GLFW_KEY_D =>
              lastState.items.lookup(InHands()).headOption foreach { item =>
                pushAction(Action.PutDown(item))
              }
            case GLFW_KEY_PERIOD => pushAction(Action.Wait(1))
            case GLFW_KEY_COMMA => pushAction(Action.Wait(100))
            case GLFW_KEY_E => pushScreen(new ExamineDirectionScreen(this, lastState))
            case GLFW_KEY_A => pushScreen(new AssemblyScreen(this, lastState))
            case GLFW_KEY_GRAVE_ACCENT => pushScreen(new WishScreen(this, lastState))
            case GLFW_KEY_SEMICOLON => pushScreen(new LookScreen(this, lastState))
            case _ if lastState.isRoomTest =>
              key match {
                case GLFW_KEY_R if (mods & GLFW_MOD_SUPER) != 0 =>
                  pushAction(Action.Regenerate)
                case _ =>
              }
            case _ =>
          }
        }
      }
    }
    window.onClose { () =>
      pushAction(Action.Quit)
    }
  }

  def worldToScreen(state: GameState)(worldCoords: (Int, Int)): Option[(Int, Int)] = {
    val wr = worldRect(state)
    val (wx, wy) = worldCoords
    val offX = wx - wr.l
    val offY = wy - wr.t
    if (offX >= 0 && offY >= 0 && offX < wr.width && offY < wr.height)
      Some((offX, offY))
    else
      None
  }

  val sidebarWidth = 20
  val screenRect = Rect(0, 0, windowWidthChars, windowHeightChars)
  val Seq(worldViewRect, sidebarRect) = screenRect.cutHorizontal(Seq(0, screenRect.width - sidebarWidth, screenRect.width)).to(Seq)

  def worldRect(state: GameState): Rect = {
    Rect(
      l = state.player.x - worldViewRect.width / 2,
      t = state.player.y - worldViewRect.height / 2,
      r = state.player.x - worldViewRect.width / 2 + worldViewRect.width,
      b = state.player.y - worldViewRect.height / 2 + worldViewRect.height
    )
  }

  def render(state: GameState): Unit = {
    val (fbWidth, fbHeight) = window.framebufferSize
    if (fbWidth != windowWidthChars * (font.tileWidth * font.scaleFactor) ||
      fbHeight != windowHeightChars * (font.tileHeight * font.scaleFactor)) {
      window.setSize(
        windowWidthChars * (font.tileWidth * font.scaleFactor),
        windowHeightChars * (font.tileHeight * font.scaleFactor)
      )
      window.center()
    }
    if (!window.isVisible)
      window.show()

    window.render { g =>
      val glyphRenderer = g.glyphs(font)

      renderWorld(state, glyphRenderer, worldRect(state), worldViewRect.l, worldViewRect.t)

      val Seq(top, mid, bot) = sidebarRect.cutVertical(Seq(0, 10, 22, windowHeightChars)).toSeq
      glyphRenderer.drawBox(top)

      state.symptoms.take(5).zipWithIndex.foreach {
        case(symptom,i) => {
          glyphRenderer.drawString(top.l + 1, top.t + i + 1, symptom.description)
        }
      }
//      glyphRenderer.drawString(top.l + 1, top.t + 1, f"BT: ${state.bodyTemp - 273}%.1f C")

      glyphRenderer.drawBox(mid);
      {
        val held = state.items.lookup(InHands())
        var y = 0
        glyphRenderer.drawString(mid.l + 1, mid.t + 1 + y, "Held")
        y += 1
        held.zipWithIndex.foreach {
          case (item, i) =>
            glyphRenderer.drawString(
              mid.l + 2,
              mid.t + 1 + y + i,
              state.itemDisplayName(item),
              maxWidth = mid.width - 4
            )
        }
        y += held.size
        glyphRenderer.drawString(mid.l + 1, mid.t + 1 + y, "Worn")
        y += 1
        val worn = state.items.lookup(Worn())
        worn.zipWithIndex.foreach {
          case (item, i) =>
            glyphRenderer.drawString(
              mid.l + 2,
              mid.t + 1 + y + i,
              state.itemDisplayName(item),
              maxWidth = mid.width - 4
            )
        }
      }

      glyphRenderer.drawBox(bot);
      {
        val maxMessageAge = 300
        val oldestMessageTime = state.currentTime - maxMessageAge
        val wrappedLines = state.messages.filter(_._2 >= oldestMessageTime)
          .flatMap(m => GlyphRenderer.wrapString(bot.width - 2, Integer.MAX_VALUE, m._1).map((_, m._2)))
        val lines = wrappedLines.slice(wrappedLines.size - (bot.height - 2), wrappedLines.size)
        val top = bot.b - 1 - lines.size
        lines.zipWithIndex.foreach {
          case ((line, time), y) =>
            val lineAge = state.currentTime - time
            val color =
              if (lineAge < 30) Color.White
              else if (lineAge < 120) Color(0.5f, 0.5f, 0.5f, 1.0f)
              else Color(0.2f, 0.2f, 0.2f, 1.0f)
            glyphRenderer.drawString(bot.l + 1, top + y, line, fg = color)
        }
      }

      for (screen <- screens) {
        screen.render(glyphRenderer)
      }
    }
  }

  private def renderWorld(
    state: GameState,
    renderer: GlyphRenderer,
    worldRect: Rect,
    screenLeft: Int,
    screenTop: Int,
  ): Unit = {
    val Rect(left, top, right, bottom) = worldRect
    val levelId = state.player.levelId
    for (y <- top until bottom; x <- left until right) {
      if (state.isVisible(Location(levelId, x, y)) || state.seeThroughWalls) {
        val (char, fg, bg) = Appearance.charAtPosition(state, x, y)
        val d: Float =
          if (state.sightRadius > 20) 1f
          else {
            val k = state.sightRadius / 20f
            val pctNoise = (1 - k) * 0.1f
            k * (1 - pctNoise) + math.random().toFloat * pctNoise
          }
        renderer.drawChar(screenLeft + x - left, screenTop + y - top, char, fg.darken(d), bg.darken(d))
      } else {
        state.remembered(Location(levelId, x, y)) foreach { case (char, _, _) =>
          renderer.drawChar(screenLeft + x - left, screenTop + y - top, char, fg = Color(0.0f, 0.1f, 0.05f, 1.0f))
        }
      }

      val level = state.levels(levelId)

      if (state.showGasDebug && level.gasComposition.contains(x, y)) {
        val gc = level.gasComposition(x, y)
        val color = Color(0, 0, gc.oxygen.toFloat / 15, 0.3f)
        renderer.drawChar(screenLeft + x - left, screenTop + y - top, BoxDrawing.LURD, color, bg = Color(0f, 0f, 0f, 0f))
      }

      if (state.showTempDebug && level.temperature.contains(x, y)) {
        val temp = level.temperature(x, y)
        val color = if (temp > 273)
          Color((1 - math.exp(-(temp - 273)/30)).toFloat, 0, 0, 0.3f)
        else
          Color(0, 0, (1 - math.exp((temp - 273)/30)).toFloat, 0.3f)
        renderer.drawChar(screenLeft + x - left, screenTop + y - top, BoxDrawing.LURD, color, bg = Color(0f, 0f, 0f, 0f))
      }
    }
  }

  override def update(state: GameState): Unit = {
    lastState = state
    if (state.isDead) {
      pushScreen(new DeathScreen(this, state))
      return
    }
    render(state)
  }

  override def waitForAction: Action = {
    if (!pendingActions.isEmpty)
      return pendingActions.poll()
    while (pendingActions.isEmpty)
      glfwWaitEvents()
    pendingActions.poll()
  }

  override def postAction(action: Action): Unit = {
    pushAction(action)
    glfwPostEmptyEvent()
  }

  override def running: Boolean = !window.shouldClose
}
