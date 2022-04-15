package adrift.display

import adrift.*
import adrift.display.CP437.BoxDrawing
import adrift.items.{Item, Message}
import org.lwjgl.glfw.GLFW.*

import scala.collection.mutable
import scala.util.Random

//noinspection TypeAnnotation
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
  case object DN extends Dir { override def opposite: Dir = DN }

  /** dir of b from a */
  def from(a: (Int, Int), b: (Int, Int)): Dir = {
    if (a._1 < b._1) {
      if (a._2 < b._2) NW
      else if (a._2 == b._2) W
      else SW
    } else if (a._1 == b._1) {
      if (a._2 < b._2) N
      else if (a._2 == b._2) DN
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

  def charForWall(connectLeft: Boolean, connectUp: Boolean, connectRight: Boolean, connectDown: Boolean, viewingFrom: Dir): Char = {
    import CP437.BoxDrawing.*
    import Dir.*
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
          case DN => 254
        }
      case (true, true, false, true) =>
        viewingFrom match {
          case NW | N | W => LU__
          case SW | S => L__D
          case E | NE | SE => _U_D
          case DN => 254
        }
      case (true, false, true, true) =>
        viewingFrom match {
          case SW | S | W => L__D
          case SE | E => __RD
          case N | NW | NE => L_R_
          case DN => 254
        }
      case (false, true, true, true) =>
        viewingFrom match {
          case NE | N | E => _UR_
          case SE | S => __RD
          case W | NW | SW => _U_D
          case DN => 254
        }
      case (true, true, true, true) =>
        viewingFrom match {
          case NE | N => _UR_
          case SE | E => __RD
          case NW | W => LU__
          case SW | S => L__D
          case DN => 254
        }
    }).toChar
  }

  def charForConnection(connectLeft: Boolean, connectUp: Boolean, connectRight: Boolean, connectDown: Boolean): Char = {
    import CP437.BoxDrawing.*
    ((connectLeft, connectUp, connectRight, connectDown) match {
      //    Left   Up     Right  Down
      case (false, false, false, false) => 254
      case (false, false, false, true ) => _U_D
      case (false, false, true,  false) => L_R_
      case (false, false, true,  true ) => __RD
      case (false, true , false, false) => _U_D
      case (false, true , false, true ) => _U_D
      case (false, true , true,  false) => _UR_
      case (false, true , true,  true ) => _URD
      case (true , false, false, false) => L_R_
      case (true , false, false, true ) => L__D
      case (true , false, true,  false) => L_R_
      case (true , false, true,  true ) => L_RD
      case (true , true , false, false) => LU__
      case (true , true , false, true ) => LU_D
      case (true , true , true,  false) => LUR_
      case (true , true , true,  true ) => LURD
    }).toChar
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
        val (char, fg, bg, _) = charForItem(state, topItem)
        (char, fg, bg)
      } else {
        val terrain = level.terrain(x, y)

        if (terrain.connects) {
          def apparent(x: Int, y: Int): Option[Terrain] = {
            if (state.broadcastToLocation(OnFloor(Location(levelId, x, y)), Message.DisplayConnectedTo(terrain)).connected)
              Some(terrain)
            else
              level.terrain.get(x, y) map { t => t.connectsTo.map(state.data.terrain).getOrElse(t) }
          }

          val left = apparent(x - 1, y)
          val up = apparent(x, y - 1)
          val right = apparent(x + 1, y)
          val down = apparent(x, y + 1)
          val viewedFrom = Dir.from((state.player.x, state.player.y), (x, y))
          val (_, fg, bg, _) = state.data.display.getDisplay(terrain.display)
          (charForWall(
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
      else if (state.showGasDebug.nonEmpty)
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
  def animating: Boolean = false
}

class GLFWDisplay(val window: GLFWWindow, val font: Font, val state: GameState) {
  /** NB. this is thread-safe because it can be accessed from the file watcher for reloading data */
  private val pendingActions = new java.util.concurrent.ConcurrentLinkedQueue[Action]
  val windowWidthChars = 80
  val windowHeightChars = 60
  private val screens = mutable.ListBuffer.empty[Screen]

  private [display] def pushScreen(screen: Screen): Unit = {
    screens.append(screen)
    render()
  }
  private [display] def popScreen(): Unit = {
    if (screens.nonEmpty) screens.remove(screens.size - 1)
    render()
  }
  private [display] def pushAction(a: Action): Unit = {
    pendingActions.add(a)
  }

  private var defaultPrevented = false
  def preventDefault(): Unit = defaultPrevented = true

  def init(): Unit = {
    window.init(width = windowWidthChars * font.tileWidth * font.scaleFactor, height = windowHeightChars * font.tileHeight * font.scaleFactor)
    window.onChar { (char: Int) =>
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
          render()
        }
      } else {
        if (action == GLFW_PRESS || action == GLFW_REPEAT) {
          key match {
            case DirectionKey(dx, dy) => pushAction(Action.PlayerMove(dx, dy))

              // Drop
            case GLFW_KEY_D =>
              state.items.lookup(InHands()).headOption foreach { item =>
                pushAction(Action.PutDown(item))
              }

              // Wait
            case GLFW_KEY_PERIOD => pushAction(Action.Wait(1))
            case GLFW_KEY_COMMA => pushAction(Action.Wait(100))

              // Examine
            case GLFW_KEY_E => pushScreen(new ExamineDirectionScreen(this, state))

              // Inventory
            case GLFW_KEY_I => pushScreen(new InventoryScreen(this, state))

              // Assemble
            case GLFW_KEY_A => pushScreen(new AssemblyScreen(this, state))

              // Wish
            case GLFW_KEY_GRAVE_ACCENT => pushScreen(new WishScreen(this, state))

              // Look
            case GLFW_KEY_SEMICOLON => pushScreen(new LookScreen(this, state))

            case GLFW_KEY_C => pushScreen(new CableScreen(this, state))

              // Regenerate (in RoomTest mode)
            case _ if state.isRoomTest =>
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

  def screenToWorld(state: GameState)(screenCoords: (Int, Int)): (Int, Int) = {
    val (sx, sy) = screenCoords
    val wr = worldRect(state)
    (sx + wr.l, sy + wr.t)
  }

  val sidebarWidth = 20
  val screenRect: Rect = Rect(0, 0, windowWidthChars, windowHeightChars)
  val Seq(worldViewRect, sidebarRect) = screenRect.cutHorizontal(Seq(0, screenRect.width - sidebarWidth, screenRect.width)).to(Seq)

  def worldRect(state: GameState): Rect = {
    Rect(
      l = state.player.x - worldViewRect.width / 2,
      t = state.player.y - worldViewRect.height / 2,
      r = state.player.x - worldViewRect.width / 2 + worldViewRect.width,
      b = state.player.y - worldViewRect.height / 2 + worldViewRect.height
    )
  }

  def render(): Unit = {
    val (wWidth, wHeight) = window.size
    if (wWidth != windowWidthChars * (font.tileWidth * font.scaleFactor) ||
      wHeight != windowHeightChars * (font.tileHeight * font.scaleFactor)) {
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

      layout3.draw(glyphRenderer, sidebarRect, sidebar())

      for (screen <- screens) {
        screen.render(glyphRenderer)
      }
    }
  }

  private def sidebar(): layout3.Widget = {
    import layout3.*
    Column(crossAxisAlignment = CrossAxisAlignment.Stretch, children = Seq(
      ConstrainedBox(BoxConstraints(minHeight = 10, maxHeight = 10), content = Border(Column(
        state.symptoms.take(5).map(t => Text(t.description)) ++
          (if (state.showTempDebug)
            Seq(Text(state.levels(state.player.levelId).temperature(state.player.xy).toString)) else Seq.empty) ++
          (if (state.showGasDebug.nonEmpty)
            Seq(Text(state.levels(state.player.levelId).gasComposition(state.player.xy).toString)) else Seq.empty) ++
          Seq(Text(state.currentTime.toString))
      ))),
      ConstrainedBox(BoxConstraints(minHeight = 12, maxHeight = 12), content = Border(Column(
        Seq("Held" -> InHands(), "Worn" -> Worn()).flatMap { case (title, loc) =>
          Text(title) +:
            state.items.lookup(loc).map(item => Text(" " + state.itemDisplayName(item)))
        }
      ))),
      Flexible(Border(Column(
        verticalDirection = VerticalDirection.Up,
        clipBehavior = ClipBehavior.Clip,
        children = {
          val maxMessageAge = 300
          val oldestMessageTime = state.currentTime - maxMessageAge
          val messages = state.messages.filter(_._2 >= oldestMessageTime).reverse
          messages.map {
            case (line, time) =>
              val lineAge = state.currentTime - time
              val color =
                if (lineAge < 30) Color.White
                else if (lineAge < 120) Color(0.5f, 0.5f, 0.5f, 1.0f)
                else Color(0.2f, 0.2f, 0.2f, 1.0f)
              Text(line.withFg(color))
          }
        }
      )))
    ))
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
          renderer.drawChar(screenLeft + x - left, screenTop + y - top, char, fg = state.data.display.palette("remembered"))
        }
      }

      val level = state.levels(levelId)

      if (state.showGasDebug.nonEmpty && level.terrain.contains(x, y)) {
        val gc = level.gasComposition(x, y)
        val gasLevel = state.showGasDebug.get match {
          case "o2" => gc.oxygen
          case "co2" => gc.carbonDioxide
          case "n2" => gc.nitrogen
          case _ => 0f
        }
        val (min, max) = (0, 42)
        val gradient = Gradient(
          //Seq(Color.fromHex("#fee8c8"), Color.fromHex("#fdbb84"), Color.fromHex("#e34a33")).map(_.get)
          Seq(Color.fromHex("#0000ff"), Color.fromHex("#fee8c8"), Color.fromHex("#ff0000")).map(_.get)
        )
        val t = (gasLevel - min) / (max - min)
        val color = gradient.sample(t).copy(a = 0.8f)
        renderer.drawChar(screenLeft + x - left, screenTop + y - top, BoxDrawing.LURD, fg = color, bg = Color(0f, 0f, 0f, 0f))
      }

      if (state.showTempDebug && level.terrain.contains(x, y)) {
        val temp = level.temperature(x, y)
        val color = if (temp > 273)
          Color((1 - math.exp(-(temp - 273)/10)).toFloat, 0, 0, 0.8f)
        else
          Color(0, 0, (1 - math.exp((temp - 273)/10)).toFloat, 0.8f)
        renderer.drawChar(screenLeft + x - left, screenTop + y - top, BoxDrawing.LURD, color, bg = Color(0f, 0f, 0f, 0f))
      }
    }
    for (p <- particles) {
      if (state.isVisible(Location(levelId, p.x.toInt, p.y.toInt)) || state.seeThroughWalls)
        renderer.drawParticle(screenLeft + p.x - left, screenTop + p.y - top, 2, Color(1, 1, 1, 0.1f))
    }
  }

  def updateParticles(dt: Float = 0.01f): Unit = {
    val level = state.levels(state.player.levelId)
    for (p <- particles) {
      val tx = p.x.toInt
      val ty = p.y.toInt
      val gc = level.gasComposition(tx, ty)
      def gcAt(tx: Int, ty: Int): GasComposition = {
        // if a neighboring tile is not permeable, pretend its pressure is the same as this tile,
        // to give it a neutral contribution to the final gradient.
        val permeable = state.isPermeable(Location(state.player.levelId, tx, ty))
        if (permeable)
          level.gasComposition(tx, ty)
        else
          gc
      }
      val gcUp = gcAt(tx, ty - 1)
      val gcDown = gcAt(tx, ty + 1)
      val gcLeft = gcAt(tx - 1, ty)
      val gcRight = gcAt(tx + 1, ty)

      val gcp = gc.totalPressure
      val gx = 1 * (gcLeft.totalPressure - gcp) + -1 * (gcRight.totalPressure - gcp)
      val gy = 1 * (gcUp.totalPressure - gcp) + -1 * (gcDown.totalPressure - gcp)
      p.x += gx * dt + (prandom.nextFloat() * 2 - 1) * 0.01f
      p.y += gy * dt + (prandom.nextFloat() * 2 - 1) * 0.01f
      p.lifetime -= dt

      if (p.lifetime < 0 || p.x < 0 || p.y < 0 || p.x >= level.width || p.y >= level.height || !state.isPermeable(Location(state.player.levelId, p.x.toInt, p.y.toInt))) {
        p.lifetime = 2f + prandom.nextFloat()
        do {
          p.x = level.width * prandom.nextFloat()
          p.y = level.height * prandom.nextFloat()
        } while (!state.isPermeable(Location(state.player.levelId, p.x.toInt, p.y.toInt)))
      }
    }
  }

  def update(): Unit = {
    if (state.isDead) {
      pushScreen(new DeathScreen(this, state))
      return
    }
    render()
  }

  def waitForAction: Action = {
    while (pendingActions.isEmpty) {
      if (screens.exists(_.animating) || true)
        window.poll()
      else
        window.waitEvents()
      render()
      updateParticles()
    }
    pendingActions.poll()
  }

  def postAction(action: Action): Unit = {
    pushAction(action)
    glfwPostEmptyEvent()
  }

  case class Particle(var x: Float, var y: Float, var lifetime: Float)
  val particles = mutable.Buffer.empty[Particle];
  val prandom = new Random;
  {
    val width = state.levels(state.player.levelId).width
    val height = state.levels(state.player.levelId).height
    for (_ <- 0 until width * height * 5) {
      particles.append(Particle(prandom.nextFloat() * width, prandom.nextFloat() * height, 10 * prandom.nextFloat()))
    }
    updateParticles()
  }

  def run(): Unit = {
    while (!window.shouldClose) {
      val action = waitForAction
      state.receive(action)
      update()
    }
  }
}
