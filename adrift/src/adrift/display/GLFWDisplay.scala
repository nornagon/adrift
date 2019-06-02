package adrift.display

import adrift._
import adrift.display.CP437.BoxDrawing
import adrift.display.glutil.{Image, SpriteBatch, Texture}
import adrift.items.{Item, Message}
import adrift.items.behaviors
import org.lwjgl.BufferUtils
import org.lwjgl.glfw.GLFW._
import org.lwjgl.glfw.GLFWErrorCallback
import org.lwjgl.opengl.GL
import org.lwjgl.opengl.GL11._

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

  def charForWall(center: Terrain, left: Terrain, up: Terrain, right: Terrain, down: Terrain, viewingFrom: Dir): Char = {
    import CP437.BoxDrawing._
    import Dir._
    ((left == center, up == center, right == center, down == center) match {
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
    val OnFloor(x, y) = state.items.lookup(unrolledItem)
    val unrolled = unrolledItem.behaviors.collectFirst { case u: behaviors.Unrolled => u }.get
    val prevDir = Dir.from(unrolled.fromCell, (x, y))
    val nextDir = unrolled.toCell.map(Dir.from(_, (x, y))).getOrElse(prevDir.opposite)
    import Dir._
    import CP437.BoxDrawing._
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

  def charForItem(state: GameState, item: Item): (Char, Color, Color, Int) =
    state.data.display.getDisplay(displayForItem(state, item))
  def displayForItem(state: GameState, item: Item): String =
    state.sendMessage(item, Message.Display(item.kind.display)).display

  def charAtPosition(state: GameState, x: Int, y: Int): (Char, Color, Color) = {
    if (x == state.player._1 && y == state.player._2) {
      val (char, fg, bg, _) = state.data.display.getDisplay("PLAYER")
      (char, fg, bg)
    } else if (state.terrain.contains(x, y)) {
      val items = state.items.lookup(OnFloor(x, y))
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
        def apparent(x: Int, y: Int): Option[Terrain] = {
          if (state.terrain.contains(x, y))
            Some(state.terrain((x, y)))
          else None
        }

        val terrain = state.terrain(x, y)
        if (terrain.connects) {
          val left = apparent(x - 1, y)
          val up = apparent(x, y - 1)
          val right = apparent(x + 1, y)
          val down = apparent(x, y + 1)
          val viewedFrom = Dir.from(state.player, (x, y))
          val (_, fg, bg, _) = state.data.display.getDisplay(terrain.display)
          (charForWall(terrain,
            left.orNull,
            up.orNull,
            right.orNull,
            down.orNull,
            viewedFrom), fg, bg)
        } else {
          charForTerrain(state, terrain)
        }
      }
    } else (' ', Color.Black, Color.Black)
  }

  def messageAtCell(state: GameState, position: (Int, Int)): String = {
    val items = state.items.lookup(OnFloor(position._1, position._2))
    val debug =
      if (state.showTempDebug)
        f" (${state.temperature(position) - 273}%.1f C)"
      else if (state.showGasDebug)
        s" (${state.gasComposition(position)})"
      else
        ""
    (if (items.nonEmpty) {
      s"Here: ${items.last.kind.name}" + (if (items.size > 1) s" and ${items.size - 1} other things" else "")
    } else {
      state.terrain(position).name
    }) + debug
  }
}

trait Screen {
  def key(key: Int, scancode: Int, action: Int, mods: Int): Unit
  def char(char: Int): Unit = {}
  def render(renderer: GlyphRenderer): Unit
}

class GLFWDisplay extends Display {
  private var window: Long = 0
  var font: Texture = _
  private var spriteBatch: SpriteBatch = _
  private var lastState: GameState = _
  private val pendingActions = mutable.Buffer.empty[Action]
  val windowWidthChars = 80
  val windowHeightChars = 48
  val screenCharWidth = 16
  val screenCharHeight = 16

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
    pendingActions.append(a)
  }

  def init(): Unit = {
    GLFWErrorCallback.createPrint(System.err).set()

    if (!glfwInit()) {
      throw new IllegalStateException("Unable to initialize GLFW")
    }

    glfwWindowHint(GLFW_VISIBLE, GLFW_FALSE)
    glfwWindowHint(GLFW_RESIZABLE, GLFW_FALSE)

    window = glfwCreateWindow(640, 480, "Adrift", 0, 0)
    if (window == 0)
      throw new RuntimeException("Failed to create GLFW window")

    glfwSetCharCallback(window, (window: Long, char: Int) => {
      if (screens.nonEmpty) {
        screens.last.char(char)
      }
    })

    glfwSetKeyCallback(window, (window: Long, key: Int, scancode: Int, action: Int, mods: Int) => {
      if (screens.nonEmpty) {
        if (key == GLFW_KEY_ESCAPE && action == GLFW_PRESS) {
          popScreen()
        } else {
          screens.last.key(key, scancode, action, mods)
          render(lastState)
        }
      } else {
        if (action == GLFW_PRESS || action == GLFW_REPEAT) {
          key match {
            case GLFW_KEY_LEFT | GLFW_KEY_H => pendingActions.append(Action.PlayerMove(-1, 0))
            case GLFW_KEY_DOWN | GLFW_KEY_J => pendingActions.append(Action.PlayerMove(0, 1))
            case GLFW_KEY_UP | GLFW_KEY_K => pendingActions.append(Action.PlayerMove(0, -1))
            case GLFW_KEY_RIGHT | GLFW_KEY_L => pendingActions.append(Action.PlayerMove(1, 0))
            case GLFW_KEY_Y => pendingActions.append(Action.PlayerMove(-1, -1))
            case GLFW_KEY_U => pendingActions.append(Action.PlayerMove(1, -1))
            case GLFW_KEY_B => pendingActions.append(Action.PlayerMove(-1, 1))
            case GLFW_KEY_N => pendingActions.append(Action.PlayerMove(1, 1))
            case GLFW_KEY_PERIOD => pendingActions.append(Action.Wait())
            case _ =>
          }
        } else if (action == GLFW_RELEASE) {
          key match {
            case GLFW_KEY_I => pushScreen(new InventoryScreen(this, lastState))
            case GLFW_KEY_A => pushScreen(new AssemblyScreen(this, lastState))
            case GLFW_KEY_GRAVE_ACCENT => pushScreen(new WishScreen(this, lastState))
            case GLFW_KEY_SEMICOLON => pushScreen(new LookScreen(this, lastState))
            case _ =>
          }
        }
      }
    })
    glfwSetWindowCloseCallback(window, (_: Long) => pendingActions.append(Action.Quit))

    glfwMakeContextCurrent(window)
    glfwSwapInterval(1)

    GL.createCapabilities()

    font = Texture.fromImage(Image.fromFile("cp437_8x8.png"))
    spriteBatch = SpriteBatch.create
  }

  def cameraBounds(state: GameState): (Int, Int, Int, Int) = {
    val worldHeightChars = windowHeightChars - 1
    val left = state.player._1 - windowWidthChars/2
    val right = left + windowWidthChars
    val top = state.player._2 - worldHeightChars/2
    val bottom = top + worldHeightChars
    (left, right, top, bottom)
  }

  def render(state: GameState): Unit = {
    val worldHeightChars = windowHeightChars - 1
    val wBuf = BufferUtils.createIntBuffer(1)
    val hBuf = BufferUtils.createIntBuffer(1)
    glfwGetWindowSize(window, wBuf, hBuf)
    if (wBuf.get(0) != windowWidthChars * screenCharWidth || hBuf.get(0) != windowHeightChars * screenCharHeight)
      glfwSetWindowSize(window, windowWidthChars * screenCharWidth, windowHeightChars * screenCharHeight)
    val visible = glfwGetWindowAttrib(window, GLFW_VISIBLE)
    if (visible == GLFW_FALSE)
      glfwShowWindow(window)
    glfwGetFramebufferSize(window, wBuf, hBuf)
    glViewport(0, 0, wBuf.get(), hBuf.get())
    glClearColor(0, 0, 0, 1)

    spriteBatch.resize(screenCharWidth * windowWidthChars, screenCharHeight * windowHeightChars)

    val renderer = new GlyphRenderer(spriteBatch, 8, 8, screenCharWidth, screenCharHeight, font)

    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)
    glEnable(GL_BLEND)
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)

    val left = state.player._1 - windowWidthChars/2
    val right = left + windowWidthChars
    val top = state.player._2 - worldHeightChars/2
    val bottom = top + worldHeightChars

    spriteBatch.begin()

    renderWorld(state, renderer, left, right, top, bottom)
    val message = state.message.getOrElse(Appearance.messageAtCell(state, state.player))
    renderer.drawString(0, worldHeightChars, message)
    renderer.drawString(windowWidthChars - 10, worldHeightChars, f"${state.bodyTemp - 273}%.1f C")

    for (screen <- screens) {
      screen.render(renderer)
    }

    spriteBatch.end()

    glfwSwapBuffers(window)
  }

  private def renderWorld(
    state: GameState,
    renderer: GlyphRenderer,
    left: Int,
    right: Int,
    top: Int,
    bottom: Int
  ): Unit = {
    for (y <- top until bottom; x <- left until right) {
      if (state.isVisible(x, y)) {
        val (char, fg, bg) = Appearance.charAtPosition(state, x, y)
        renderer.drawChar(font, x - left, y - top, char, fg, bg)
      } else {
        val (char, fg, bg) = Appearance.charAtPosition(state, x, y)
        renderer.drawChar(font, x - left, y - top, char, fg = Color(0.0f, 0.1f, 0.05f, 1.0f))
      }

      if (state.showGasDebug && state.gasComposition.contains(x, y)) {
        val gc = state.gasComposition(x, y)
        val color = Color(0, 0, gc.oxygen.toFloat / 15, 0.3f)
        renderer.drawChar(font, x - left, y - top, BoxDrawing.LURD, color, bg = Color(0f, 0f, 0f, 0f))
      }

      if (state.showTempDebug && state.temperature.contains(x, y)) {
        val temp = state.temperature(x, y)
        val color = if (temp > 273)
          Color((1 - math.exp(-(temp - 273)/30)).toFloat, 0, 0, 0.3f)
        else
          Color(0, 0, (1 - math.exp((temp - 273)/30)).toFloat, 0.3f)
        renderer.drawChar(font, x - left, y - top, BoxDrawing.LURD, color, bg = Color(0f, 0f, 0f, 0f))
      }
    }
  }

  override def update(state: GameState): Unit = {
    lastState = state
    render(state)
  }

  override def waitForAction: Action = {
    if (pendingActions.nonEmpty)
      return pendingActions.remove(0)
    while (pendingActions.isEmpty)
      glfwWaitEvents()
    pendingActions.remove(0)
  }

  override def running: Boolean = !glfwWindowShouldClose(window)
}
