package adrift.display

import adrift._
import adrift.display.glutil.{Image, SpriteBatch, Texture}
import adrift.items.{DoorOpen, Item, ItemOperation}
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

object Appearance {
  def charForTerrain(terrain: Terrain): Int = terrain.display match {
    case "FLOOR" => '.'
    case "SPACE" => ' '
  }

  def charForWall(center: Terrain, left: Terrain, up: Terrain, right: Terrain, down: Terrain): Int = {
    import CP437.BoxDrawing._
    (left == center, up == center, right == center, down == center) match {
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
      case (true, true, true, false) => LUR_
      case (true, true, false, true) => LU_D
      case (true, false, true, true) => L_RD
      case (false, true, true, true) => _URD
      case (true, true, true, true) => LURD
    }
  }

  def charForItem(item: Item): Int = item.kind match {
    case k if k.name == "automatic door" =>
      val isOpen = item.conditions.exists(_.isInstanceOf[DoorOpen])
      if (isOpen) '-' else '+'
    case other =>
      Item.item_display(other.display)
  }


  def charAtPosition(state: GameState, x: Int, y: Int): Int = {
    if (x == state.player._1 && y == state.player._2) '@'
    else if (state.map.contains(x, y)) {
      val items = state.items(x, y)
      if (items.nonEmpty) {
        charForItem(items.last)
      } else {
        def apparent(x: Int, y: Int): Option[Terrain] = {
          if (state.map.contains(x, y) && state.isVisible(x, y))
            Some(state.map((x, y)))
          else None
        }
        state.map(x, y).display match {
          case "WALL" =>
            val left = apparent(x - 1, y)
            val up = apparent(x, y - 1)
            val right = apparent(x + 1, y)
            val down = apparent(x, y + 1)
            charForWall(state.map(x, y),
              left.orNull,
              up.orNull,
              right.orNull,
              down.orNull)
          case _ => charForTerrain(state.map(x, y))
        }
      }
    } else ' '
  }

  def messageAtCell(state: GameState, position: (Int, Int)): String = {
    val items = state.items(position)
    if (items.nonEmpty) {
      s"Here: ${items.last.kind.name}" + (if (items.size > 1) s" and ${items.size - 1} other things" else "")
    } else {
      state.map(position).name
    }
  }
}

trait Screen {
  def key(key: Int, scancode: Int, action: Int, mods: Int): Unit
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
            case GLFW_KEY_I => pushScreen(new InventoryScreen(this, lastState))
            case GLFW_KEY_A => pushScreen(new AssemblyScreen(this, lastState))
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
        val char = Appearance.charAtPosition(state, x, y)
        renderer.drawChar(font, x - left, y - top, char)
      } else {
        val char = Appearance.charAtPosition(state, x, y)
        renderer.drawChar(font, x - left, y - top, char, fg = (0.0f, 0.1f, 0.05f, 1.0f))
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
