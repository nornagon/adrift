package adrift.display

import adrift.display.glutil.{Image, SpriteBatch, Texture}
import adrift._
import adrift.items.Item
import org.lwjgl.BufferUtils
import org.lwjgl.glfw.GLFW._
import org.lwjgl.glfw.GLFWErrorCallback
import org.lwjgl.opengl.GL
import org.lwjgl.opengl.GL11._

import scala.collection.mutable

object PETSCII {
  object BoxDrawing {
    val DR = 112
    val UR = 109
    val DL = 110
    val UL = 125
    val LR = 64
    val UD = 66
  }
  val Spades = 65
  val Clubs = 88
  val UpperRightTriangle = 95
}

class Renderer(
  spriteBatch: SpriteBatch,
  tileWidth: Int,
  tileHeight: Int,
  screenTileWidth: Int,
  screenTileHeight: Int,
  upper: Texture,
  lower: Texture,
) {
  def drawChar(tex: Texture, x: Int, y: Int, c: Int): Unit = {
    val cx = c % 32
    val cy = c / 32
    spriteBatch.drawRegion(
      tex,
      cx * tileWidth, cy * tileHeight,
      tileWidth, tileHeight,
      x * screenTileWidth, y * screenTileHeight,
      screenTileWidth, screenTileHeight
    )
  }

  def drawBox(x: Int, y: Int, w: Int, h: Int): Unit = {
    import PETSCII.BoxDrawing
    drawChar(upper, x, y, BoxDrawing.DR)
    drawChar(upper, x + w - 1, y, BoxDrawing.DL)
    drawChar(upper, x, y + h - 1, BoxDrawing.UR)
    drawChar(upper, x + w - 1, y + h - 1, BoxDrawing.UL)
    for (iy <- 1 until (h - 1); ix <- 1 until (w - 1))
      drawChar(upper, x + ix, y + iy, 32)
    for (ix <- 1 until (w - 1)) {
      drawChar(upper, x + ix, y, BoxDrawing.LR)
      drawChar(upper, x + ix, y + h - 1, BoxDrawing.LR)
    }
    for (iy <- 1 until (h - 1)) {
      drawChar(upper, x, y + iy, BoxDrawing.UD)
      drawChar(upper, x + w - 1, y + iy, BoxDrawing.UD)
    }
  }

  def drawString(x: Int, y: Int, s: String): Unit = {
    for ((c, i) <- s.zipWithIndex) {
      val tc = c match {
        case cc if cc >= 'a' && cc <= 'z' => cc - 'a' + 1
        case '[' => 27
        case ']' => 29
        case cc => cc
      }
      drawChar(lower, x + i, y, tc)
    }
  }

  def drawStringWrapped(x: Int, y: Int, maxWidth: Int, maxHeight: Int, s: String): Unit = {
    var cx = 0
    var cy = 0
    for (word <- s.split("\\s")) {
      if (cx + word.length >= maxWidth) {
        cy += 1
        cx = 0
      }
      if (cy >= maxHeight)
        return
      drawString(x + cx, y + cy, word)
      cx += word.length + 1
    }
  }
}

trait Screen {
  def key(key: Int, scancode: Int, action: Int, mods: Int): Unit
  def render(renderer: Renderer): Unit
}

class ExamineScreen(display: GLFWDisplay, state: GameState, item: Item) extends Screen {
  val anchor = (5, 3)
  override def key(key: Int, scancode: Int, action: Int, mods: Int): Unit = {

  }

  override def render(renderer: Renderer): Unit = {
    val itemsByKind = item.parts.groupBy(_.kind)
    val width = 30
    val height = 9 + itemsByKind.size
    renderer.drawBox(anchor._1, anchor._2, width, height)
    renderer.drawString(anchor._1 + 1, anchor._2, s"[${item.kind.name}]")
    renderer.drawStringWrapped(anchor._1 + 1, anchor._2 + 1, maxWidth = width - 2, maxHeight = 5, item.kind.description)
    renderer.drawString(anchor._1 + 1, anchor._2 + 1 + 5 + 1, "Parts:")
    for ((p, i) <- itemsByKind.zipWithIndex) {
      val str = s"${if (p._2.size != 1) s"${p._2.size} x " else ""}${p._1.name}"
      renderer.drawString(anchor._1 + 2, anchor._2 + 1 + 5 + 1 + 1 + i, str)
    }
  }
}

class InventoryScreen(display: GLFWDisplay, state: GameState) extends Screen {
  def nearbyItems: Seq[(Item, (Int, Int))] = {
    val player = state.player
    val nearbyItems = mutable.Buffer.empty[(Item, (Int, Int))]
    for (dy <- -2 to 2; dx <- -2 to 2) {
      val items = state.items(player._1 + dx, player._2 + dy)
      if (items.nonEmpty) {
        nearbyItems ++= items.map((_, (player._1 + dx, player._2 + dy)))
      }
    }
    nearbyItems
  }

  var selectedIdx = 0
  override def key(key: Int, scancode: Int, action: Int, mods: Int): Unit = {
    if (action == GLFW_PRESS || action == GLFW_REPEAT)
      key match {
        case GLFW_KEY_J | GLFW_KEY_DOWN => selectedIdx = math.min(selectedIdx + 1, nearbyItems.size - 1)
        case GLFW_KEY_K | GLFW_KEY_UP => selectedIdx = math.max(selectedIdx - 1, 0)
        case GLFW_KEY_E =>
          if (selectedIdx >= 0 && selectedIdx < nearbyItems.size)
            display.pushScreen(new ExamineScreen(display, state, nearbyItems(selectedIdx)._1))
        case _ =>
      }
  }

  override def render(renderer: Renderer): Unit = {
    val player = state.player
    renderer.drawBox(1, 1, 30, 20)
    renderer.drawString(2, 2, "Nearby")
    for (((item, pos), i) <- nearbyItems.zipWithIndex) {
      renderer.drawString(4, 3 + i, item.kind.name)
      renderer.drawString(20, 3 + i, directionString(pos._1 - player._1, pos._2 - player._2))
    }
    if (nearbyItems.nonEmpty)
      renderer.drawString(2, 3 + selectedIdx, ">")
  }

  def directionString(x: Int, y: Int): String = {
    if (x < 0) {
      if (y < 0) "NW"
      else if (y == 0) "W"
      else "SW"
    } else if (x == 0) {
      if (y < 0) "N"
      else if (y == 0) "."
      else "S"
    } else {
      if (y < 0) "NE"
      else if (y == 0) "E"
      else "SE"
    }
  }
}

class GLFWDisplay extends Display {
  var window: Long = 0
  var upper: Texture = _
  var lower: Texture = _
  var spriteBatch: SpriteBatch = _
  var lastState: GameState = _
  private val pendingActions = mutable.Buffer.empty[Action]

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
    glfwWindowHint(GLFW_RESIZABLE, GLFW_TRUE)

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
            case _ =>
          }
        }
      }
    })
    glfwSetWindowCloseCallback(window, (_: Long) => pendingActions.append(Action.Quit))

    glfwMakeContextCurrent(window)
    glfwSwapInterval(1)

    GL.createCapabilities()

    upper = Texture.fromImage(loadc64("c64_upp.gif"))
    lower = Texture.fromImage(loadc64("c64_low.gif"))
    spriteBatch = SpriteBatch.create
  }

  private def loadc64(name: String): Image = {
    val image = Image.fromFile(name)
    // TODO: would be better to just do this transform in an image editor
    for (i <- 0 until image.bytes.limit() by 4) {
      val isWhite = image.bytes.get(i) == 0
      if (isWhite) {
        image.bytes.put(i, 0xff.toByte)
        image.bytes.put(i+1, 0xff.toByte)
        image.bytes.put(i+2, 0xff.toByte)
        image.bytes.put(i+3, 0xff.toByte)
      } else {
        image.bytes.put(i, 0x00.toByte)
        image.bytes.put(i+1, 0x00.toByte)
        image.bytes.put(i+2, 0x00.toByte)
        image.bytes.put(i+3, 0x00.toByte)
      }
    }
    image
  }

  def charForTerrain(terrain: Terrain): Int = terrain match {
    case EmptySpace => ' '
    case Floor => '.'
    case Grass => ','
    case TreeOak => PETSCII.Spades
    case TreeFicus => PETSCII.Clubs
    case GlassWall => PETSCII.UpperRightTriangle
  }

  def charForItem(item: Item): Int = item.kind match {
    case items.HoloNote => '!'
  }

  def render(state: GameState): Unit = {
    val windowWidthChars = 80
    val windowHeightChars = 48
    val screenCharWidth = 16
    val screenCharHeight = 16
    val worldHeightChars = windowHeightChars - 1
    val wBuf = BufferUtils.createIntBuffer(1)
    val hBuf = BufferUtils.createIntBuffer(1)
    glfwGetWindowSize(window, wBuf, hBuf)
    if (wBuf.get(0) != windowWidthChars * screenCharWidth || hBuf.get(0) != windowHeightChars * screenCharHeight)
      glfwSetWindowSize(window, windowWidthChars * screenCharWidth, windowHeightChars * screenCharHeight)
    val visible = glfwGetWindowAttrib(window, GLFW_VISIBLE)
    if (visible == GLFW_FALSE)
      glfwShowWindow(window)
    glClearColor(0, 0, 0, 1)

    spriteBatch.resize(screenCharWidth * windowWidthChars, screenCharHeight * windowHeightChars)

    val renderer = new Renderer(spriteBatch, 8, 8, screenCharWidth, screenCharHeight, upper, lower)

    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)

    val left = state.player._1 - windowWidthChars/2
    val right = left + windowWidthChars
    val top = state.player._2 - worldHeightChars/2
    val bottom = top + worldHeightChars

    spriteBatch.begin()

    renderWorld(state, renderer, left, right, top, bottom)
    val message = messageAtCell(state, state.player)
    renderer.drawString(0, worldHeightChars, message)

    for (screen <- screens) {
      screen.render(renderer)
    }

    spriteBatch.end()

    glfwSwapBuffers(window)
  }

  private def renderWorld(
    state: GameState,
    renderer: Renderer,
    left: Int,
    right: Int,
    top: Int,
    bottom: Int
  ) = {
    for (y <- top until bottom; x <- left until right) {
      if (x == state.player._1 && y == state.player._2) renderer.drawChar(upper, x - left, y - top, 0)
      else if (state.map.contains(x, y)) {
        val items = state.items(x, y)
        if (items.nonEmpty) {
          renderer.drawChar(upper, x - left, y - top, charForItem(items.last))
        } else {
          renderer.drawChar(upper, x - left, y - top, charForTerrain(state.map(x, y)))
        }
      }
    }
  }

  def messageAtCell(state: GameState, position: (Int, Int)): String = {
    val items = state.items(position)
    if (items.nonEmpty) {
      s"Here: ${items.last.kind.name}" + (if (items.size > 1) s" and ${items.size - 1} other things" else "")
    } else {
      state.map(position).toString
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
