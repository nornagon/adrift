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
      drawChar(upper, x + ix, y + iy, ' ')
    for (ix <- 1 until (w - 1)) {
      drawChar(upper, x + ix, y, BoxDrawing.LR)
      drawChar(upper, x + ix, y + h - 1, BoxDrawing.LR)
    }
    for (iy <- 1 until (h - 1)) {
      drawChar(upper, x, y + iy, BoxDrawing.UD)
      drawChar(upper, x + w - 1, y + iy, BoxDrawing.UD)
    }
  }

  def drawString(x: Int, y: Int, s: String, maxWidth: Int = 0): Unit = {
    for ((c, i) <- s.zipWithIndex) {
      if (maxWidth != 0 && i >= maxWidth) return
      val tc = c match {
        case cc if cc >= 'a' && cc <= 'z' => cc - 'a' + 1
        case '[' => 27
        case ']' => 29
        case cc => cc
      }
      drawChar(lower, x + i, y, tc)
    }
  }

  def linesForString(maxWidth: Int, maxHeight: Int, s: String): Seq[String] = {
    val lines = mutable.Buffer.empty[String]
    val currentLine = new mutable.StringBuilder()
    for (word <- s.split("\\s")) {
      val wordWithSpace = (if (currentLine.nonEmpty) " " else "") + word
      if (currentLine.size + wordWithSpace.length >= maxWidth) {
        lines.append(currentLine.toString())
        currentLine.clear()
      }
      // TODO: ellipsis
      if (lines.size >= maxHeight) return lines
      if (currentLine.nonEmpty)
        currentLine.append(" ")
      currentLine.append(word)
    }
    if (currentLine.isEmpty)
      lines
    else
      lines :+ currentLine.toString()
  }

  def drawStringWrapped(x: Int, y: Int, maxWidth: Int, maxHeight: Int, s: String): Unit = {
    for ((line, cy) <- linesForString(maxWidth, maxHeight, s).zipWithIndex) {
      drawString(x, y + cy, line)
    }
  }
}

trait Screen {
  def key(key: Int, scancode: Int, action: Int, mods: Int): Unit
  def render(renderer: Renderer): Unit
}

class ExamineScreen(display: GLFWDisplay, state: GameState, itemLocation: ItemLocation) extends Screen {
  def item: Item = state.itemAtLocation(itemLocation)
  private val anchor = (5, 3)
  override def key(key: Int, scancode: Int, action: Int, mods: Int): Unit = {
    if (action == GLFW_PRESS) {
      key match {
        case GLFW_KEY_D if (mods & GLFW_MOD_SHIFT) != 0 =>
          display.pushAction(Action.Disassemble(itemLocation))
          display.popScreen()
        case _ =>
      }
    }
  }

  override def render(renderer: Renderer): Unit = {
    val itemsByKind = item.parts.groupBy(_.kind)
    val width = 30
    val descriptionLines = renderer.linesForString(maxWidth = width - 2, maxHeight = 9, item.kind.description)
    val height = 2 + descriptionLines.size + 2 + itemsByKind.size
    renderer.drawBox(anchor._1, anchor._2, width, height)
    renderer.drawString(anchor._1 + 1, anchor._2, s"[${item.kind.name}]", maxWidth = width-2)
    renderer.drawStringWrapped(anchor._1 + 1, anchor._2 + 1, maxWidth = width - 2, maxHeight = 9, item.kind.description)
    renderer.drawString(anchor._1 + 1, anchor._2 + 1 + descriptionLines.size + 1, "Parts:")
    for ((p, i) <- itemsByKind.zipWithIndex) {
      val str = s"${if (p._2.size != 1) s"${p._2.size} x " else ""}${p._1.name}"
      renderer.drawString(anchor._1 + 2, anchor._2 + 1 + descriptionLines.size + 1 + 1 + i, str, maxWidth = width - 3)
    }
  }
}

class InventoryScreen(display: GLFWDisplay, state: GameState) extends Screen {
  def nearbyItems: Seq[(Item, ItemLocation)] = {
    val player = state.player
    val nearbyItems = mutable.Buffer.empty[(Item, ItemLocation)]
    for (dy <- -2 to 2; dx <- -2 to 2) {
      val items = state.items(player._1 + dx, player._2 + dy)
      if (items.nonEmpty) {
        nearbyItems ++= items.zipWithIndex.map { case (item, i) => (item, OnFloor(player._1 + dx, player._2 + dy, i)) }
      }
    }
    nearbyItems ++ inHandItems
  }

  def inHandItems: Seq[(Item, ItemLocation)] = {
    state.hands.contents.zipWithIndex.map {
      case (item, i) => (item, InHands(i))
    }
  }

  var selectedIdx = 0
  override def key(key: Int, scancode: Int, action: Int, mods: Int): Unit = {
    if (action == GLFW_PRESS || action == GLFW_REPEAT)
      key match {
        case GLFW_KEY_J | GLFW_KEY_DOWN => selectedIdx = (selectedIdx + 1) % nearbyItems.size
        case GLFW_KEY_K | GLFW_KEY_UP => selectedIdx = (selectedIdx + nearbyItems.size - 1) % nearbyItems.size
        case GLFW_KEY_E =>
          if (selectedIdx >= 0 && selectedIdx < nearbyItems.size)
            display.pushScreen(new ExamineScreen(display, state, nearbyItems(selectedIdx)._2))
        case GLFW_KEY_G =>
          if (selectedIdx >= 0 && selectedIdx < nearbyItems.size)
            display.pushAction(Action.PickUp(nearbyItems(selectedIdx)._2))
        case GLFW_KEY_D =>
          if (selectedIdx >= 0 && selectedIdx < nearbyItems.size && nearbyItems(selectedIdx)._2.isInstanceOf[InHands])
            display.pushAction(Action.PutDown(nearbyItems(selectedIdx)._2))
        case _ =>
      }
  }

  override def render(renderer: Renderer): Unit = {
    val anchor = (1, 1)
    val width = 30
    renderer.drawBox(anchor._1, anchor._2, width, anchor._2 + 2 + nearbyItems.size)
    renderer.drawString(anchor._1 + 1, anchor._2 + 1, "Nearby")
    for (((item, pos), i) <- nearbyItems.zipWithIndex) {
      renderer.drawString(anchor._1 + 2, anchor._2 + 2 + i, item.kind.name, maxWidth = width - 3 - 2)
      renderer.drawString(width - 2, anchor._2 + 2 + i, directionString(pos))
    }
    if (nearbyItems.nonEmpty)
      renderer.drawString(anchor._1 + 1, anchor._2 + 2 + selectedIdx, ">")
  }

  def directionString(pos: ItemLocation): String = {
    pos match {
      case OnFloor(x, y, _) =>
        val dx = x - state.player._1
        val dy = y - state.player._2
        if (dx < 0) {
          if (dy < 0) "NW"
          else if (dy == 0) "W"
          else "SW"
        } else if (dx == 0) {
          if (dy < 0) "N"
          else if (dy == 0) "."
          else "S"
        } else {
          if (dy < 0) "NE"
          else if (dy == 0) "E"
          else "SE"
        }
      case InHands(_) =>
        "H"
    }
  }
}

class GLFWDisplay extends Display {
  private var window: Long = 0
  private var upper: Texture = _
  private var lower: Texture = _
  private var spriteBatch: SpriteBatch = _
  private var lastState: GameState = _
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
        image.bytes.putInt(i, 0xffffffff)
      } else {
        image.bytes.putInt(i, 0x00000000)
      }
    }
    image
  }

  def charForTerrain(terrain: Terrain): Int = terrain match {
    case Terrain.EmptySpace => ' '
    case Terrain.Floor => '.'
    case Terrain.Grass => ','
    case Terrain.TreeOak => PETSCII.Spades
    case Terrain.TreeFicus => PETSCII.Clubs
    case Terrain.GlassWall => PETSCII.UpperRightTriangle
  }

  def charForItem(item: Item): Int = item.kind match {
    case items.HoloNote => '!'
      // Tiny components `
    case items.MRAMChip => 39
    case items.TypeIAScrew => 39
    case items.Microprocessor => 39
    case items.TinyDCMotor => 39
    case items.LaserDiode => 39
      // Small components ◆
    case items.HolographicProjector => 90
    case items.RechargeableBattery => 90
    case items.Magnet => 90
    case items.Mirror => 90
      //
    case items.SmallPlasticCasing => 87 // ⊚
    case items.CopperWire => 93 // vertical squiggle
      // ???
    case _ => 127 // ▚
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
