package adrift.display

import adrift.display.glutil.{Image, SpriteBatch, Texture}
import adrift._
import adrift.items.{DoorOpen, Item}
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
      // TODO: encode item display in yaml
      /*
    case items.HoloNote => '!'
    // Tiny components `
    case items.MRAMChip => '`'
    case items.Screw(_,_,_) => '`'
    case items.Microprocessor => '`'
    case items.TinyDCMotor => '`'
    case items.LaserDiode => '`'
    // Small components ◆
    case items.HolographicProjector => CP437.Diamonds
    case items.RechargeableBattery => CP437.Diamonds
    case items.Magnet => CP437.Diamonds
    case items.FlatMirror => CP437.Diamonds
    case items.LaserPump => CP437.Diamonds
    //
    case items.SmallPlasticCasing => CP437.Bullet // ⊚
    case items.CopperWire => CP437.Delta // δ
    */
    // ???
    case _ => 2 // ☻
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

class GlyphRenderer(
  spriteBatch: SpriteBatch,
  tileWidth: Int,
  tileHeight: Int,
  screenTileWidth: Int,
  screenTileHeight: Int,
  font: Texture,
) {
  private val tilesPerRow: Int = font.width / tileWidth
  require(font.width / tileWidth.toFloat - tilesPerRow == 0)

  def drawChar(
    tex: Texture,
    x: Int,
    y: Int,
    c: Int,
    fg: (Float, Float, Float, Float) = (1f, 1f, 1f, 1f),
    bg: (Float, Float, Float, Float) = (0f, 0f, 0f, 1f)
  ): Unit = {
    if (bg._4 != 0) {
      val cx = 0xdb % tilesPerRow
      val cy = 0xdb / tilesPerRow
      spriteBatch.drawRegion(
        tex,
        cx * tileWidth, cy * tileHeight,
        tileWidth, tileHeight,
        x * screenTileWidth, y * screenTileHeight,
        screenTileWidth, screenTileHeight,
        bg
      )
    }
    val cx = c % tilesPerRow
    val cy = c / tilesPerRow
    spriteBatch.drawRegion(
      tex,
      cx * tileWidth, cy * tileHeight,
      tileWidth, tileHeight,
      x * screenTileWidth, y * screenTileHeight,
      screenTileWidth, screenTileHeight,
      fg
    )
  }

  def drawBox(x: Int, y: Int, w: Int, h: Int): Unit = {
    import CP437.BoxDrawing
    drawChar(font, x, y, BoxDrawing.__RD)
    drawChar(font, x + w - 1, y, BoxDrawing.L__D)
    drawChar(font, x, y + h - 1, BoxDrawing._UR_)
    drawChar(font, x + w - 1, y + h - 1, BoxDrawing.LU__)
    for (iy <- 1 until (h - 1); ix <- 1 until (w - 1))
      drawChar(font, x + ix, y + iy, ' ')
    for (ix <- 1 until (w - 1)) {
      drawChar(font, x + ix, y, BoxDrawing.L_R_)
      drawChar(font, x + ix, y + h - 1, BoxDrawing.L_R_)
    }
    for (iy <- 1 until (h - 1)) {
      drawChar(font, x, y + iy, BoxDrawing._U_D)
      drawChar(font, x + w - 1, y + iy, BoxDrawing._U_D)
    }
  }

  def drawString(x: Int, y: Int, s: String, maxWidth: Int = 0): Unit = {
    for ((c, i) <- s.zipWithIndex) {
      if (maxWidth != 0 && i >= maxWidth) return
      drawChar(font, x + i, y, c)
    }
  }

  def wrapString(maxWidth: Int, maxHeight: Int, s: String): Seq[String] = {
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
    for ((line, cy) <- wrapString(maxWidth, maxHeight, s).zipWithIndex) {
      drawString(x, y + cy, line)
    }
  }

  def frame(left: Int = 0, top: Int = 0, width: Int = 0, title: String = null, lines: Seq[String]): Unit = {
    drawBox(left, top, width, lines.size + 2)
    if (title != null)
      drawString(left + 1, top, s"[$title]", maxWidth = width-2)
    for ((l, i) <- lines.zipWithIndex) {
      drawString(left + 1, top + 1 + i, l, width - 2)
    }
  }
}

trait Screen {
  def key(key: Int, scancode: Int, action: Int, mods: Int): Unit
  def render(renderer: GlyphRenderer): Unit
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

  override def render(renderer: GlyphRenderer): Unit = {
    val itemsByKind = item.parts.groupBy(_.kind)
    val width = 30
    val descriptionLines = renderer.wrapString(maxWidth = width - 2, maxHeight = 9, item.kind.description)
    renderer.frame(
      left = anchor._1, top = anchor._2,
      width = width,
      title = item.kind.name,
      lines = descriptionLines ++
        Seq("", "Parts:") ++
        itemsByKind.map {
          case (kind, items) if items.size == 1 => kind.name
          case (kind, items) => s"${items.size} x ${kind.name}"
        }
    )
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

  override def render(renderer: GlyphRenderer): Unit = {
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

class LookScreen(display: GLFWDisplay, state: GameState) extends Screen {
  var (x, y) = state.player
  override def key(key: Int, scancode: Int, action: Int, mods: Int): Unit = {
    if (action == GLFW_PRESS || action == GLFW_REPEAT) {
      key match {
        case GLFW_KEY_H | GLFW_KEY_LEFT => x -= 1
        case GLFW_KEY_J | GLFW_KEY_DOWN => y += 1
        case GLFW_KEY_K | GLFW_KEY_UP => y -= 1
        case GLFW_KEY_L | GLFW_KEY_RIGHT => x += 1
        case GLFW_KEY_Y => x -= 1; y -= 1
        case GLFW_KEY_U => x += 1; y -= 1
        case GLFW_KEY_B => x -= 1; y += 1
        case GLFW_KEY_N => x += 1; y += 1
        case _ =>
      }
    }
  }

  override def render(renderer: GlyphRenderer): Unit = {
    val char = Appearance.charAtPosition(state, x, y)
    val (left, right, top, bottom) = display.cameraBounds(state)
    renderer.drawChar(display.font, x - left, y - top, char, fg=(0, 0, 0, 1), bg=(1,1,1,1))
    val width = 20
    val anchor =
      if (x - left <= (right - left) / 2 - 1)
        (display.windowWidthChars - 1 - width, 1)
      else
        (1, 1)

    val terrain = state.map(x, y)
    val items = state.items(x, y)

    renderer.frame(
      left = anchor._1, top = anchor._2,
      width = width,
      lines = Seq(terrain.toString) ++
        items.take(9).map(_.kind.name) ++
        (if (items.size > 9) Seq(s"${items.size - 9} more...") else Seq.empty)
    )
  }
}

class GLFWDisplay extends Display {
  private var window: Long = 0
  var font: Texture = _
  private var spriteBatch: SpriteBatch = _
  private var lastState: GameState = _
  private val pendingActions = mutable.Buffer.empty[Action]
  val windowWidthChars = 80
  val windowHeightChars = 48

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
    val windowWidthChars = 80*2
    val windowHeightChars = 48*2
    val screenCharWidth = 16/2
    val screenCharHeight = 16/2
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
