package adrift.display

import adrift.Rect
import adrift.display.glutil.{SpriteBatch, Texture}
import org.lwjgl.BufferUtils
import org.lwjgl.glfw.GLFW._
import org.lwjgl.glfw.GLFWErrorCallback
import org.lwjgl.opengl.GL
import org.lwjgl.opengl.GL11._

case class Font(texture: Texture, tileWidth: Int, tileHeight: Int, scaleFactor: Int = 1)

class Graphics(val bounds: Rect, val spriteBatch: SpriteBatch) {
  def glyphs(font: Font): GlyphRenderer =
    new GlyphRenderer(
      spriteBatch,
      font.tileWidth,
      font.tileHeight,
      font.tileWidth * font.scaleFactor,
      font.tileHeight * font.scaleFactor,
      font.texture,
      Rect(0, 0, bounds.width / (font.tileWidth * font.scaleFactor), bounds.height / (font.tileHeight * font.scaleFactor))
    )
}

class GLFWWindow() {
  private var window: Long = 0
  private var spriteBatch: SpriteBatch = _

  def onChar(cb: Int => Unit): Unit =
    glfwSetCharCallback(window, (window: Long, c: Int) => cb(c))
  def onKey(cb: (Int, Int, Int, Int) => Unit): Unit =
    glfwSetKeyCallback(
      window,
      (window: Long, key: Int, scancode: Int, action: Int, mods: Int) => cb(key, scancode, action, mods)
    )
  def onClose(cb: () => Unit): Unit =
    glfwSetWindowCloseCallback(window, (window: Long) => cb())

  def init(width: Int = 640, height: Int = 480, title: String = "Adrift"): Unit = {
    if (window != 0) return // already initialized
    GLFWErrorCallback.createPrint(System.err).set()

    if (!glfwInit()) {
      throw new IllegalStateException("Unable to initialize GLFW")
    }

    glfwWindowHint(GLFW_VISIBLE, GLFW_FALSE)
    glfwWindowHint(GLFW_RESIZABLE, GLFW_FALSE)

    window = glfwCreateWindow(width, height, title, 0, 0)
    if (window == 0)
      throw new RuntimeException("Failed to create GLFW window")

    glfwMakeContextCurrent(window)
    glfwSwapInterval(1)

    GL.createCapabilities()

    spriteBatch = SpriteBatch.create
  }

  def framebufferSize: (Int, Int) = {
    val wBuf = BufferUtils.createIntBuffer(1)
    val hBuf = BufferUtils.createIntBuffer(1)
    glfwGetFramebufferSize(window, wBuf, hBuf)
    (wBuf.get(), hBuf.get())
  }
  def setSize(width: Int, height: Int): Unit =
    glfwSetWindowSize(window, width, height)
  def setPosition(x: Int, y: Int): Unit =
    glfwSetWindowPos(window, x, y)
  def center(): Unit = {
    val monitor = glfwGetPrimaryMonitor()
    val vidmode = glfwGetVideoMode(monitor)
    val sw = vidmode.width()
    val sh = vidmode.height()
    val (ww, wh) = size
    setPosition(sw / 2 - ww / 2, sh / 2 - wh / 2)
  }
  def size: (Int, Int) = {
    val wBuf = BufferUtils.createIntBuffer(1)
    val hBuf = BufferUtils.createIntBuffer(1)
    glfwGetWindowSize(window, wBuf, hBuf)
    (wBuf.get(), hBuf.get())
  }

  def isVisible: Boolean =
    glfwGetWindowAttrib(window, GLFW_VISIBLE) == GLFW_TRUE

  def show(): Unit = glfwShowWindow(window)

  def shouldClose: Boolean = glfwWindowShouldClose(window)

  def render(f: Graphics => Unit): GLFWWindow = {
    val (fbWidth, fbHeight) = framebufferSize

    glViewport(0, 0, fbWidth, fbHeight)
    val (windowWidth, windowHeight) = size
    spriteBatch.resize(windowWidth, windowHeight)

    glEnable(GL_BLEND)
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)

    glClearColor(0, 0, 0, 1)
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)

    spriteBatch.begin()

    try {
      f(new Graphics(Rect(0, 0, size._1, size._2), spriteBatch))
    } finally {
      spriteBatch.end()
    }


    glfwSwapBuffers(window)

    this
  }

  def poll(): Unit = glfwPollEvents()
}
