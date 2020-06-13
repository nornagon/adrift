package adrift.display

import adrift.Rect
import adrift.display.glutil.{SpriteBatch, Texture}
import org.lwjgl.BufferUtils
import org.lwjgl.glfw.GLFW._
import org.lwjgl.glfw.GLFWErrorCallback
import org.lwjgl.opengl.GL
import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.GL20._
import org.lwjgl.opengl.GL30._

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

    // 3.2 is the minimum version to be able to request a core profile.
    glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3)
    glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 2)
    glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE)
    glfwWindowHint(GLFW_OPENGL_FORWARD_COMPAT, GL_TRUE)
    glfwWindowHint(GLFW_OPENGL_DEBUG_CONTEXT, GL_TRUE)

    // Allows a dual-gpu mac to use its internal gpu. This has no effect on non-mac systems.
    glfwWindowHint(GLFW_COCOA_GRAPHICS_SWITCHING, GL_TRUE)

    glfwWindowHint(GLFW_VISIBLE, GLFW_FALSE)
    glfwWindowHint(GLFW_RESIZABLE, GLFW_FALSE)

    window = glfwCreateWindow(width, height, title, 0, 0)
    if (window == 0)
      throw new RuntimeException("Failed to create GLFW window")

    glfwMakeContextCurrent(window)
    glfwSwapInterval(1)

    GL.createCapabilities()

    println(s"OpenGL version: ${glGetString(GL_VERSION)}")
    println(s"  Vendor: ${glGetString(GL_VENDOR)}")
    println(s"  Renderer: ${glGetString(GL_RENDERER)}")
    println(s"  GLSL Version: ${glGetString(GL_SHADING_LANGUAGE_VERSION)}")
    println(s"  Extensions: ${(for (i <- 0 until glGetInteger(GL_NUM_EXTENSIONS)) yield glGetStringi(GL_EXTENSIONS, i)).mkString(", ")}")

    val vaid = glGenVertexArrays()
    glBindVertexArray(vaid)

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

    swap()

    this
  }

  def swap(): Unit = glfwSwapBuffers(window)

  def poll(): Unit = glfwPollEvents()
  def waitEvents(): Unit = glfwWaitEvents()
}
