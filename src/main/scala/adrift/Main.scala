package adrift

import java.nio.{ByteBuffer, FloatBuffer}

import org.lwjgl.BufferUtils
import org.lwjgl.glfw.GLFW._
import org.lwjgl.glfw.GLFWErrorCallback
import org.lwjgl.opengl.GL
import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.GL12._
import org.lwjgl.opengl.GL13._
import org.lwjgl.opengl.GL20._
import org.lwjgl.stb.STBImage._

import scala.collection.mutable

class Grid[T](val width: Int, val height: Int)(initial: T) {
  private val cells: mutable.Seq[T] = mutable.Seq.fill[T](width * height)(initial)
  def apply(x: Int, y: Int): T = {
    require(contains(x, y), s"Coordinate ($x, $y) was out of range $width x $height")
    cells(y * width + x)
  }
  def update(x: Int, y: Int, c: T): Unit = {
    require(contains(x, y), s"Coordinate ($x, $y) was out of range $width x $height")
    cells(y * width + x) = c
  }
  def contains(x: Int, y: Int): Boolean = x >= 0 && x < width && y >= 0 && y < height
  def indices: IndexedSeq[(Int, Int)] = for (y <- 0 until height; x <- 0 until width) yield (x, y)
}

object Main {
  val CHAR_W = 16
  val CHAR_H = 16
  var window: Long = 0

  var player: (Int, Int) = (10, 9)
  val map = new Grid[Int](80, 24)(46)
  val random = new scala.util.Random(42)
  for ((x, y) <- map.indices) {
    if (random.nextFloat() < 0.1) map(x, y) = 44
    if (random.nextFloat() < 0.05) map(x, y) = 65
  }

  def main(args: Array[String]): Unit = {
    GLFWErrorCallback.createPrint(System.err).set()

    if (!glfwInit()) {
      throw new IllegalStateException("Unable to initialize GLFW")
    }

    glfwWindowHint(GLFW_VISIBLE, GLFW_FALSE)
    glfwWindowHint(GLFW_RESIZABLE, GLFW_TRUE)

    window = glfwCreateWindow(CHAR_W * map.width, CHAR_H * map.height, "Adrift", 0, 0)
    if (window == 0)
      throw new RuntimeException("Failed to create GLFW window")

    glfwSetKeyCallback(window, (window: Long, key: Int, scancode: Int, action: Int, mods: Int) => {
      if (action == GLFW_PRESS || action == GLFW_REPEAT) {
        key match {
          case GLFW_KEY_LEFT => player = (player._1 - 1, player._2)
          case GLFW_KEY_RIGHT => player = (player._1 + 1, player._2)
          case GLFW_KEY_UP => player = (player._1, player._2 - 1)
          case GLFW_KEY_DOWN => player = (player._1, player._2 + 1)
          case _ =>
        }
      }
    })

    glfwMakeContextCurrent(window)
    glfwSwapInterval(1)
    glfwShowWindow(window)
    GL.createCapabilities()

    loop()
  }

  def loop(): Unit = {
    glClearColor(0, 0, 0, 1)

    glActiveTexture(GL_TEXTURE0)
    val image = Image.fromFile("c64_upp.gif")
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
    val texture = Texture.fromImage(image)
    val shader = ShaderProgram.compile(vs, fs, shaderAttribs)

    val spriteBatch = new SpriteBatch(shader)
    spriteBatch.resize(CHAR_W * map.width, CHAR_H * map.height)

    def drawChar(x: Int, y: Int, c: Int): Unit = {
      val cx = c % 32
      val cy = c / 32
      spriteBatch.drawRegion(texture, cx * 8, cy * 8, 8, 8, x * CHAR_W, y * CHAR_H, CHAR_W, CHAR_H)
    }

    while (!glfwWindowShouldClose(window)) {
      glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)

      spriteBatch.begin()
      for (y <- 0 until map.height; x <- 0 until map.width) {
        if (x == player._1 && y == player._2) drawChar(x, y, 0)
        else drawChar(x, y, map(x, y))
      }
      spriteBatch.end()

      glfwSwapBuffers(window)
      glfwWaitEvents()
    }
  }


  /// The below was cribbed from https://github.com/mattdesl/lwjgl-basics/blob/master/src/mdesl/graphics/SpriteBatch.java

  class SpriteBatch(program: ShaderProgram) {
    private val maxIndex: Int = 1000 * 6
    private val data = VertexArray.create(maxIndex, shaderAttribs)
    private var drawing = false
    private var idx: Int = 0
    private var texture: Texture = _

    def resize(width: Int, height: Int): Unit = {
      program.use()
      val left: Float = 0
      val right: Float = width
      val top: Float = 0
      val bottom: Float = height
      val zFar: Float = -1
      val zNear: Float = 1
      val tx = -(right + left) / (right - left)
      val ty = -(top + bottom) / (top - bottom)
      val tz = -(zFar + zNear) / (zFar - zNear)
      val projMat = Array[Float](
        2 / (right - left), 0, 0, tx,
        0, 2 / (top - bottom), 0, ty,
        0, 0, -2 / (zFar - zNear), tz,
        0, 0, 0, 1
      )
      glUniformMatrix4fv(program.uniforms("u_projView"), true, projMat)
      glUniform1i(program.uniforms("u_texture"), 0)
    }

    def begin(): Unit = {
      assert(!drawing)
      drawing = true
      program.use()
      idx = 0
      texture = null
    }

    def end(): Unit = {
      assert(drawing)
      drawing = false
      flush()
    }

    def flush(): Unit = {
      if (idx > 0) {
        data.flip()
        render()
        idx = 0
        data.buffer.clear()
      }
    }

    def drawRegion(tex: Texture, srcX: Float, srcY: Float, srcWidth: Float, srcHeight: Float, dstX: Float, dstY: Float): Unit = {
      drawRegion(tex, srcX, srcY, srcWidth, srcHeight, dstX, dstY, srcWidth, srcHeight)
    }
    def drawRegion(tex: Texture, srcX: Float, srcY: Float, srcWidth: Float, srcHeight: Float, dstX: Float, dstY: Float, dstWidth: Float, dstHeight: Float): Unit = {
      val u = srcX / tex.width
      val v = srcY / tex.height
      val u2 = (srcX + srcWidth) / tex.width
      val v2 = (srcY + srcHeight) / tex.height
      draw(tex, dstX, dstY, dstWidth, dstHeight, u, v, u2, v2)
    }

    def draw(tex: Texture, x: Float, y: Float, width: Float, height: Float, u: Float, v: Float, u2: Float, v2: Float): Unit = {
      checkFlush(tex)

      val x1 = x
      val y1 = y

      val x2 = x + width
      val y2 = y

      val x3 = x + width
      val y3 = y + height

      val x4 = x
      val y4 = y + height

      val r = 1.0f
      val g = 1.0f
      val b = 1.0f
      val a = 1.0f

      vertex(x1, y1, r, g, b, a, u, v)
      vertex(x2, y2, r, g, b, a, u2, v)
      vertex(x4, y4, r, g, b, a, u, v2)

      vertex(x2, y2, r, g, b, a, u2, v)
      vertex(x3, y3, r, g, b, a, u2, v2)
      vertex(x4, y4, r, g, b, a, u, v2)
    }

    def vertex(x: Float, y: Float, r: Float, g: Float, b: Float, a: Float, u: Float, v: Float): Unit = {
      data.put(x).put(y).put(r).put(g).put(b).put(a).put(u).put(v)
      idx += 1
    }

    private def checkFlush(tex: Texture): Unit = {
      if (!(texture eq tex) || idx >= maxIndex) {
        flush()
        texture = tex
      }
    }

    private def render(): Unit = {
      if (texture != null)
        texture.bind()
      data.bind()
      data.draw(GL_TRIANGLES, 0, idx)
      data.unbind()
    }
  }

  val shaderAttribs = Seq(
    VertexAttrib(0, "Position", 2),
    VertexAttrib(1, "Color", 4),
    VertexAttrib(2, "TexCoord", 2)
  )
  val vs =
    """
      uniform mat4 u_projView;
      attribute vec4 Color;
      attribute vec2 Position;
      attribute vec2 TexCoord;
      varying vec4 vColor;
      varying vec2 vTexCoord;
      void main() {
        vColor = Color;
        vTexCoord = TexCoord;
        gl_Position = u_projView * vec4(Position.xy, 0.0, 1.0);
      }
    """

  val fs =
    """
      uniform sampler2D u_texture;
      varying vec4 vColor;
      varying vec2 vTexCoord;
      void main() {
        vec4 texColor = texture2D(u_texture, vTexCoord);
        gl_FragColor = vColor * texColor;
      }
    """

  case class VertexArray(buffer: FloatBuffer, attributes: Seq[VertexAttrib]) {
    val stride: Int = attributes.map(_.numComponents).sum * 4
    def put(f: Float): VertexArray = { buffer.put(f); this }
    def put(fs: Array[Float], offset: Int, length: Int): VertexArray = { buffer.put(fs, offset, length); this }
    def flip(): VertexArray = { buffer.flip(); this }
    def bind(): Unit = {
      var offset = 0
      attributes foreach { a =>
        buffer.position(offset)
        glEnableVertexAttribArray(a.location)
        glVertexAttribPointer(a.location, a.numComponents, GL_FLOAT, false, stride, buffer)
        offset += a.numComponents
      }
    }
    def draw(geom: Int, first: Int, count: Int): Unit = glDrawArrays(geom, first, count)
    def unbind(): Unit = attributes foreach { a => glDisableVertexAttribArray(a.location) }
  }
  object VertexArray {
    def create(count: Int, attributes: Seq[VertexAttrib]): VertexArray = {
      val stride = attributes.map(_.numComponents).sum
      val buffer = BufferUtils.createFloatBuffer(count * stride)
      VertexArray(buffer, attributes)
    }
  }

  case class VertexAttrib(location: Int, name: String, numComponents: Int)

  case class Attrib(name: String, size: Int, attribType: Int, location: Int)

  case class ShaderProgram(program: Int, uniforms: Map[String, Int], attribs: Map[String, Attrib]) {
    def use(): Unit = glUseProgram(program)
    def setUniformMatrix(loc: Int, transpose: Boolean, m: Array[Float]): Unit = {
      glUniformMatrix4fv(loc, transpose, m)
    }
  }
  object ShaderProgram {
    def compile(vsSource: String, fsSource: String, attribLocations: Seq[VertexAttrib]): ShaderProgram = {
      val vs = compileShader(GL_VERTEX_SHADER, vsSource)
      val fs = compileShader(GL_FRAGMENT_SHADER, fsSource)
      val program = glCreateProgram()

      for (VertexAttrib(index, name, _) <- attribLocations) {
        glBindAttribLocation(program, index, name)
      }

      glAttachShader(program, vs)
      glAttachShader(program, fs)
      glLinkProgram(program)
      val comp = glGetProgrami(program, GL_LINK_STATUS)
      val len = glGetProgrami(program, GL_INFO_LOG_LENGTH)
      val err = glGetProgramInfoLog(program, len)
      if (comp == GL_FALSE)
        throw new RuntimeException(s"Error linking shader program: $err")

      val numUniforms = glGetProgrami(program, GL_ACTIVE_UNIFORMS)

      val uniforms: Map[String, Int] = (0 until numUniforms).map({ i =>
        val size = BufferUtils.createIntBuffer(1)
        val ty = BufferUtils.createIntBuffer(1)
        val name = glGetActiveUniform(program, i, size, ty)
        val id = glGetUniformLocation(program, name)
        name -> id
      })(collection.breakOut)

      val numAttributes = glGetProgrami(program, GL_ACTIVE_ATTRIBUTES)
      val attribs: Map[String, Attrib] = (0 until numAttributes).map({ i =>
        val size = BufferUtils.createIntBuffer(1)
        val ty = BufferUtils.createIntBuffer(1)
        val name = glGetActiveAttrib(program, i, size, ty)
        val loc = glGetAttribLocation(program, name)
        name -> Attrib(name, size.get(0), ty.get(0), loc)
      })(collection.breakOut)

      ShaderProgram(program, uniforms, attribs)
    }

    private def compileShader(shaderType: Int, source: String): Int = {
      val shader = glCreateShader(shaderType)
      glShaderSource(shader, source)
      glCompileShader(shader)
      val comp = glGetShaderi(shader, GL_COMPILE_STATUS)
      val len = glGetShaderi(shader, GL_INFO_LOG_LENGTH)
      val err = glGetShaderInfoLog(shader, len)
      if (comp == GL_FALSE)
        throw new RuntimeException(s"Error compiling shader: $err")
      shader
    }
  }

  case class Texture(id: Int, width: Int, height: Int) {
    def bind(): Unit = glBindTexture(GL_TEXTURE_2D, id)
  }
  object Texture {
    def fromImage(image: Image): Texture = {
      fromPixels(image.width, image.height, image.bytes)
    }

    def fromPixels(width: Int, height: Int, bytes: ByteBuffer): Texture = {
      glEnable(GL_TEXTURE_2D)
      val id = glGenTextures()
      glBindTexture(GL_TEXTURE_2D, id)
      glPixelStorei(GL_UNPACK_ALIGNMENT, 1)
      glPixelStorei(GL_PACK_ALIGNMENT, 1)
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST)
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST)
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE)
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE)
      glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, width, height, 0, GL_RGBA, GL_UNSIGNED_BYTE, bytes)
      Texture(id, width, height)
    }
  }

  case class Image(width: Int, height: Int, channels: Int, bytes: ByteBuffer)
  object Image {
    def fromFile(filename: String): Image = {
      val wBuf = BufferUtils.createIntBuffer(1)
      val hBuf = BufferUtils.createIntBuffer(1)
      val compBuf = BufferUtils.createIntBuffer(1)
      val imageBytes = stbi_load(filename, wBuf, hBuf, compBuf, 0)
      if (imageBytes == null)
        throw new RuntimeException(s"Failed to load image at '$filename': ${stbi_failure_reason()}")
      Image(
        wBuf.get(0),
        hBuf.get(0),
        compBuf.get(0),
        imageBytes
      )
    }
  }
}
