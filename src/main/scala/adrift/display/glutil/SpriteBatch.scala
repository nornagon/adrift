package adrift.display.glutil

import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.GL20._

object SpriteBatch {
  /// Cribbed from https://github.com/mattdesl/lwjgl-basics/blob/master/src/mdesl/graphics/SpriteBatch.java

  def create: SpriteBatch = {
    val shaderAttribs = Seq(
      VertexAttrib(0, "Position", 2),
      VertexAttrib(1, "Color", 4),
      VertexAttrib(2, "TexCoord", 2)
    )

    val vs = """
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

    val fs = """
      uniform sampler2D u_texture;
      varying vec4 vColor;
      varying vec2 vTexCoord;
      void main() {
        vec4 texColor = texture2D(u_texture, vTexCoord);
        gl_FragColor = vColor * texColor;
      }
    """

    new SpriteBatch(ShaderProgram.compile(vs, fs, shaderAttribs), shaderAttribs)
  }
}

class SpriteBatch private (program: ShaderProgram, shaderAttribs: Seq[VertexAttrib]) {
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
