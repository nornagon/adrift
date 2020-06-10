package adrift

import adrift.display.GLFWWindow
import adrift.display.glutil._
import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.GL13._
import org.lwjgl.opengl.GL20.glUniform1i
import org.lwjgl.opengl.GL30._
import org.lwjgl.opengl.GLDebugMessageCallback
import org.lwjgl.system.MemoryUtil

import scala.util.Random

class Framebuffer(val texture: Texture) {
  def width: Int = texture.width
  def height: Int = texture.height
  private lazy val id = {
    val id = glGenFramebuffers()
    val previouslyBound = glGetInteger(GL_DRAW_FRAMEBUFFER_BINDING)
    glBindFramebuffer(GL_FRAMEBUFFER, id)
    glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, texture.id, 0)
    val status = glCheckFramebufferStatus(GL_FRAMEBUFFER)
    assert(status == GL_FRAMEBUFFER_COMPLETE, s"Framebuffer status was $status (${framebufferStatusToString(status)})")
    glClearColor(0.0f, 0.0f, 0.0f, 1.0f)
    glClear(GL_COLOR_BUFFER_BIT)
    glBindFramebuffer(GL_FRAMEBUFFER, previouslyBound)
    glCheckError()
    id
  }

  def bind(): Unit = glBindFramebuffer(GL_FRAMEBUFFER, id)

  private def framebufferStatusToString(status: Int): String = status match {
    case GL_FRAMEBUFFER_COMPLETE => "GL_FRAMEBUFFER_COMPLETE"
    case GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT => "GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT"
    case GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT => "GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT"
    case GL_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER => "GL_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER"
    case GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER => "GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER"
    case GL_FRAMEBUFFER_UNSUPPORTED => "GL_FRAMEBUFFER_UNSUPPORTED"
    case GL_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE => "GL_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE"
    case GL_FRAMEBUFFER_UNDEFINED => "GL_FRAMEBUFFER_UNDEFINED"
    case _ => s"unknown"
  }

  def dispose(): Unit = {
    glDeleteFramebuffers(id)
    texture.dispose()
  }
}

object AtmoSim {
  def diffusionPass: RenderPass = RenderPass.fragment("""
    #version 140

    uniform sampler2D heat;
    in vec2 vTexCoord;

    out float res;

    void main() {
      ivec2 texSize = textureSize(heat, 0);
      vec3 texelSize = vec3(1.0 / texSize.x, 1.0 / texSize.y, 0.0);
      float l = texture(heat, vTexCoord - texelSize.xz).r;
      float r = texture(heat, vTexCoord + texelSize.xz).r;
      float u = texture(heat, vTexCoord - texelSize.zy).r;
      float d = texture(heat, vTexCoord + texelSize.zy).r;
      float c = texture(heat, vTexCoord).r;
      float sigma = 0.5;
      float dt = 1.0;
      res = mix(c, (l + r + u + d) / 4.0, 1.0-exp(-sigma*dt));
    }
  """)
}

class AtmoSim(val width: Int, val height: Int) {
  lazy private val diffusionPass = AtmoSim.diffusionPass

  //val heat = new Grid[Float](width, height)(250f)

  def step(heat: Grid[Float]): Unit = {
    // 1. copy from output to input
    //    (also: update map if necessary)
    // 2. run sim
    // 3. read

    val width = heat.width
    val height = heat.height

    val source = Texture.fromFloatArray(width, height, heat.cells.toArray)
    glCheckError()
    val target = new Framebuffer(Texture.emptyFloat(width, height))
    glCheckError()
    diffusionPass.renderToFramebuffer(Map("heat" -> source), target)
    glCheckError()
    val buf = target.texture.readFloat()
    val arr = new Array[Float](width * height)
    buf.get(arr)
    println(arr.toSeq.slice(0, 32))
    for (i <- heat.cells.indices) heat.cells(i) = arr(i)
    source.dispose()
    target.dispose()
    glCheckError()
  }
}

class RenderPass(program: ShaderProgram, attribs: Seq[VertexAttrib]) {
  lazy private val quad = {
    val va = VertexArray.create(6, attribs)
    def vertex(x: Float, y: Float, u: Float, v: Float): Unit = va.put(x).put(y).put(u).put(v)

    vertex(-1f, -1f, 0f, 0f)
    vertex(1f, -1f, 1f, 0f)
    vertex(-1f, 1f, 0f, 1f)

    vertex(1f, -1f, 1f, 0f)
    vertex(1f, 1f, 1f, 1f)
    vertex(-1f, 1f, 0f, 1f)

    va
  }
  def renderToFramebuffer(textures: Map[String, Texture], target: Framebuffer): Unit = {
    program.use()
    for (((k, t), i) <- textures.zipWithIndex) {
      glActiveTexture(GL_TEXTURE0 + i)
      t.bind()
      glUniform1i(program.uniforms(k), i)
    }
    target.bind()
    glViewport(0, 0, target.width, target.height)
    quad.bind()
    quad.draw(GL_TRIANGLES, 0, 6)
    quad.unbind()
    glActiveTexture(GL_TEXTURE0)
    glCheckError()
  }

  def renderToScreen(source: Texture, width: Int, height: Int): Unit = {
    program.use()
    glUniform1i(program.uniforms("u_texture"), 0)
    source.bind()
    glBindFramebuffer(GL_FRAMEBUFFER, 0)
    glViewport(0, 0, width, height)
    quad.bind()
    quad.draw(GL_TRIANGLES, 0, 6)
    quad.unbind()
  }
}
object RenderPass {
  def fragment(source: String): RenderPass = {
    val attribs = Seq(
      VertexAttrib(0, "Position", 2),
      VertexAttrib(1, "TexCoord", 2)
    )
    val shader = ShaderProgram.compile(
      """
        #version 140
        in vec2 Position;
        in vec2 TexCoord;
        out vec2 vTexCoord;
        void main() {
          vTexCoord = TexCoord;
          gl_Position = vec4(Position.xy, 0.0, 1.0);
        }
      """,
      fsSource = source,
      attribs
    )
    new RenderPass(shader, attribs)
  }
}

object AtmoSimTest {
  val passthruVertexShader =
    """
        #version 330
      layout(location = 0) in vec2 Position;
      layout(location = 1) in vec2 TexCoord;
      out vec2 vTexCoord;
      void main() {
        vTexCoord = TexCoord;
        gl_Position = vec4(Position.xy, 0.0, 1.0);
      }
    """

  def main(args: Array[String]): Unit = {
    val win = new GLFWWindow()
    win.init(512, 512)
    glCheckError()
    println(for (i <- 0 until glGetInteger(GL_NUM_EXTENSIONS)) yield glGetStringi(GL_EXTENSIONS, i))

    if (org.lwjgl.glfw.GLFW.glfwExtensionSupported("GL_ARB_debug_output")) {
      println("Debug output supported")
      import org.lwjgl.opengl.GL43._
      glEnable(GL_DEBUG_OUTPUT)
      val cb = GLDebugMessageCallback.create((source: Int, `type`: Int, id: Int, severity: Int, length: Int, message: Long, userParam: Long) => {
        val msg = MemoryUtil.memUTF8(message)
        println(s"OpenGL error: source=$source, type=${`type`}, id=$id, sev=$severity, length=$length, message=$msg")
      })
      glDebugMessageCallback(cb, 0)
    }

    val toScreenPass = RenderPass.fragment("""
      #version 140
      uniform sampler2D u_texture;
      in vec2 vTexCoord;
      out vec4 color;
      void main() {
        vec4 texColor = texture(u_texture, vTexCoord);
        color = vec4((texColor.r - 250.) / 20., 0,0,1);
      }
    """)

    val pass = AtmoSim.diffusionPass
    val (fbw, fbh) = win.framebufferSize
    val tex1 = Texture.fromFloatArray(fbw, fbh, Array.fill(fbw * fbh)(Random.nextFloat() * 20 + 250))
    val fb1 = new Framebuffer(tex1)
    val tex2 = Texture.emptyFloat(fbw, fbh)
    val fb2 = new Framebuffer(tex2)

    var fbs = (fb1, fb2)

    def step(): Unit = {
      pass.renderToFramebuffer(Map("heat" -> fbs._1.texture), fbs._2)
      fbs = fbs.swap
    }

    def draw(): Unit = {
      toScreenPass.renderToScreen(fbs._2.texture, fbw, fbh)
    }

    win.show()

    while (!win.shouldClose) {
      step()
      draw()
      win.swap()
      win.poll()
    }

  }


  def old(): Unit = {
    val win = new GLFWWindow()
    win.init(512, 512)
    val attribs = Seq(
      VertexAttrib(0, "Position", 2),
      VertexAttrib(1, "TexCoord", 2)
    )
    val magentaShader = ShaderProgram.compile(
      passthruVertexShader,
      """
        #version 140
        out vec4 color;
        void main() {
          color = vec4(1.0, 0.0, 1.0, 1.0);
        }
      """,
      attribs
    )
    glCheckError()
    val magentaPass = new RenderPass(magentaShader, attribs)
    val addSinXShader = ShaderProgram.compile(
      passthruVertexShader,
      """
        #version 140
        uniform sampler2D u_texture;
        in vec2 vTexCoord;
        out vec4 color;
        void main() {
          vec4 texColor = texture(u_texture, vTexCoord);
          color = texColor + vec4(-sin(vTexCoord.x * vTexCoord.y * 50.0), 0.0, 0.0, 0.0);
        }
      """,
      attribs
    )
    glCheckError()
    val addSinXPass = new RenderPass(addSinXShader, attribs)
    glCheckError()
    val addCosXShader = ShaderProgram.compile(
      passthruVertexShader,
      """
        #version 140
        uniform sampler2D u_texture;
        in vec2 vTexCoord;
        out vec4 color;
        void main() {
          vec4 texColor = texture(u_texture, vTexCoord);
          color = texColor + vec4(0.0, -cos(vTexCoord.x * vTexCoord.y * 50.0), 0.0, 0.0);
        }
      """,
      attribs
    )
    glCheckError()
    val addCosXPass = new RenderPass(addCosXShader, attribs)
    glCheckError()

    glClearColor(0, 255, 0, 1)
    glClear(GL_COLOR_BUFFER_BIT)
    val (fbW, fbH) = win.framebufferSize
    glCheckError()

    val fb = new Framebuffer(Texture.empty(512, 512))
    magentaPass.renderToFramebuffer(Map.empty, fb)
    val fb2 = new Framebuffer(Texture.empty(512, 512))
    addSinXPass.renderToFramebuffer(Map("u_texture" -> fb.texture), fb2)
    val bb = fb2.texture.read()
    println("fb2", bb.get(0), bb.get(1), bb.get(2), bb.get(3))
    addCosXPass.renderToScreen(fb2.texture, fbW, fbH)

    glCheckError()

    win.swap()
    win.show()
    win.poll()

    while (!win.shouldClose) {
      win.waitEvents()
    }
  }
}
