package adrift

import adrift.display.GLFWWindow
import adrift.display.glutil.*
import org.lwjgl.opengl.GL11.*
import org.lwjgl.opengl.GL13.*
import org.lwjgl.opengl.GL20.glUniform1i
import org.lwjgl.opengl.GL30.*
import org.lwjgl.opengl.GLDebugMessageCallback
import org.lwjgl.system.MemoryUtil

import scala.util.Random

object AtmoSim {
  // from: https://people.eecs.berkeley.edu/~demmel/cs267/lecture17/lecture17.html#link_1.4
  // the "explicit Euler algorithm"
  //
  // let a = dt/(dx*dx)
  // let c = U(x,y,t), l = U(x-1,y,t), r = U(x+1,y,t), u = U(x,y-1,t), d = U(x,y+1,t)
  //
  // then the update is:
  // U(x,y,t+1) = (1-4*a) * c + a*(l + r + u + d)
  //            = c - 4*a*c + a*(l + r + u + d)
  //            = c + a*(l + r + u + d - 4 * c)
  //            = c + a * ((l-c) + (r-c) + (u-c) + (d-c))
  // i.e. temperature at t+1 is increased by an amount proportional to the sum of the
  // differences between the cell and its neighbors.
  // a must be <= 0.5 for a stable solution.
  // i.e. dt <= (dx * dx) / 2
  //
  // intuitively, we can handle anisotropic transfer rates by attenuating each of the terms!
  // i.e.
  // U(x,y,t+1) = c + a * ((l-c)*hl + (r-c)*hr + (u-c)*hu + (d-c)*hd)
  // in order to be consistent, hr of U(x,y) must match hl of U(x+1,y) and so on.

  def diffusionPass: RenderPass = RenderPass.fragment("""
    #version 140

    uniform sampler2D temperature;
    uniform sampler2D heatTransfer;
    uniform sampler2D heatCapacity;
    in vec2 vTexCoord;

    out vec4 res;

    // dt <= 0.5 * dx * dx or the system will be unstable
    // [later]: apparently 0.25 is the highest value that works here when dx=1? idk why that is.
    float dt = 0.25;
    float dx = 1.0;
    float a = dt / (dx * dx);

    void main() {
      ivec2 texSize = textureSize(temperature, 0);
      vec3 texelSize = vec3(1.0 / texSize.x, 1.0 / texSize.y, 0.0);
      vec4 l = texture(temperature, vTexCoord - texelSize.xz);
      vec4 r = texture(temperature, vTexCoord + texelSize.xz);
      vec4 u = texture(temperature, vTexCoord - texelSize.zy);
      vec4 d = texture(temperature, vTexCoord + texelSize.zy);
      vec4 c = texture(temperature, vTexCoord);

      vec4 hc = texture(heatTransfer, vTexCoord);
      vec4 hl = texture(heatTransfer, vTexCoord - texelSize.xz) * hc;
      vec4 hr = texture(heatTransfer, vTexCoord + texelSize.xz) * hc;
      vec4 hu = texture(heatTransfer, vTexCoord - texelSize.zy) * hc;
      vec4 hd = texture(heatTransfer, vTexCoord + texelSize.zy) * hc;

      vec4 capacity = texture(heatCapacity, vTexCoord);

      res = c + a * ( (l - c) * hl + (r - c) * hr + (u - c) * hu + (d - c) * hd ) / capacity;
    }
  """)
}

class AtmoSim(val width: Int, val height: Int) {
  lazy private val diffusionPass = AtmoSim.diffusionPass
  private val transferTexture = Texture.emptyFloat4(width, height)
  private val heatCapacityTexture = Texture.emptyFloat4(width, height)

  def updateTransferTexture(heatTransfer: Grid[Float], permeability: (Int, Int) => Float): Unit = {
    assert(heatTransfer.width == width && heatTransfer.height == height)
    val combined = new Array[Float](width * height * 4)
    for ((x, y) <- heatTransfer.indices) {
      val i = (y * width + x) * 4
      combined(i + 0) = heatTransfer(x, y)
      val p = permeability(x, y)
      combined(i + 1) = p
      combined(i + 2) = p
      combined(i + 3) = p
    }
    transferTexture.uploadFloat4(combined)
  }

  def updateHeatCapacityTexture(grid: Grid[Float]): Unit = {
    assert(grid.width == width && grid.height == height)
    val combined = new Array[Float](width * height * 4)
    for ((x, y) <- grid.indices) {
      val i = (y * width + x) * 4
      combined(i + 0) = grid(x, y)
      combined(i + 1) = 1
      combined(i + 2) = 1
      combined(i + 3) = 1
    }
    heatCapacityTexture.uploadFloat4(combined)
  }

  def step(atmosphere: Array[Float], cylindrical: Boolean): Unit = {
    assert(atmosphere.length == this.width * this.height * 4)
    // 1. copy from output to input
    //    (also: update map if necessary)
    // 2. run sim
    // 3. read

    // We have to disable blending otherwise the output values are scaled by their alpha values,
    // but the 4th channel has no special meaning for us here.
    val wasBlendingEnabled = glIsEnabled(GL_BLEND)
    glDisable(GL_BLEND)

    timed("  first glFinish") { glFinish() }
    val start = System.nanoTime()
    val source = Texture.fromFloat4Array(width, height, atmosphere)
    val sourceFb = new Framebuffer(source)
    source.wrapS = if (cylindrical) Texture.Repeat else Texture.ClampToEdge
    glCheckError()
    glFinish()
    println(f"  uploading took ${(System.nanoTime() - start) / 1e6}%.2f ms")
    val target = new Framebuffer(Texture.emptyFloat4(width, height))
    diffusionPass.renderToFramebuffer(
      Map("temperature" -> source, "heatTransfer" -> transferTexture, "heatCapacity" -> heatCapacityTexture),
      target
    )
    diffusionPass.renderToFramebuffer(
      Map("temperature" -> target.texture, "heatTransfer" -> transferTexture, "heatCapacity" -> heatCapacityTexture),
      sourceFb
    )
    diffusionPass.renderToFramebuffer(
      Map("temperature" -> source, "heatTransfer" -> transferTexture, "heatCapacity" -> heatCapacityTexture),
      target
    )
    timed("  glFinish") { glFinish() }
    val buf = timed("  copy back") { target.texture.readFloat4() }
    timed("  copy") { buf.get(atmosphere) }
    source.dispose()
    target.dispose()
    glCheckError()
    if (wasBlendingEnabled)
      glEnable(GL_BLEND)
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
    va.flip()

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

    if (org.lwjgl.glfw.GLFW.glfwExtensionSupported("GL_ARB_debug_output")) {
      println("Debug output supported")
      import org.lwjgl.opengl.GL43.*
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
    val tex1 = Texture.fromFloatArray(fbw, fbh, Array.tabulate(fbw, fbh)((i, j) => if (i < fbw/2) 270f else (250f + Random.nextFloat() * 10f)).flatten)
    val fb1 = new Framebuffer(tex1)
    val tex2 = Texture.emptyFloat(fbw, fbh)
    val fb2 = new Framebuffer(tex2)

    var fbs = (fb1, fb2)

    val ht = Texture.fromFloatArray(fbw, fbh, Array.tabulate(fbw, fbh)((i, j) => if (i == fbw/2 && j > fbh/4 && j < fbh*3/4) 0.1f else 1f).flatten)
    val hc = Texture.fromFloatArray(fbw, fbh, Array.tabulate(fbw, fbh)((i, j) => if (j < fbw/4) 1f else 10f).flatten)

    def step(): Unit = {
      pass.renderToFramebuffer(Map("temperature" -> fbs._1.texture, "heatTransfer" -> ht, "heatCapacity" -> hc), fbs._2)
      fbs = fbs.swap
    }

    def draw(): Unit = {
      toScreenPass.renderToScreen(fbs._2.texture, fbw, fbh)
    }

    win.show()

    while (!win.shouldClose) {
      for (_ <- 1 to 4) step()
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
