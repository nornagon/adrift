package adrift.display.glutil

import org.lwjgl.BufferUtils
import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.GL20._

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

