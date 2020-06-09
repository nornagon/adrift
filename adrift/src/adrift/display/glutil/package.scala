package adrift.display

import org.lwjgl.opengl.GL11._

package object glutil {
  def glCheckError(): Unit = {
    val errstr = glGetError() match {
      case GL_NO_ERROR => null
      case GL_INVALID_ENUM => "GL_INVALID_ENUM"
      case GL_INVALID_VALUE => "GL_INVALID_VALUE"
      case GL_INVALID_OPERATION => "GL_INVALID_OPERATION"
      case GL_STACK_OVERFLOW => "GL_STACK_OVERFLOW"
      case GL_STACK_UNDERFLOW => "GL_STACK_UNDERFLOW"
      case GL_OUT_OF_MEMORY => "GL_OUT_OF_MEMORY"
      case other => s"unknown ($other)"
    }
    if (errstr != null) throw new RuntimeException(s"GL Error: $errstr")
  }
}
