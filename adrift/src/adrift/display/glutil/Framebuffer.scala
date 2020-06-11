package adrift.display.glutil

import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.GL30._

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
