package adrift.display.glutil

import java.nio.ByteBuffer

import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.GL12._

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
