package adrift.display.glutil

import java.nio.ByteBuffer

import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.GL12._

trait TextureSource {
  def width: Int
  def height: Int
  def channels: Int
  def pixels: ByteBuffer
  def release(): Unit
}

class ImageTextureSource(private var image: Image) extends TextureSource {
  override val width: Int = image.width
  override val height: Int = image.height
  override val channels: Int = image.channels
  override def pixels: ByteBuffer = image.bytes
  override def release(): Unit = image = null
}

class Texture(textureSource: TextureSource) {
  val width: Int = textureSource.width
  val height: Int = textureSource.width

  // Lazily upload the texture so that textures can be created before GL is initialized.
  private lazy val id = {
    glEnable(GL_TEXTURE_2D)
    val id = glGenTextures()
    glBindTexture(GL_TEXTURE_2D, id)
    glPixelStorei(GL_UNPACK_ALIGNMENT, 1)
    glPixelStorei(GL_PACK_ALIGNMENT, 1)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE)
    val format = textureSource.channels match {
      case 3 => GL_RGB
      case 4 => GL_RGBA
      case _ => throw new NotImplementedError(s"Don't know how to handle images with ${textureSource.channels} channels")
    }
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, textureSource.width, textureSource.height, 0, format, GL_UNSIGNED_BYTE, textureSource.pixels)
    textureSource.release()
    id
  }

  def bind(): Unit = glBindTexture(GL_TEXTURE_2D, id)
}

object Texture {
  def fromImage(image: Image): Texture = new Texture(new ImageTextureSource(image))
}
