package adrift.display.glutil

import java.nio.{ByteBuffer, FloatBuffer}

import org.lwjgl.BufferUtils
import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.GL12._
import org.lwjgl.opengl.GL30._

trait TextureSource {
  def width: Int
  def height: Int
  def upload(): Unit
}

class ImageTextureSource(private var image: Image) extends TextureSource {
  override val width: Int = image.width
  override val height: Int = image.height

  override def upload(): Unit = {
    assert(image != null)
    val format = image.channels match {
      case 1 => GL_RED
      case 2 => GL_RG
      case 3 => GL_RGB
      case 4 => GL_RGBA
      case _ => throw new NotImplementedError(s"Don't know how to handle images with ${image.channels} channels")
    }
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, width, height, 0, format, GL_UNSIGNED_BYTE, image.bytes)
    image = null
  }
}
class EmptyTextureSource(val width: Int, val height: Int) extends TextureSource {
  override def upload(): Unit = {
    nglTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, width, height, 0, GL_RGBA, GL_UNSIGNED_BYTE, 0)
  }
}
class FloatTextureSource(val width: Int, val height: Int, private var buf: Array[Float]) extends TextureSource {
  override def upload(): Unit = {
    assert(width * height == buf.length)
    glTexImage2D(GL_TEXTURE_2D, 0, GL_R32F, width, height, 0, GL_RED, GL_FLOAT, buf)
    buf = null
  }
}
class EmptyFloatTextureSource(val width: Int, val height: Int) extends TextureSource {
  override def upload(): Unit = {
    nglTexImage2D(GL_TEXTURE_2D, 0, GL_R32F, width, height, 0, GL_RED, GL_FLOAT, 0)
  }
}
class Float4TextureSource(val width: Int, val height: Int, private var buf: Array[Float]) extends TextureSource {
  override def upload(): Unit = {
    assert(width * height * 4 == buf.length)
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA32F, width, height, 0, GL_RGBA, GL_FLOAT, buf)
    buf = null
  }
}
class EmptyFloat4TextureSource(val width: Int, val height: Int) extends TextureSource {
  override def upload(): Unit = {
    nglTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA32F, width, height, 0, GL_RGBA, GL_FLOAT, 0)
  }
}

class Texture(textureSource: TextureSource) {
  val width: Int = textureSource.width
  val height: Int = textureSource.height

  // Lazily upload the texture so that textures can be created before GL is initialized.
  lazy val id: Int = {
    val id = glGenTextures()
    val previouslyBound = glGetInteger(GL_TEXTURE_BINDING_2D)
    glBindTexture(GL_TEXTURE_2D, id)
    glPixelStorei(GL_UNPACK_ALIGNMENT, 1)
    glPixelStorei(GL_PACK_ALIGNMENT, 1)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE)
    glCheckError()
    textureSource.upload()
    glCheckError()
    glBindTexture(GL_TEXTURE_2D, previouslyBound)
    id
  }

  def bind(): Unit = glBindTexture(GL_TEXTURE_2D, id)

  def read(): ByteBuffer = {
    val buf = BufferUtils.createByteBuffer(width * height * 4)
    bind()
    glGetTexImage(GL_TEXTURE_2D, 0, GL_RGBA, GL_UNSIGNED_BYTE, buf)
    buf
  }

  def readFloat(): FloatBuffer = {
    val buf = BufferUtils.createFloatBuffer(width * height)
    bind()
    glGetTexImage(GL_TEXTURE_2D, 0, GL_RED, GL_FLOAT, buf)
    glCheckError()
    buf
  }

  def readFloat4(): FloatBuffer = {
    val buf = BufferUtils.createFloatBuffer(width * height * 4)
    bind()
    glGetTexImage(GL_TEXTURE_2D, 0, GL_RGBA, GL_FLOAT, buf)
    glCheckError()
    buf
  }

  def dispose(): Unit = glDeleteTextures(id)

  def uploadFloat(fb: Array[Float]): Unit = {
    bind()
    new FloatTextureSource(width, height, fb).upload()
    glCheckError()
  }

  def uploadFloat4(fb: Array[Float]): Unit = {
    bind()
    new Float4TextureSource(width, height, fb).upload()
    glCheckError()
  }
}

object Texture {
  def fromFloatArray(width: Int, height: Int, buf: Array[Float]) =
    new Texture(new FloatTextureSource(width, height, buf))
  def emptyFloat(width: Int, height: Int) = new Texture(new EmptyFloatTextureSource(width, height))
  def fromFloat4Array(width: Int, height: Int, buf: Array[Float]) =
    new Texture(new Float4TextureSource(width, height, buf))
  def emptyFloat4(width: Int, height: Int) = new Texture(new EmptyFloat4TextureSource(width, height))

  def fromByteArray(width: Int, height: Int, buf: Array[Byte]) = new Texture(new ImageTextureSource(Image.fromBytes(width, height, 1, buf)))

  def fromImage(image: Image): Texture = new Texture(new ImageTextureSource(image))
  def empty(width: Int, height: Int) = new Texture(new EmptyTextureSource(width, height))
}
