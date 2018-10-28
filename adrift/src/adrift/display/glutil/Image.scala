package adrift.display.glutil

import java.nio.ByteBuffer

import org.lwjgl.BufferUtils
import org.lwjgl.stb.STBImage._

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


