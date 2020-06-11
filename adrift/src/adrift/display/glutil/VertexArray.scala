package adrift.display.glutil

import java.nio.FloatBuffer

import org.lwjgl.BufferUtils
import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.GL20._
import org.lwjgl.opengl.GL15._

case class VertexArray(buffer: FloatBuffer, attributes: Seq[VertexAttrib]) {
  val stride: Int = attributes.map(_.numComponents).sum * 4
//  val bufferId: Int = glGenBuffers()
  def put(f: Float): VertexArray = { buffer.put(f); this }
  def put(fs: Array[Float], offset: Int, length: Int): VertexArray = { buffer.put(fs, offset, length); this }
  def flip(): VertexArray = { buffer.flip(); this }
  def bind(): Unit = {
//    glBindBuffer(GL_ARRAY_BUFFER, bufferId)
    var offset = 0
    attributes foreach { a =>
      buffer.position(offset)
      glEnableVertexAttribArray(a.location)
      glVertexAttribPointer(a.location, a.numComponents, GL_FLOAT, false, stride, buffer)
      offset += a.numComponents
    }
    glCheckError()
//    glBindBuffer(GL_ARRAY_BUFFER, 0)
  }
  def draw(geom: Int, first: Int, count: Int): Unit = glDrawArrays(geom, first, count)
  def unbind(): Unit = attributes foreach { a => glDisableVertexAttribArray(a.location) }
}
object VertexArray {
  def create(count: Int, attributes: Seq[VertexAttrib]): VertexArray = {
    val stride = attributes.map(_.numComponents).sum
    val buffer = BufferUtils.createFloatBuffer(count * stride)
    VertexArray(buffer, attributes)
  }
}
