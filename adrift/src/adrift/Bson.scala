package adrift

import java.io._
import java.util.zip.{GZIPInputStream, GZIPOutputStream}

import io.circe._

import scala.collection.mutable

/**
  * Binary encoding of JSON values, gzipped.
  *
  * BSON files start with eight bytes: "BSON", followed by a four-byte version number.
  *
  * The first byte of a value indicates the type of value. See |Bson.Code| for details.
  */
object Bson {
  object Code {
    /** Represents 'null'. No extra data. */
    val Null = 0
    /** Represents 'true'. No extra data. */
    val True = 1
    /** Represents 'false'. No extra data. */
    val False = 2
    /** Represents an integer number. The following four bytes are a little-endian integer. */
    val Int = 3
    /** Represents an integer number. The following eight bytes are a little-endian long. */
    val Long = 4
    /** Represents a floating-point number. The following four bytes are an IEEE 754 float. */
    val Float = 5
    /** Represents a floating-point number. The following eight bytes are an IEEE 754 double. */
    val Double = 6
    /** Represents a string. The following two bytes are the length of the string in bytes, followed by UTF-8 encoded string. */
    val NewString = 7
    /** Represents a string. The following four bytes are a little-endian integer representing an index into the string table. */
    val StringRef = 8
    /** Represents an array. The following four bytes are a little-endian integer representing the length of the array. Then, decode that many JSON values using the same procedure. */
    val Array = 9
    /** Represents an object. The following four bytes are a little-endian integer representing the number of key/value pairs in the object. Following that are that many pairs of JSON values, the even ones of which are guaranteed to be strings (NewString or StringRef). */
    val Object = 10
  }
  class Folder(_os: OutputStream) extends Json.Folder[Unit] {
    import Code._
    val os = new DataOutputStream(_os)
    val stringTable = mutable.Map.empty[String, Int]

    override def onNull: Unit = os.write(Null)

    override def onBoolean(value: Boolean): Unit = if (value) os.write(True) else os.write(False)

    override def onNumber(value: JsonNumber): Unit = {
      value.toInt match {
        case Some(i) =>
          os.write(Int)
          os.writeInt(i)
        case None =>
          value.toLong match {
            case Some(l) =>
              os.write(Long)
              os.writeLong(l)
            case None =>
              os.write(Double)
              os.writeDouble(value.toDouble)
          }
      }
    }

    override def onString(value: String): Unit = {
      stringTable.get(value) match {
        case None =>
          os.write(NewString)
          os.writeUTF(value)
          stringTable.put(value, stringTable.size)
        case Some(i) =>
          os.write(StringRef)
          os.writeInt(i)
      }
    }

    override def onArray(value: Vector[Json]): Unit = {
      os.write(Array)
      os.writeInt(value.size)
      value.foreach(_.foldWith(this))
    }

    override def onObject(value: JsonObject): Unit = {
      os.write(Object)
      os.writeInt(value.size)
      value.toIterable.foreach {
        case (k, v) =>
          onString(k)
          v.foldWith(this)
      }
    }
  }

  def encode(j: Json, out: OutputStream): Unit = {
    out.write("BSON".getBytes)
    new DataOutputStream(out).writeInt(0)
    val os = new BufferedOutputStream(new GZIPOutputStream(out, true))
    j.foldWith(new Folder(os))
    os.flush()
  }

  def decode(in: InputStream): Json = {
    import Code._

    val magic = new Array[Byte](4)
    in.read(magic)
    if (new String(magic) != "BSON")
      throw new RuntimeException("Not a BSON file")

    val version = new DataInputStream(in).readInt()
    if (version != 0)
      throw new RuntimeException(s"Unknown version: $version")

    val is = new DataInputStream(new BufferedInputStream(new GZIPInputStream(in)))
    val stringTable = mutable.Buffer.empty[String]

    def nextValue(): Json = {
      is.readByte() match {
        case Null => Json.Null
        case True => Json.True
        case False => Json.False
        case Int => Json.fromInt(is.readInt())
        case Long => Json.fromLong(is.readLong())
        case Float => Json.fromFloatOrString(is.readFloat())
        case Double => Json.fromDoubleOrString(is.readDouble())
        case NewString =>
          val str = is.readUTF()
          stringTable.append(str)
          Json.fromString(str)
        case StringRef =>
          val i = is.readInt()
          Json.fromString(stringTable(i))
        case Array =>
          val size = is.readInt()
          Json.fromValues((1 to size).map(_ => nextValue()))
        case Object =>
          val size = is.readInt()
          Json.fromFields((1 to size).map(_ => {
            val k = is.readByte() match {
              case NewString =>
                val str = is.readUTF()
                stringTable.append(str)
                str
              case StringRef =>
                val i = is.readInt()
                stringTable(i)
              case id => throw new RuntimeException(s"Only strings allowed, got $id")
            }
            val v = nextValue()
            (k, v)
          }))
        case id => throw new RuntimeException(s"Bad value type: $id")
      }
    }
    nextValue()
  }
}
