package com.colingodsey.mcbot.protocol

import scala.annotation.tailrec
import scala.collection.immutable.VectorBuilder

trait Fields {
	type UByte = Byte

	trait LengthCodec[T] {
		def readLength(src: DataSource): Int
		def writeLength(l: Int, dest: DataDest)
	}

	def readFields[T](src: DataSource, n: Int)(implicit codec: FieldCodec[T]): Seq[T] =
		Vector.fill(n)(src.read[T])

	implicit def seqCodec[T](implicit codec: FieldCodec[T],
			lcodec: LengthCodec[_]): FieldCodec[Seq[T]] = new FieldCodec[Seq[T]] {
		def read(src: DataSource): Seq[T] = {
			val oldPos = src.position
			val len = lcodec.readLength(src)
			//println("reading len " + len)
			readFields(src, len)(codec)
		}

		def write(obj: Seq[T], dest: DataDest): Unit = {
			lcodec.writeLength(obj.length, dest)
			obj.foreach(dest write _)
		}
	}

	implicit def mapCodec[T, U](implicit codec: FieldCodec[T],
			codec2: FieldCodec[U],
			lcodec: LengthCodec[_]): FieldCodec[Map[T, U]] = new FieldCodec[Map[T, U]] {
		def read(src: DataSource): Map[T, U] = {

			val len = lcodec.readLength(src)

			Vector.fill(len) {
				src.read[T] -> src.read[U]
			}.toMap
		}

		def write(obj: Map[T, U], dest: DataDest): Unit = {
			lcodec.writeLength(obj.size, dest)
			obj foreach { case (k, v) =>
				dest write k
				dest write v
			}
		}
	}

	implicit object VarIntCodec extends FieldCodec[VarInt] {
		def read(src: DataSource): VarInt =
			VarInt(src.readRawVarint32(ByteCodec))

		def write(obj: VarInt, dest: DataDest): Unit =
			dest.writeRawVarint32(obj.x)

	}

	implicit object StringCodec extends FieldCodec[String] {
		def read(src: DataSource): String = {
			val startPos = src.position
			val len = src.read[VarInt].x

			val bytes = src.read(len).toArray

			new String(bytes, "UTF8")
		}
		def write(obj: String, dest: DataDest) {
			dest.write[VarInt](VarInt(obj.length))
			dest.stream.write(obj.getBytes("UTF8"))
		}
	}

	implicit object BooleanCodec extends FieldCodec[Boolean] {
		def read(src: DataSource): Boolean =
			src.stream.readBoolean
		def write(obj: Boolean, dest: DataDest): Unit =
			dest.stream.writeBoolean(obj)
	}

	implicit object ByteCodec extends FieldCodec[Byte] {
		def read(src: DataSource): Byte =
			src.stream.readByte
		def write(obj: Byte, dest: DataDest): Unit =
			dest.stream.writeByte(obj)
	}

	implicit object ShortCodec extends FieldCodec[Short] {
		def read(src: DataSource): Short =
			src.stream.readShort
		def write(obj: Short, dest: DataDest): Unit =
			dest.stream.writeShort(obj)
	}

	implicit object IntCodec extends FieldCodec[Int] {
		def read(src: DataSource): Int =
			src.stream.readInt
		def write(obj: Int, dest: DataDest): Unit =
			dest.stream.writeInt(obj)
	}

	implicit object LongCodec extends FieldCodec[Long] {
		def read(src: DataSource): Long =
			src.stream.readLong
		def write(obj: Long, dest: DataDest): Unit =
			dest.stream.writeLong(obj)
	}

	//big int!
	implicit object BigInt128Codec extends FieldCodec[BigInt128] {
		def read(src: DataSource): BigInt128 =
			BigInt(src.read(16).toArray)

		def write(obj: BigInt128, dest: DataDest): Unit = {
			val bytes = obj.x.toByteArray
			require(bytes.length == 16)
			dest.write(bytes.iterator)
		}
	}

	implicit object FloatCodec extends FieldCodec[Float] {
		def read(src: DataSource): Float =
			src.stream.readFloat
		def write(obj: Float, dest: DataDest): Unit =
			dest.stream.writeFloat(obj)
	}

	implicit object DoubleCodec extends FieldCodec[Double] {
		def read(src: DataSource): Double =
			src.stream.readDouble
		def write(obj: Double, dest: DataDest): Unit =
			dest.stream.writeDouble(obj)
	}

	object LengthCodec {
		implicit case object ByteLengthCodec extends LengthCodec[Byte] {
			def readLength(src: DataSource): Int = src.read[Byte]
			def writeLength(l: Int, dest: DataDest) = dest.write(l.toByte)
		}

		implicit case object ShortLengthCodec extends LengthCodec[Short] {
			def readLength(src: DataSource): Int = src.read[Short]
			def writeLength(l: Int, dest: DataDest) = dest.write(l.toShort)
		}

		implicit case object VarIntLengthCodec extends LengthCodec[VarInt] {
			def readLength(src: DataSource): Int = src.read[VarInt].x
			def writeLength(l: Int, dest: DataDest) = dest.write(VarInt(l))
		}

		implicit case object IntLengthCodec extends LengthCodec[Int] {
			def readLength(src: DataSource): Int = src.read[Int]
			def writeLength(l: Int, dest: DataDest) = dest.write(l.toInt)
		}

		def apply[T](read: DataSource => Int, write: (Int, DataDest) => Unit): LengthCodec[T] = new LengthCodec[T] {
			def readLength(src: DataSource): Int = read(src)
			def writeLength(l: Int, dest: DataDest) = write(l, dest)
		}

		def apply[T](implicit lc: LengthCodec[T]): LengthCodec[T] = lc
	}
}
