package com.colingodsey.mcbot.protocol

import java.io.{ByteArrayOutputStream, DataOutputStream, ByteArrayInputStream, DataInputStream}
import com.google.protobuf.{CodedOutputStream, CodedInputStream}
import com.colingodsey.mcbot.protocol
import com.colingodsey.logos.collections._

object DataSource {
	def apply(seq: Seq[Byte]): DataSource = new DataSource {
		//TODO:  use our own Seq-based input stream
		val is = new SeqInputStream(seq)

		def position = {
			//println("xxx", seq.length, is.available, is.position)
			//seq.length - is.available
			is.position
		}

		val stream: DataInputStream = new DataInputStream(is)
	}
}

trait DataSource {
	//mke sure its big-endian
	def stream: DataInputStream
	def position: Int

	def read[T](implicit codec: FieldCodec[T]): T = codec.read(this)

	def read(bytes: Int): IndexedSeq[Byte] =
		Vector.fill(bytes)(stream.readByte)

	def readUnsignedByte: Int =
		stream.read()

	def readRawVarint32(implicit codec: FieldCodec[Byte]): Int = {
		var tmp = read[Byte].toInt
		if (tmp >= 0) {
			return tmp
		}
		var result = tmp & 0x7f
		tmp = read[Byte]
		if (tmp >= 0) {
			result |= tmp << 7
		} else {
			result |= (tmp & 0x7f) << 7
			tmp = read[Byte]
			if (tmp >= 0) {
				result |= tmp << 14
			} else {
				result |= (tmp & 0x7f) << 14
				tmp = read[Byte]
				if (tmp >= 0) {
					result |= tmp << 21
				} else {
					result |= (tmp & 0x7f) << 21
					tmp = read[Byte]
					result |= tmp << 28
					if (tmp < 0) {
						// Discard upper 32 bits.
						//for (int i = 0; i < 5; i++) {
						for(_ <- 0 until 5) {
							if (read[Byte] >= 0) {
								return result
							}
						}
						//throw InvalidProtocolBufferException.malformedVarint();
						sys.error("Bad varint!")
					}
				}
			}
		}

		result
	}

	//def protoBufStream: CodedInputStream = CodedInputStream.newInstance(stream)

	def close = stream.close
}

trait DataDest {
	def stream: DataOutputStream

	def write[T](x: T)(implicit codec: FieldCodec[T]) =
		codec.write(x, this)

	def write(bytes: TraversableOnce[Byte]) = bytes.foreach(write[Byte](_)(protocol.ByteCodec))

	def writeUnsignedByte(byte: Int) =
		stream.write(byte)

	def writeRawVarint32(value: Long) {
		if((value & ~0x7F) == 0)
			writeUnsignedByte(value.toInt)
		else {
			val x = ((value.toInt & 0x7F) | 0x80)
			writeUnsignedByte(x.toInt)
			writeRawVarint32(value >>> 7)
		}
	}
}

class BufferedDataDest extends DataDest {
	val os = new ByteArrayOutputStream
	val stream = new DataOutputStream(os)

	def getBytes = os.toByteArray.toIndexedSeq
}



/*trait PacketCodec[T] extends FieldCodec[T] {
	def packetID: Byte
}*/

object FieldCodec {
	def apply[T](implicit codec: FieldCodec[T]) = codec
}

trait FieldCodec[T] {
	def read(src: DataSource): T
	def write(obj: T, dest: DataDest): Unit
}

trait PacketCodec[T] extends FieldCodec[T] {
	def read(src: DataSource): T
	def write(obj: T, dest: DataDest): Unit
}

//abstract class ProtocolCompanion[T <: Protocol](val packetID: Byte) {
trait PacketCompanion[T <: Packet] { self =>
	def codec: FieldCodec[T]
	def packetID: Byte

	private[protocol] abstract class Packet/*(val packetID: Byte)*/ extends protocol.Packet {
		def comp = self.asInstanceOf[protocol.PacketCompanion[this.type]]
	}
}

trait Packet extends Product {
	def comp: PacketCompanion[this.type]
	//def comp(implicit c: ProtocolCompanion[this.type]) = c
	//def packetID: Byte = comp.packetID

	def write(dest: DataDest) = comp.codec.write(this, dest)
}