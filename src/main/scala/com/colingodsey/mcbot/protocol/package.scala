package com.colingodsey.mcbot

import java.io.{ByteArrayInputStream, DataOutputStream, DataInputStream}

package object protocol extends Fields with CodecFrom {
	case class VarInt(x: Int) extends AnyVal
	implicit class BigInt128(val x: BigInt) extends AnyVal

	case class Slot(id: Short, count: Byte, damage: Short, nbt: Seq[Byte])

	object ByteSlotCodec extends FieldCodec[Slot] {
		def read(src: DataSource): Slot = {
			val id = src.stream.readShort
			if(id != -1) {
				val count = src.stream.readByte
				val damage = src.stream.readShort
				val len = src.stream.readByte
				val nbt = if(len == -1) Nil
				else src.read(len).toSeq

				Slot(id, count, damage, nbt)
			} else Slot(-1, 0, 0, Nil)
		}

		def write(obj: Slot, dest: DataDest) {
			dest.stream.writeShort(obj.id)
			if(obj.id != -1) {
				dest.stream.writeByte(obj.count)
				dest.stream.writeShort(obj.damage)
				//this either needs to be a short or byte... byte for metadata
				dest.stream.writeByte(if(obj.nbt.isEmpty) -1 else obj.nbt.length)
				dest.stream.write(obj.nbt.toArray)
			}
		}
	}

	object ShortSlotCodec extends FieldCodec[Slot] {
		def read(src: DataSource): Slot = {
			val id = src.stream.readShort
			if(id != -1) {
				val count = src.stream.readByte
				val damage = src.stream.readShort
				val len = src.stream.readShort
				val nbt = if(len == -1) Nil
				else src.read(len).toSeq

				Slot(id, count, damage, nbt)
			} else Slot(-1, 0, 0, Nil)
		}

		def write(obj: Slot, dest: DataDest) {
			dest.stream.writeShort(obj.id)
			if(obj.id != -1) {
				dest.stream.writeByte(obj.count)
				dest.stream.writeShort(obj.damage)
				//this either needs to be a short or byte... byte for metadata
				dest.stream.writeShort(if(obj.nbt.isEmpty) -1 else obj.nbt.length)
				dest.stream.write(obj.nbt.toArray)
			}
		}
	}

	val ClientProtocol = com.colingodsey.mcbot.protocol.client.Protocol
	val ServerProtocol = server.Protocol

	object status {
		implicit object Ping extends PacketCompanion[Ping] {
			val codec = codecFrom1(Ping.apply)
			val packetID = 1.toByte
		}
		case class Ping(time: Long) extends Ping.Packet

		object server extends Protocol {
			implicit object RequestComp extends LocalPacketCompanion[Request.type](0) {
				val codec = codecFrom0(Request)
			}
			case object Request extends Ping.Packet

			val packets = Set[PacketCompanion[_]](RequestComp, Ping)
		}

		object client extends Protocol {
			implicit object Response extends LocalPacketCompanion[Response](0) {
				val codec = codecFrom1(Response.apply)
			}
			case class Response(jsonString: String) extends Response.Packet

			val packets = Set[PacketCompanion[_]](Response, Ping)
		}
	}

	object handshake {
		object server extends Protocol {
			implicit object Handshake extends LocalPacketCompanion[Handshake](0) {
				val codec = codecFrom4(Handshake.apply)

				def login(host: String, port: Int) =
					Handshake(VarInt(4), host, port.toShort, VarInt(2))
			}
			case class Handshake(protocolVersion: VarInt, serverAddress: String, serverPort: Short,
					nextstate: VarInt) extends Handshake.Packet

			val packets = Set[PacketCompanion[_]](Handshake)
		}
	}
}
