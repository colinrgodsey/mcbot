package com.colingodsey.mcbot.protocol

import scala.util.Try
import com.colingodsey.mcbot.protocol

trait Protocol {
	abstract class LocalPacketCompanion[T <: Packet](val packetID: Byte)
			extends PacketCompanion[T]

	def packets: Set[PacketCompanion[_]]

	lazy val packetMap = packets.map(x => x.packetID ->
			x.asInstanceOf[PacketCompanion[Packet]]).toMap

	def protocolForId(id: Byte): PacketCompanion[Packet] = {
		//println(packets)
		packetMap(id)
	}

	def readPacket(src: DataSource): Packet = {
		val id = src.read[VarInt].x.toByte
		val proto = protocolForId(id)

		proto.codec.read(src)
	}

	def peakLength(src: DataSource) =
		Try((src.read[VarInt].x, src.position))

}