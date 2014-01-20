package com.colingodsey.mcbot.network

import akka.actor._
import org.jboss.netty.handler.codec.frame.FrameDecoder
import org.jboss.netty.channel._
import org.jboss.netty.buffer.ChannelBuffer
import com.colingodsey.mcbot.protocol._
import scala.util.{Failure, Success}
import java.util.concurrent.Executors
import org.jboss.netty.bootstrap.ClientBootstrap
import org.jboss.netty.channel.socket.nio.NioClientSocketChannelFactory
import com.colingodsey.mcbot.protocol.VarInt
import scala.util.Failure
import scala.util.Success
import org.jboss.netty.handler.timeout.IdleStateHandler
import java.net.InetSocketAddress
import com.colingodsey.logos.collections._
import com.mediamath.vad.collections.Cord

object ProtocolStream {
	case class UnparsedData(data: IndexedSeq[Byte])
}

trait ProtocolStream
		extends Actor with ActorLogging with Stash {
	import ProtocolStream._

	def networkConnector: ActorRef
	var readProtocol: Protocol

	val checkEncodingDecoding = true

	var buffer = Cord[Byte]()

	def src = DataSource(buffer)
	def ref = context.parent

	def handleData: Unit = readProtocol.peakLength(src) match {
		case Success((length, size)) if (length + size) <= buffer.length =>
			require(size > 0, "bad length!")

			val view = buffer.view

			val lengthBuf = view take size
			val (packetData, tail) = view.drop(size) splitAt length
			buffer = Cord(tail.toIndexedSeq)

			//println(length, size, packetData.length)
			log.ifdebug(s"Recved packet of length $length (size $size. Remaining: ${buffer.length}")

			require(packetData.length == length)
			require(!packetData.isEmpty, "packet data empty!!!")

			handlePacket(packetData, lengthBuf)

			if(buffer.length > 0) handleData
		case Success((length, size)) =>
			val need = (length + size) - buffer.length
			log.ifdebug(s"Waiting for $need bytes. " + (length, size))
		case Failure(t) =>
			log.error(t, "NonFatal - peakLength")
	}

	case class PacketException(id: Int, msg: String, cause: Throwable = null)
			extends Exception(msg, cause)

	def handlePacket(data: Seq[Byte], lengthBuf: Seq[Byte]): Unit = try {
		require(!data.isEmpty, "empty packet!!")

		//peak at id for logging reasons
		val id = DataSource(data).read[VarInt].x

		val src = DataSource(data)
		try {
			val packet = readProtocol.readPacket(src)

			//TODO: HACK!!! no other way of remotely switching protocols fast enough
			// so we do it manually here
			packet match {
				case _: login.client.LoginSuccess =>
					log.info("Hacking switching protocols...")
					readProtocol = com.colingodsey.mcbot.protocol.ClientProtocol
				case _: handshake.server.Handshake =>
					log.info("Hacking switching protocols2...")
					readProtocol = com.colingodsey.mcbot.protocol.login.server
				case _ =>
			}

			if(checkEncodingDecoding) {
				val dest = new BufferedDataDest

				//packet.comp
				dest.write(VarInt(packet.comp.packetID))
				packet.write(dest)

				val encd = dest.getBytes

				if(encd != data) {
					var repd = false
					val dif = data.zipWithIndex map { case (x, idx) =>
						if(x != encd(idx) && !repd) {
							repd = true
							log.warning(s"First byte (orig: $x, encd: ${encd(idx)} differs at idx $idx")
						}

						if(x != encd(idx)) {
							s"--($x != ${encd(idx)}})--"
						} else {
							x
						}
					}
					println(dif.mkString(","))
					require(false, s"error checking encoding for packet. origlen: ${data.length} encLength: ${encd.length} packet: $packet")
				}


			}

			if(src.position != data.length) {
				log.warning(s"Only read ${src.position} bytes of ${data.length} for $packet")
				ref ! ProtocolStream.UnparsedData(lengthBuf.toIndexedSeq ++ data)
			} else {
				ref ! packet
			}
		} catch {
			case t: Throwable =>
				log.error(t,
					s"Couldnt read packet id=$id, length=${data.length}, read=${src.position}")
				ref ! ProtocolStream.UnparsedData(lengthBuf.toIndexedSeq ++ data)
				//println(data.map(b => "0x" + BigInt(b & 0xFF).toString(16)).mkString("."))
		}
	} catch {
		case t: Throwable =>
			log.error(t, "Couldnt read packet id! Killing connection.")
			throw t
	}

	def handleRawData: Receive = {
		case TCPConnection.Data(dat) =>
			buffer ++= dat
			handleData
	}

	def waitingForCon: Receive = {
		case TCPConnection.Connected =>
			context.become(normal)
			log.info("Accepting packets")
			ref ! TCPConnection.Connected
			unstashAll()
		case _ => stash()
	}

	def normal: Receive = handleRawData orElse {
		case packet: Packet =>
			val id = packet.comp.packetID

			val dest = new BufferedDataDest
			dest.write(VarInt(id))
			packet.write(dest)

			val data = dest.getBytes
			val length = data.length

			val headerDest = new BufferedDataDest
			headerDest.write(VarInt(length))
			val header = headerDest.getBytes

			/*networkConnector ! TCPConnection.Data(header.toIndexedSeq)
			networkConnector ! TCPConnection.Data(data.toIndexedSeq)*/

			networkConnector ! TCPConnection.Data(header ++ data)
		case proto: Protocol =>
			readProtocol = proto
		case UnparsedData(data) => networkConnector ! TCPConnection.Data(data)
	}

	def receive = waitingForCon

	override def preStart {
		context.watch(networkConnector)
		super.preStart
	}
}
