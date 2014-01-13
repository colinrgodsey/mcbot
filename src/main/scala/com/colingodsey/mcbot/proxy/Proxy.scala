package com.colingodsey.mcbot.proxy

import akka.actor._
import com.colingodsey.mcbot.network._
import com.colingodsey.mcbot.protocol
import org.jboss.netty.channel.ChannelPipeline
import com.colingodsey.mcbot.protocol.Packet

object Proxy {
	case class InStream(ref: ActorRef)
}

class Proxy(pipeline: ChannelPipeline, host: String, port: Int)
		extends Actor with ActorLogging with Stash {

	val inStreamProps = Props(classOf[NettyServerProtocolStream],
		protocol.handshake.server, pipeline)

	val inStream = context.actorOf(inStreamProps, name = "client-stream")
	val outStream = context.actorOf(Props(classOf[NettyClientProtocolStream],
		protocol.login.client, host, port),
		name = "server-stream")

	inStream ! TCPConnection.Connected

	context watch inStream
	context watch outStream

	def waitingForConnection(n: Int): Receive = {
		case TCPConnection.Connected if n > 1 =>
			context.become(waitingForConnection(n - 1))
		case TCPConnection.Connected if n == 1 =>
			context.become(normal)
			unstashAll()
		case _ => stash()
	}

	def normal: Receive = {
		case packet: Packet if sender == inStream =>
			packet match {
				case x: protocol.ServerProtocol.Player =>
				case x: protocol.ServerProtocol.KeepAlive =>
				case _ => log.info("------C->S Forwarding " + packet)
			}
			outStream ! packet
		case ProtocolStream.UnparsedData(data) if sender == inStream =>
			log.info("C->S Forwarding bytes: " + data.length)
			outStream ! ProtocolStream.UnparsedData(data)

		case packet: Packet  if sender == outStream =>
			packet match {
				case x: protocol.ClientProtocol.EntityRelativeMove =>
				case x: protocol.ClientProtocol.EntityVelocity =>
				case x: protocol.ClientProtocol.EntityLook =>
				case x: protocol.ClientProtocol.EntityLookandRelativeMove =>
				case x: protocol.ClientProtocol.EntityHeadLook =>
				case x: protocol.ClientProtocol.TimeUpdate =>
				case x: protocol.ClientProtocol.DestroyEntities =>
				case x: protocol.ClientProtocol.ChangeGameState =>
				case x: protocol.ClientProtocol.MapChunkBulk =>
					//log.info("S->C Forwarding MapChunkBulk")
				case x: protocol.ClientProtocol.MapChunkMeta =>
				case x: protocol.ClientProtocol.ChunkData =>
					//log.info("S->C Forwarding ChunkData")
				case x: protocol.ClientProtocol.SpawnMob =>
				case x: protocol.ClientProtocol.KeepAlive =>
				case x: Product if x.productPrefix.startsWith("Entity") => ///hmmmmm
				case _ => log.info("S->C Forwarding " + packet)
			}
			if(packet.isInstanceOf[protocol.login.client.LoginSuccess])
				inStream ! protocol.ServerProtocol
			inStream ! packet
		case ProtocolStream.UnparsedData(data) if sender == outStream =>
			//log.info("S->C Forwarding bytes: " + data.length)
			inStream ! ProtocolStream.UnparsedData(data)
	}

	def receive = waitingForConnection(2)
}
