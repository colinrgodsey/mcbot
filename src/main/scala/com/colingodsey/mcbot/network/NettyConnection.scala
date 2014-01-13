package com.colingodsey.mcbot.network

import com.colingodsey.logos.collections._
import akka.actor._
import org.jboss.netty.bootstrap.{ServerBootstrap, ClientBootstrap}
import org.jboss.netty.channel.socket.nio.{NioServerSocketChannelFactory, NioClientSocketChannelFactory}
import org.jboss.netty.channel._
import java.net.InetSocketAddress
import org.jboss.netty.handler.codec.frame.FrameDecoder
import org.jboss.netty.buffer.{ChannelBuffers, ByteBufferBackedChannelBuffer, WrappedChannelBuffer, ChannelBuffer}
import java.nio.ByteBuffer
import akka.event.LoggingAdapter
import akka.actor.Actor.Receive
import java.util.concurrent.Executors
import org.jboss.netty.handler.ssl.SslHandler
import org.jboss.netty.handler.timeout.IdleStateHandler
import org.jboss.netty.handler.codec.http.{HttpContentDecompressor, HttpContentCompressor, HttpServerCodec}
import scala.util.Failure
import com.colingodsey.mcbot.protocol

class StreamHandler(self: ActorRef) extends FrameDecoder {
	import TCPConnection._

	def decode(ctx: ChannelHandlerContext, channel: Channel,
			buffer: ChannelBuffer): AnyRef = {

		val l = buffer.readableBytes()
		val dat = Vector.fill(l)(buffer.readByte)

		//self ! TCPConnection.RecvData(buffer)
		self ! TCPConnection.RecvData(dat)

		null
	}

	override def channelClosed(ctx: ChannelHandlerContext, event: ChannelStateEvent) {
		println("Channel closecd " + event)
		self ! ConnectionClosed
		super.channelClosed(ctx, event)
	}

	override def exceptionCaught(
			ctx: ChannelHandlerContext, e: ExceptionEvent) {
		//fail(e.getCause, "exceptionCaught")
		self ! Failure(e.getCause)
		super.exceptionCaught(ctx, e)
	}
}

trait NettyConnection {
	import TCPConnection._

	def log: LoggingAdapter
	def channel: Channel
	def senderRef: ActorRef
	def self: ActorRef
	def sender: ActorRef

	def close

	def dataReceive: Receive = {
		case RecvData(data) => senderRef ! Data(data)
		case Data(data) =>
			channel.write(data: ChannelBuffer)
			//channel.write(ChannelBuffers.wrappedBuffer(data.toArray))
			//log.info("Sent " + data.length + " bytes")
		case ConnectionClosed =>
			log.info("Received ConnectionClosed from " + sender)
			close
		case Failure(t) =>
			log.error(t, "NettyConnection Failure")
			close
	}
}

class NettyServerConnectionActor(val pipeline: ChannelPipeline)
		extends Actor with ActorLogging with NettyConnection {
	def channel = pipeline.getChannel

	def receive = dataReceive

	def senderRef = context.parent

	def close = {
		log.debug("Asked to close....")
		context stop self
	}

	override def postStop {
		super.postStop

		channel.close
	}
}

trait NettyServerActor
		extends Actor with ActorLogging {
	def port: Int
	//def receiverProps: Props
	//def newReceiver: ActorRef
	//def recvProtocol: protocol.Protocol
	def newReceiver(pipeline: ChannelPipeline): ActorRef

	lazy val (bootstrap, channel) = {
		//val workerPool = new JettyTools.ExecutorFromEC(systemEc)
		val workerPool = Executors.newCachedThreadPool

		//System.setProperty("org.jboss.netty.epollBugWorkaround", "true")

		val factory = new NioServerSocketChannelFactory(
			Executors.newCachedThreadPool, workerPool)

		val bs = new ServerBootstrap(factory)

		//bs.setOption("backlog", 10000)

		bs.setPipelineFactory(new ChannelPipelineFactory {
			def getPipeline = try {
				val pipeline = Channels.pipeline

				newReceiver(pipeline)

				Thread.sleep(1000)

				pipeline
			} catch { case x: Throwable =>
				log.error(x, "error creating pipeline!")
				throw x
			}
		})

		val ch = bs.bind(new InetSocketAddress("0.0.0.0", port))

		val serverConfig = ch.getConfig.asInstanceOf[DefaultServerChannelConfig]
		serverConfig.setConnectTimeoutMillis(8000)

		(bs, ch)
	}

	override def postStop {
		super.postStop

		channel.close
	}

	def receive = {
		case "BLAH" =>
	}
}

class NettyClientConnection(host: String, port: Int)
		extends Actor with ActorLogging with NettyConnection {
	import TCPConnection._

	def senderRef = context.parent

	object pipelineFactory extends ChannelPipelineFactory {
		override def getPipeline: ChannelPipeline = {
			val pipeline = Channels.pipeline()

			pipeline.addLast("streamer", new StreamHandler(self))

			pipeline
		}
	}

	val bootstrap = {
		val bs = new ClientBootstrap(
			new NioClientSocketChannelFactory)
		bs.setPipelineFactory(pipelineFactory)
		bs.setOption("child.keepAlive", true)
		bs
	}

	val conFuture = bootstrap.connect(
		new InetSocketAddress(host, port))
	val channel = conFuture.getChannel

	conFuture.addListener(new ChannelFutureListener {
		def operationComplete(future: ChannelFuture) {
			if(future.isSuccess) {
				log.info("Connected!")
				senderRef ! Connected
			} else self ! Failure(future.getCause)
		}
	})

	def close = context.stop(self)

	def receive = dataReceive

	override def postStop {
		super.postStop

		channel.close
	}
}

class NettyClientProtocolStream(proto: protocol.Protocol, host: String,
		port: Int) extends ProtocolStream {
	val networkConnector: ActorRef = context.actorOf(Props(classOf[NettyClientConnection],
		host, port), name = "netty-connector")

	var readProtocol = proto
}

class NettyServerProtocolStream(proto: protocol.Protocol,
		pipeline: ChannelPipeline) extends ProtocolStream {
	val networkConnector: ActorRef = context.actorOf(Props(
		classOf[NettyServerConnectionActor], pipeline), name = "netty-connector")

	var readProtocol = proto

	pipeline.addLast("streamer", new StreamHandler(networkConnector))
}