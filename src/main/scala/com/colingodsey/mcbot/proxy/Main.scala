package com.colingodsey.mcbot.proxy

import scala.concurrent.duration._
import akka.actor._
import akka.util.Timeout
import com.colingodsey.mcbot.network.{NettyServerProtocolStream, NettyServerActor, ProtocolStream}
import com.colingodsey.mcbot.client.BotClient
import org.jboss.netty.channel.ChannelPipeline
import com.colingodsey.mcbot.protocol

object Main extends App {
	// we need an ActorSystem to host our application in
	implicit val system = ActorSystem("MCProxy")

	system.scheduler.schedule(1 seconds, 1 minute)(System.gc)(system.dispatcher)

	implicit val timeout = Timeout(5.seconds)

	class ServerActor extends NettyServerActor {
		var clientId = 0

		val port = 25565
		def newReceiver(pipeline: ChannelPipeline): ActorRef = {
			val props = Props(classOf[Proxy], pipeline, "localhost", port)
			clientId += 1
			context.actorOf(props, name = "proxy-" + clientId)
		}

		override def preStart {
			super.preStart
			bootstrap
		}
	}

	val server = system.actorOf(Props[ServerActor])

	// start a new HTTP server on port 8080 with our service actor as the handler
	//IO(Http) ? Http.Bind(service, interface = "0.0.0.0", port = 8099)

	//system.awaitTermination
	while(!system.isTerminated) Thread.sleep(500)
}
