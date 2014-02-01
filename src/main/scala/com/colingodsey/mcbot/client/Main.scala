package com.colingodsey.mcbot.client

import scala.concurrent.duration._
import akka.actor.{Actor, ActorSystem}
import akka.util.Timeout
import com.colingodsey.mcbot.network.ProtocolStream

object Main extends App {
	val host = "192.168.0.2"
	val port = 25565

	val botSettings = BotClient.Settings(host, port, "funnybot1")

	java.lang.Math.random()

	// we need an ActorSystem to host our application in
	implicit val system = ActorSystem("MCBotClient")

	val bot = system.actorOf(BotClient.props(botSettings), name = "bot-client")

	val otherBots = for(i <- 2 until 6) yield
		system.actorOf(BotClient.props(botSettings.copy(username = "funnybot" + i)),
			name = "bot-client" + i)

	implicit val timeout = Timeout(5.seconds)

	// start a new HTTP server on port 8080 with our service actor as the handler
	//IO(Http) ? Http.Bind(service, interface = "0.0.0.0", port = 8099)

	//system.awaitTermination
	while(!system.isTerminated) {
		Thread.sleep(500)

	}
}
