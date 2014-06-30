package com.colingodsey.mcbot.client

import scala.concurrent.duration._
import akka.actor.{Actor, ActorSystem}
import akka.util.Timeout
import com.colingodsey.mcbot.network.ProtocolStream
import java.io.{FileInputStream, File}
import com.colingodsey.mcbot.protocol.DataSource

object Main extends App {
	val host = "192.168.0.2"
	val port = 25565

	val waypointFile = new File("funnybot1.wp.dat")

	/*val wps = if(!waypointFile.canRead) Nil
	else {
		val src = DataSource(new FileInputStream(waypointFile))

		try src.read[WaypointSnapshot](
			WaypointSnapshot.codec).waypoints finally src.close
	}*/


	val botSettings = BotClient.Settings(host, port, "funnybot1", None)

	// we need an ActorSystem to host our application in
	implicit val system = ActorSystem("MCBotClient")
	implicit val ec = system.dispatcher

	val bot = system.actorOf(BotClient.props(botSettings), name = "bot-client")

	val otherBots = for(i <- 2 to 3) yield {
		system.scheduler.scheduleOnce((i * 16).seconds) {
			val ob = system.actorOf(BotClient.props(botSettings.copy(
				username = "funnybot" + i, wpMasterRef = Some(bot))),
				name = "bot-client" + i)
			bot.tell(BotClient.Subscribe, ob)
			ob
		}
	}

	implicit val timeout = Timeout(5.seconds)

	// start a new HTTP server on port 8080 with our service actor as the handler
	//IO(Http) ? Http.Bind(service, interface = "0.0.0.0", port = 8099)

	sys addShutdownHook {
		system.shutdown()
		system.awaitTermination()
	}
}
