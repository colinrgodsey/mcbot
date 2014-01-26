package com.colingodsey.mcbot.panel

import scala.concurrent.duration._
import akka.actor._
import akka.util.Timeout
import com.colingodsey.mcbot.network.ProtocolStream
import javafx.application.{Platform, Application}
import javafx.stage.Stage
import javafx.scene.layout.StackPane
import javafx.scene._
import javafx.scene.paint.Color
import com.typesafe.config.{Config, ConfigFactory}
import scala.concurrent.ExecutionContext
import akka.event.{BusLogging, Logging, LogSource, LoggingAdapter}
import akka.dispatch.{ExecutorServiceFactory, ExecutorServiceConfigurator, DispatcherPrerequisites}
import java.util.concurrent.{AbstractExecutorService, ExecutorService, ThreadFactory}
import java.util
import com.colingodsey.mcbot.client.WaypointManager.Waypoint
import javafx.scene.shape.Circle

class Main extends Application {
	val defCfg = ConfigFactory.load(getClass.getClassLoader)
	val panelConfig = defCfg.getConfig("mcbot.panel").withFallback(defCfg)

	implicit val system = ActorSystem("MCBotPanel", panelConfig)

	override def start(primaryStage: Stage) {
		val uiProps = Props(classOf[UIStageActor], primaryStage).withDispatcher("jfx-dispatcher")
		val ui = system.actorOf(uiProps, "ui")

		ui ! UIStageActor.Show
	}
}

object Main extends App {
	Application.launch(classOf[Main], args: _*)
}

object UIStageActor {
	case object Show
	case class AddWaypoint(wp: Waypoint)
	case class DelWaypoint(wpId: Int)
}

class UIStageActor(primaryStage: Stage) extends Actor with ActorLogging {
	import UIStageActor._

	val root = new StackPane
	val scene = new Scene(root, 800, 600, Color.BLACK)

	var wpNodes = Map[Int, Node]()

	//root.getChildren().add(btn)
	primaryStage setScene scene

	log.info("UI starting...")

	def removeWaypoint(wp: Waypoint) = wpNodes.get(wp.id) match {
		case Some(x) =>
			root.getChildren remove x
			wpNodes -= wp.id
			true
		case None => false
	}

	def addWaypoint(wp: Waypoint) {
		removeWaypoint(wp)

		val node = new Circle()

		node.setFill(Color.WHITE)
		node.setRadius(wp.pos.y)

		root.getChildren add node
		wpNodes += wp.id -> node
	}

	def receive = {
		case Show =>
			log.info("UI started! Showing...")
			primaryStage.show()
		case AddWaypoint(wp) =>

	}
}