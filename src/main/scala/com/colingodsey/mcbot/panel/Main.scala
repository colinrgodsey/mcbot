package com.colingodsey.mcbot.panel

import scala.concurrent.duration._
import akka.actor._
import akka.util.Timeout
import com.colingodsey.mcbot.network.ProtocolStream
import javafx.application.{Platform, Application}
import javafx.stage.{Screen, WindowEvent, Stage}
import javafx.scene.layout.{VBoxBuilder, StackPane}
import javafx.scene._
import javafx.scene.paint.Color
import com.typesafe.config.{Config, ConfigFactory}
import scala.concurrent.ExecutionContext
import akka.event.{BusLogging, Logging, LogSource, LoggingAdapter}
import akka.dispatch.{ExecutorServiceFactory, ExecutorServiceConfigurator, DispatcherPrerequisites}
import java.util.concurrent.{AbstractExecutorService, ExecutorService, ThreadFactory}
import java.util
import com.colingodsey.mcbot.client.WaypointManager.Waypoint
import javafx.scene.shape.{Line, Circle}
import com.colingodsey.mcbot.client.BotClient.{BotPosition, BotSnapshot}
import com.colingodsey.mcbot.client.BotClient
import com.colingodsey.logos.collections.Vec3
import javafx.event.EventHandler
import javafx.geometry.Pos
import javafx.beans.value.{ObservableValue, ChangeListener}
import javax.swing.event.ChangeEvent

class Main extends Application {
	val defCfg = ConfigFactory.load(getClass.getClassLoader)
	val panelConfig = defCfg.getConfig("mcbot.panel").withFallback(defCfg)

	implicit val system = ActorSystem("MCBotPanel", panelConfig)

	val botSel = system.actorSelection(
		"akka.tcp://MCBotClient@192.168.0.102:3488/user/bot-client")

	override def start(primaryStage: Stage) {
		val uiProps = Props(classOf[UIStageActor],
			primaryStage, botSel).withDispatcher("jfx-dispatcher")
		val ui = system.actorOf(uiProps, "ui")

		ui ! UIStageActor.Show
	}

	override def stop {
		super.stop
		system.shutdown()
	}
}

object Main extends App {
	Application.launch(classOf[Main], args: _*)
}

object UIStageActor {
	case object Show
	case object SubTimer

	case class AddWaypoint(wp: Waypoint)
	case class DelWaypoint(wpId: Int)
}

class UIStageActor(stage: Stage, bot: ActorSelection) extends Actor with ActorLogging {
	import UIStageActor._

	import context.dispatcher

	val yScale = 1 / 20
	val latScale = 5.4

	val subscribeTimer = context.system.scheduler.schedule(
		1.seconds, 5.seconds, self, SubTimer)

	val root = new StackPane
	val scene = new Scene(root, 800, 600, Color.BLACK)
	val camera = new PerspectiveCamera
	val selfCircle = new Circle

	camera.setFieldOfView(160)//90)
	scene.setCamera(camera)

	var wpNodes = Map[Int, Group]()
	var waypoints = Map[Int, Waypoint]()
	var curPos = Vec3.zero

	selfCircle.setFill(Color.TRANSPARENT)
	selfCircle.setStroke(Color.WHITE)
	selfCircle.setRadius(2)
	selfCircle.setTranslateZ(-1)

	root.setScaleX(latScale)
	root.setScaleY(latScale)
	root.setDepthTest(DepthTest.ENABLE)
	root.setAlignment(Pos.CENTER)

	//stage.setFullScreen(true)

	stage.setOnCloseRequest(new EventHandler[WindowEvent] {
		def handle(event: WindowEvent) {
			context.system.shutdown
		}
	})

	stage setScene scene
	root.getChildren add selfCircle

	log.info("UI starting...")

	bot ! BotClient.Subscribe

	def removeWaypoint(wpId: Int) = wpNodes.get(wpId) match {
		case Some(x) =>
			root.getChildren remove x
			wpNodes -= wpId
			waypoints -= wpId
			true
		case None => false
	}

	def addConnections(wp: Waypoint) {
		val node = wpNodes(wp.id)

		//if(node.getChildren.size() > 1) return

		wp.connections.values.map { conn =>
			try {
				val other = waypoints(conn.destId)

				val vec = (other.pos - wp.pos).normal * 8

				val line = new Line

				val disc = math.max(0, conn.weight("discover"))

				val discoverFactor = math.min(disc / 20, 1)
				//println(disc, discoverFactor)
				line.setStroke(Color.color(1 - discoverFactor, discoverFactor, 0))

				line.setStartX(0)
				line.setStartY(0)
				line.setEndX(vec.x)
				line.setEndY(vec.z)

				line.setStrokeWidth(0.3)

				node.getChildren add line
				//root.getChildren add line
			} catch {
				case x: Throwable =>
			}
		}
	}

	def addWaypoint(wp: Waypoint) {
		removeWaypoint(wp.id)

		val node = new Group()
		val circle = new Circle()

		node.setLayoutX(0)
		node.setLayoutY(0)
		node.setAutoSizeChildren(false)

		node.getChildren add circle

		circle.setFill(Color.WHITE)

		val vec = wp.pos - curPos

		val circleSize = 2

		//if(math.abs(vec.y) < 4) {
			circle.setRadius(circleSize)
		//} else {
		//	val yFac = math.min(math.abs(vec.y) * yScale, 1)
		//	circle.setRadius((1 - yFac) * circleSize)
		//}

		circle.setFill(Color.WHITE)

		/*val disc = math.max(0, wp.connections.map(_._2.weight("discover")).max)

		val discoverFactor = math.min(disc / 70, 1)
		//println(disc, discoverFactor)
		circle.setFill(Color.color(1 - discoverFactor, discoverFactor, 0))*/

		node.setTranslateX(wp.pos.x)
		node.setTranslateY(wp.pos.z)
		node.setTranslateZ(-wp.pos.y)

		root.getChildren add node
		wpNodes += wp.id -> node
		waypoints += wp.id -> wp
	}

	override def postStop {
		super.postStop
		stage.close
	}

	scene.widthProperty addListener new ChangeListener[java.lang.Number] {
		def changed(observableValue: ObservableValue[_ <: java.lang.Number],
			old: java.lang.Number, nw: java.lang.Number) = resetSize
	}

	scene.heightProperty addListener new ChangeListener[java.lang.Number] {
		def changed(observableValue: ObservableValue[_ <: java.lang.Number],
				old: java.lang.Number, nw: java.lang.Number) = resetSize
	}

	def resetSize {
		//root.setLayoutX(-800)
		//root.setLayoutY(-800)
		scene.setRoot(root)
		stage setScene scene
		stage.show()
	}

	def receive = {
		case Show =>
			log.info("UI started! Showing...")
			stage.show()
		case SubTimer =>
			bot ! BotClient.Subscribe
		case AddWaypoint(wp) => addWaypoint(wp)
		case DelWaypoint(id) => removeWaypoint(id)
		case BotSnapshot(wps, pos, desire) =>
			log.info("Got snapshot!")

			curPos = pos
/*
			root.setTranslateX(-curPos.x * latScale)
			root.setTranslateY(-curPos.z * latScale)
			root.setTranslateY(curPos.y * latScale)

			selfCircle.setTranslateX(curPos.x)
			selfCircle.setTranslateY(curPos.z)
			selfCircle.setTranslateZ(-curPos.y)
*/
			//wpNodes.keySet foreach removeWaypoint

			waypoints.values map { wp =>
				val dy = wp.pos.y - curPos.y

				if(math.abs(dy) > 3) removeWaypoint(wp.id)
			}

			/*val screen = Screen.getPrimary
			val bounds = screen.getVisualBounds

			stage.setX(bounds.getMinX)
			stage.setY(bounds.getMinY)
			stage.setWidth(bounds.getWidth)
			stage.setHeight(bounds.getHeight)*/

			stage setScene scene
			stage.show()

			var newWps = Set[Waypoint]()
			wps foreach { wp =>
				if(waypoints.get(wp.id) == Some(wp)) {
					//do nothing?
				} else {
					addWaypoint(wp)
					newWps += wp
				}
			}

			newWps foreach addConnections
		case BotPosition(pos) =>
			curPos = pos

			root.setTranslateX(-curPos.x * latScale)
			root.setTranslateY(-curPos.z * latScale)
			root.setTranslateZ(curPos.y - 1.2)



			selfCircle.setTranslateX(curPos.x)
			selfCircle.setTranslateY(curPos.z)
			selfCircle.setTranslateZ(-curPos.y)
	}
}