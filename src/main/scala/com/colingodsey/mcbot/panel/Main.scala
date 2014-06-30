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
import javafx.scene.shape.{Line, Circle}
import com.colingodsey.mcbot.client.BotClient.{BotPosition, BotSnapshot}
import com.colingodsey.mcbot.client.{WorldTile, BotLearn, BotClient}
import com.colingodsey.logos.collections.{Vec, Vec3}
import javafx.event.EventHandler
import javafx.geometry.Pos
import javafx.beans.value.{ObservableValue, ChangeListener}
import javax.swing.event.ChangeEvent
import com.colingodsey.collections.{VecN, MapVector}
import com.colingodsey.mcbot.client.WorldTile.TileState
import com.colingodsey.mcbot.client.BotLearn.Action
import com.colingodsey.mcbot.client.StateSaveActor.LoadState
import javafx.scene.input.ScrollEvent

class Main extends Application {
	val defCfg = ConfigFactory.load(getClass.getClassLoader)
	val panelConfig = defCfg.getConfig("mcbot.panel").withFallback(defCfg)

	implicit val system = ActorSystem("MCBotPanel", panelConfig)

	val botSel = system.actorSelection(
		"akka.tcp://MCBotClient@192.168.0.102:3488/user/bot-client2")

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

	case class AddTileState(state: TileState)
	case class DelState(state: TileState)
}

class UIStageActor(stage: Stage, bot: ActorSelection) extends Actor with ActorLogging {
	import UIStageActor._

	import context.dispatcher

	val yScale = 1 / 20
	def latScale = 5.4 * scrollRatio

	val subscribeTimer = context.system.scheduler.schedule(
		1.seconds, 5.seconds, self, SubTimer)

	val root = new StackPane
	val content = new StackPane
	val scene = new Scene(root, 800, 600, Color.BLACK)
	val camera = new PerspectiveCamera
	val selfCircle = new Circle

	camera.setFieldOfView(160)//90)
	scene.setCamera(camera)

	var tileStates = Map[TileState, Map[Action, VecN]]()
	var stateNodes = Map[TileState, Group]()
	var curPos = Vec3.zero
	var curDesire: Vec = MapVector()
	var scrollRatio = 1.0

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
	root.getChildren add content

	content setCache true

	root.setOnScroll(new EventHandler[ScrollEvent] {
		def handle(event: ScrollEvent) {
			//node.setTranslateX(node.getTranslateX() + event.getDeltaX());
			//node.setTranslateY(node.getTranslateY() + event.getDeltaY());

			scrollRatio += event.getDeltaY / 100.0
		}
	})

	log.info("UI starting...")

	bot ! BotClient.Subscribe

	def removeTileState(tileState: TileState) = stateNodes.get(tileState) match {
		case Some(x) =>
			content.getChildren remove x
			//x.getParent.asInstanceOf[Group].getChildren remove x
			stateNodes -= tileState
			tileStates -= tileState
			true
		case None => false
	}

	def addTileState(loadState: LoadState) {
		val LoadState(tileState: TileState, actions) = loadState

		removeTileState(tileState)

		val node = new Group()
		val circle = new Circle()

		node.setLayoutX(0)
		node.setLayoutY(0)
		node.setAutoSizeChildren(false)

		node.setTranslateX(tileState.pos.x)
		node.setTranslateY(tileState.pos.z)
		node.setTranslateZ(-tileState.pos.y)

		node.getChildren add circle

		circle.setFill(Color.color(1, 1, 1, 0.2))
		circle.setRadius(0.5)

		actions foreach {
			case (WorldTile.TileTransition(dest), q) =>
				val vec = (dest.pos - tileState.pos) / 2

				//Some(v, q * curDesire)
				val line = new Line

				val desireWeight = q * curDesire

				val col = desireWeight / 20
				val discoverFactor = math.max(math.min(col, 1), 0)
				//println(disc, discoverFactor)
				line.setStroke(Color.color(1 - discoverFactor, discoverFactor, 0, 0.2))

				line.setStartX(0)
				line.setStartY(0)
				line.setEndX(vec.x)
				line.setEndY(vec.z)

				line.setStrokeWidth(0.3)

				node.getChildren add line
			case _ =>
		}

		content.getChildren add node
		tileStates += tileState -> actions
		stateNodes += tileState -> node
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

	def shouldHideState(s: TileState) =
		(s.pos - curPos).length > 100 || math.abs(s.pos.y - curPos.y) > 10

	def receive = {
		case Show =>
			log.info("UI started! Showing...")
			stage.show()
		case SubTimer =>
			bot ! BotClient.Subscribe
		case a @ LoadState(state: TileState, actions) =>
			if(!shouldHideState(state)) addTileState(a)
			//log.info("got tile state")
 		//case AddWaypoint(wp) => addWaypoint(wp)
		//case DelWaypoint(id) => removeWaypoint(id)
		case BotSnapshot(pos, desire) =>
			log.info("Got snapshot!")

			curPos = pos
			curDesire = desire
/*
			root.setTranslateX(-curPos.x * latScale)
			root.setTranslateY(-curPos.z * latScale)
			root.setTranslateY(curPos.y * latScale)

			selfCircle.setTranslateX(curPos.x)
			selfCircle.setTranslateY(curPos.z)
			selfCircle.setTranslateZ(-curPos.y)
*/
			//stateNodes.keySet foreach removeTileState

			/*val screen = Screen.getPrimary
			val bounds = screen.getVisualBounds

			stage.setX(bounds.getMinX)
			stage.setY(bounds.getMinY)
			stage.setWidth(bounds.getWidth)
			stage.setHeight(bounds.getHeight)*/

			stage setScene scene
			stage.show()

			stateNodes.keySet.filter(shouldHideState) foreach removeTileState

		case BotPosition(pos) =>
			curPos = pos

			root.setTranslateX(-curPos.x * latScale)
			root.setTranslateY(-curPos.z * latScale)
			root.setTranslateZ(curPos.y - 1.2)

			root.setScaleX(latScale)
			root.setScaleY(latScale)

			selfCircle.setTranslateX(curPos.x)
			selfCircle.setTranslateY(curPos.z)
			selfCircle.setTranslateZ(-curPos.y)
	}
}