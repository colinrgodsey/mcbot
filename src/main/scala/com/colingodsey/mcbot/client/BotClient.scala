package com.colingodsey.mcbot.client

import akka.actor._
import com.colingodsey.mcbot.network.{NettyClientProtocolStream, NettyClientConnection, TCPConnection, ProtocolStream}
import com.colingodsey.mcbot.protocol
import com.colingodsey.mcbot.protocol._
import com.colingodsey.mcbot.protocol.ClientProtocol.Metadata
import scala.concurrent.duration._
import scala.concurrent.blocking
import spray.client.pipelining._
import spray.json._
import scala.util.{Success, Failure}
import spray.httpx.SprayJsonSupport._
import java.security._
import spray.httpx.encoding.Deflate
import spray.httpx.marshalling._
import spray.httpx.unmarshalling._
import spray.http.HttpRequest
import scala.concurrent.Future
import spray.json.DefaultJsonProtocol._
import java.security.spec.{X509EncodedKeySpec, PKCS8EncodedKeySpec}
import javax.crypto.{SecretKeyFactory, EncryptedPrivateKeyInfo, Cipher}
import org.bouncycastle.jce.provider.BouncyCastleProvider
import javax.security.cert.X509Certificate
import javax.crypto.spec.SecretKeySpec
import java.io.{File, InputStreamReader, ByteArrayInputStream}
import java.security.cert.CertificateFactory
import org.bouncycastle.util.encoders.Base64
import org.parboiled.common.FileUtils
import java.math.BigInteger
import org.bouncycastle.openssl.PEMParser
import spray.http.HttpRequest
import scala.util.Failure
import scala.util.Success
import org.bouncycastle.asn1.x509.SubjectPublicKeyInfo
import org.bouncycastle.openssl.jcajce.JcaPEMKeyConverter
import org.bouncycastle.crypto.{KeyGenerationParameters, CipherKeyGenerator}
import akka.util.Timeout
import akka.pattern._
import com.colingodsey.logos.collections.{Point3D, Dimensions}
import Dimensions.Three._
import protocol.ClientProtocol
import protocol.{ClientProtocol => cpr}
import protocol.{ServerProtocol => spr}
import com.colingodsey.mcbot.world._
import scala.Some
import com.colingodsey.mcbot.world.Player
import akka.event.LoggingReceive

object BotClient {
	case class Settings(host: String, port: Int)
	case object PhysicsTick
	case object Respawn
	case object LongTick
	case object PathTick
	case object SaveWaypoints
	case class PathFound(path: Seq[Point3D])

	def props(settings: BotClient.Settings) = Props(classOf[BotClient], settings)

	implicit object JsObjectWriter extends RootJsonFormat[JsObject] {
		def write(jsObject: JsObject) = jsObject
		def read(value: JsValue) = value.asJsObject
	}

	implicit object JsArrayWriter extends RootJsonFormat[JsArray] {
		def write(jsArray: JsArray) = jsArray
		def read(value: JsValue) = value.asInstanceOf[JsArray]
	}

	val jumpSpeed = 11

	val flatNeighbs = Seq(
		Point3D(-1, 0, 0),
		Point3D(0, 0, -1),
		Point3D(1, 0, 0),
		Point3D(0, 0, 1)
	)

}

class BotClient(settings: BotClient.Settings) extends Actor with ActorLogging
		with Stash with WorldClient with WaypointBot with BotNavigation {
	import settings._
	import BotClient._

	implicit def ec = context.system.dispatcher

	val username = "funnybot1"

	val sessionUrl = "https://sessionserver.mojang.com/session/minecraft/join"

	val stream = context.actorOf(Props(classOf[NettyClientProtocolStream],
		protocol.ClientProtocol, host, port),
		name = "proto-stream")

	val tickDelta = 50.millis//50.millis
	val pingTimer = context.system.scheduler.schedule(
		tickDelta, tickDelta, self, PhysicsTick)
	val posTimer = context.system.scheduler.schedule(
		2.seconds, 1.second, self, LongTick)
	val pathTimer = context.system.scheduler.schedule(
		2.seconds, 1.second, self, PathTick)
	val waypointSaveTimer = context.system.scheduler.schedule(
		15.seconds, 10.second, self, SaveWaypoints)

	//context.system.scheduler.scheduleOnce(5.seconds, stream, spr.ChatMessage("/kill"))

	var uuid = "?"
	//var selfEnt = Entity(-1, onGround = false)
	var selfId = -1
	var timeOfDay: Long = 0
	var stanceDelta = 1.62
	var joined = false
	var food = 1.0
	var lastPosEnt: Option[Player] = None
	var heldItem = 0
	var lastTime = curTime
	var targetingEnt: Option[Int] = None
	var moveGoal: Option[Point3D] = None
	var curPath: Seq[Point3D] = Nil
	var direction = Point3D.zero//(math.random, 0, math.random).normal

	var pluginMessage: Option[spr.PluginMessage] = None

	implicit val to = Timeout(30.seconds)

	context watch stream

	def maxPathLength: Int = 100

	def curTime = System.currentTimeMillis / 1000.0

	def selfEnt = entities(selfId).asInstanceOf[Player]

	def footPos = selfEnt.pos - Point3D(0, stanceDelta, 0)
	def footBlockPos = footPos + Point3D(0, 0.5, 0)
	def footBlock = getBlock(footBlockPos)

	def dead = selfEnt.health <= 0
	def movementSpeed = selfEnt.prop("generic.movementSpeed") * movementSpeedModifier
	def maxHealth = selfEnt.prop("generic.maxHealth")



	//this accesses local state in another thread... dangerous


	def lookAt(vec: Point3D) {
		val alpha1 = -math.asin(vec.normal.x) / math.Pi * 180
		val alpha2 =  math.acos(vec.normal.z) / math.Pi * 180
		val yaw = if(alpha2 > 90) 180 - alpha1
		else alpha1

		val pitch = -math.asin(vec.normal.y) / math.Pi * 180

		updateEntity(selfId) { case ent: Player =>
			ent.copy(yaw = yaw, pitch = pitch)
		}
	}

	def generateClientHash(req: login.client.EncryptionRequest) = {
		implicit def ec = context.dispatcher

		val secretKey = Auth.createSecretKey

		val spec = new X509EncodedKeySpec(req.publicKey.toArray)
		val factory = KeyFactory.getInstance("RSA")
		val pubKey = factory.generatePublic(spec)

		val verifyToken = Auth.cipher(Cipher.ENCRYPT_MODE, pubKey, req.verifyToken.toArray)
		val encryptedSecret = Auth.cipher(Cipher.ENCRYPT_MODE, pubKey, secretKey.getEncoded)

		stream ! login.server.EncryptionResponse(encryptedSecret, verifyToken)
	}

	def connecting: Receive = {
		case TCPConnection.Connected =>
			log.info("Logging in...")
			context.become(normal)
			stream ! login.client
			stream ! handshake.server.Handshake.login(host, port)
			stream ! login.server.LoginStart(username)
	}

	def sendPositionAndLook =
		stream ! spr.PlayerPositionAndLook(selfEnt.pos.x,
			selfEnt.pos.y - stanceDelta, selfEnt.pos.y, selfEnt.pos.z,
			selfEnt.yaw.toFloat, selfEnt.pitch.toFloat, selfEnt.onGround)

	def sendLook = stream ! spr.PlayerLook(
		selfEnt.yaw.toFloat, selfEnt.pitch.toFloat, selfEnt.onGround)

	def sendPosition =
		stream ! spr.PlayerPosition(selfEnt.pos.x, selfEnt.pos.y - stanceDelta,
			selfEnt.pos.y, selfEnt.pos.z, selfEnt.onGround)

	def holdItem(slot: Short) {
		stream ! spr.HeldItemChange(slot)
	}

	def respawn = {
		//sendPositionAndLook
		stream ! spr.ClientStatus.Respawn
		//joined = false

		log.info("Respawning...")
	}

	def sendPluginMessage {
		stream ! pluginMessage.get
	}

	def sendOnGround = {
		stream ! spr.Player(selfEnt.onGround)
	}

	def guaranteePlayer() = entities.get(selfId) match {
		case None => entities += selfId -> Player(selfId)
		case Some(x) =>
	}

	def jump(): Unit = if(selfEnt.onGround) updateEntity(selfId) { case ent: Player =>
		ent.copy(vel = ent.vel + Point3D(0, jumpSpeed, 0), onGround = false)
	}

	def loadingChunk() {
		context.become(waitingChunkReceive, false)
	}
	def chunkLoaded() {
		context.unbecome
		unstashAll()
	}

	def waitingChunkReceive: Receive = worldClientReceive orElse clientThink orElse {
		case _ => stash()
	}

	def say(str: String) = {
		//log.info("Saying: " + str)
		stream ! spr.ChatMessage(str)
	}

	def normal: Receive = worldClientReceive orElse blockChange orElse clientThink orElse unhandled

	def unhandled: Receive = {
		case packet: Packet =>
			log.info("Unhandled packet " + packet)
	}

	val clientThink: Receive = {
		case cpr.EntityStatus(eid, 2) if eid == selfId => //we died??
			/*updateEntity(selfId) { e =>
				e.copy(health = 0)
			}*/
			log.info("We died?")
		case x: cpr.UpdateHealth =>
			updateEntity(selfId) { case e: Player =>
				e.copy(health = x.health)
			}
			food = x.food
			log.info("Health " + x.health)
		//case cpr.SpawnPosition(x, y, z) =>
		//track spawn pos i guess?
		case cpr.PositionAndLook(x, y, z, yaw, pitch, onGround) =>
			val pos = try Point(x, y, z) catch {
				case x: Throwable =>
					log.error("Got bad position!!")
					throw x
					Point3D.zero
			}
			guaranteePlayer()

			val lastPos = selfEnt.pos

			updateEntity(selfId) { case e: Player =>
				e.copy(pos = pos, yaw = yaw, vel = Point3D.zero,
					pitch = pitch, onGround = onGround)
			}

			//lastPosEnt = Some(selfEnt)
			sendPositionAndLook

			lastPosEnt = None

			//0.1 or greater than 1.65
			if(!joined) {
				log.info("We joined at " + selfEnt.pos)

				holdItem(heldItem.toShort)

				lastTime = curTime

				joined = true

			} else
				log.info(s"Correction packet: ${selfEnt.pos - lastPos}. $lastPos -> ${selfEnt.pos}")
		case cpr.HeldItemChange(item) =>
			heldItem = item
		case cpr.TimeUpdate(newAge, newTimeOfDay) =>
			timeOfDay = newTimeOfDay
		case cpr.KeepAlive(id) =>
			//println(spr.KeepAlive(id))
			stream ! spr.KeepAlive(id)
			Thread.sleep(2000)
		case cpr.JoinGame(eid, gamemode, dim, difficulty, maxPlayers, level) =>
			log.info(s"Joined game: $level")
			selfId = eid
		case cpr.PluginMessage(chan, data) =>
			pluginMessage = Some(spr.PluginMessage(chan, data))
			//stream ! spr.ClientSettings("en_GB", 0, 0, false, 2, false)
			stream ! spr.ClientSettings("en_US", 16, 0, true, 3, true)
			sendPluginMessage
		case cpr.ChatMessage(msg) => try {
			val js = msg.asJson
			val str = js.asJsObject.fields("with").
					asInstanceOf[JsArray].elements(1).asInstanceOf[JsString].value
			processChatCommand(str)
		} catch { case t: Throwable =>
			//log.error(t, "failed to parse chat msg " + msg)
			log.info("failed to parse chat msg " + msg)
		}
		case x: cpr.SoundEffect =>
		case x: login.client.EncryptionRequest =>
			generateClientHash(x)
		case login.client.LoginSuccess(_uuid, usr) =>
			log.info("Logged in!")
			require(usr == username)
			uuid = _uuid
			stream ! cpr

		case x: cpr.PlayerListItem =>
		case x: cpr.ChangeGameState =>

		case PhysicsTick if joined =>
			val ct = curTime
			//val dt = tickDelta.toMillis / 1000.0
			val dt = ct - lastTime

			if(dt > 0.01) {
				val theta = System.currentTimeMillis * 0.0003

				//direction = Point3D(math.cos(theta), 0, math.sin(theta))
				move(selfId, dt, CollisionDetection.playerBody(stanceDelta))

				if(direction !~~ Point3D.zero) lookAt(direction)

				lastTime = ct

				val walkDir = Point3D(direction.x, 0, direction.z)

				if(walkDir.length > 0.01) {
					val moveVec = walkDir.normal * movementSpeed * dt

					//println(addLen, moveVec, moveLen, movementSpeed)
					updateEntity(selfId) { case ent: Player =>
						ent.copy(vel = ent.vel + moveVec)
					}
				}
			}

			val follow = playerOpt("colinrgodsey")
			if(follow.isDefined) {
				val f = follow.get

				//targetingEnt = Some(f.id)
			}

			val posChanged = !lastPosEnt.isDefined ||
					(selfEnt.pos != lastPosEnt.get.pos)
			val lookChanged = !lastPosEnt.isDefined ||
					(selfEnt.look != lastPosEnt.get.look)

			//if(posChanged) log.info((selfEnt.pos, lastPosEnt.map(_.pos)).toString)

			lastPosEnt = Some(selfEnt)

			if(posChanged && lookChanged)
				sendPositionAndLook
			else if(posChanged) sendPosition
			else if(lookChanged) sendLook
			else sendOnGround

			if(!curPath.isEmpty) {
				val nextStep = curPath.head + Block.halfBlockVec

				val vec = (nextStep - footBlockPos)

				if(vec.length < 0.8) {
					curPath = curPath.tail
					//if(!curPath.isEmpty) say("Next stop, " + curPath.headOption)
					/*if(curPath.isEmpty) */direction = Point3D.zero
				}

				if(vec.length > 2) {
					direction = Point3D.zero
					curPath = Nil
					//getPath()
					self ! PathTick
				}
			}

			if(!curPath.isEmpty) {
				val nextStep = curPath.head

				if(nextStep.y > footPos.y) jump()

				val dirs = for {
					i <- 0 until 3
					if i < curPath.length
					step = curPath(i) + Block.halfBlockVec
					dir = step - footBlockPos
					len = dir.length / math.pow(i + 1, 6)
				} yield dir.normal * len

				val dir = dirs.reduce(_ + _)

				//val dir = (nextStep - footBlockPos)

				direction = dir

				if(selfEnt.vel.length < 0.01)
					direction += Point3D.random
			} //else direction = Point3D.zero
		case PhysicsTick => //not joined... ignore
		case Respawn => respawn
		case SaveWaypoints =>
			saveWaypoints()
		case PathFound(path) =>
			var startIdx = 0
			var closest = 10000.0
			path.zipWithIndex foreach { case (x, idx) =>
				val pos = x + Point3D(0.5, 0, 0.5)
				val vec = footPos - pos
				if(vec.length < closest && idx < 4) {
					closest = vec.length
					startIdx = idx
				}
			}

			/*if(moveGoal.isDefined && path.isEmpty) {
				val dir = moveGoal.get - selfEnt.pos

				direction = dir
			}*/

			direction = Point3D.zero

			if(startIdx > 0) println(startIdx)

			if(path.isEmpty) {
				direction = Point3D.zero//curPath = Nil
				moveGoal = None
			} else curPath = path.drop(startIdx)

			if(!curPath.isEmpty)
				println("curpath: " + curPath)
			else if(targetingEnt.isDefined && curPath.isEmpty
					&& lastWaypoint.isDefined && !moveGoal.isDefined) {
				val target = entities(targetingEnt.get)
				val curWp = lastWaypoint.get

				//if closer to target then closest waypoint is to target

				log.info("Polling for closest waypoint to target")

				val targetClosest = getNearWaypoints(target.pos, maxNum = 6, radius = 300).toStream/*.filter { wp =>
					try !getShortPath(wp.pos, target.pos).isEmpty catch {
						case x: FindChunkError => false
					}
				}*/

				targetClosest.headOption match {
					case Some(x) if x.id != curWp.id =>
						val waypointPath = finder(target.pos).pathFrom(curWp, x, of = 2000).toSeq.flatten
						if(waypointPath.isEmpty) {
							moveGoal = None
							log.info("No wp path to closest wp by entw")
						} else {
							val nextWp = waypoints(waypointPath.head.destId)

							log.info("Tracking wp " + nextWp)
							moveGoal = Some(nextWp.pos)
						}
						//getPath(target.pos)
					case _ => moveGoal = None
				}
			} else updateEntity(selfId) { case ent: Player =>
				ent.copy(vel = ent.vel + Point3D.random * 0.2)
			}
		case PathTick if !dead && joined =>
			if(targetingEnt.isDefined) {
				val ent = entities(targetingEnt.get)
				if(!moveGoal.isDefined || math.random < 0.15) moveGoal = Some(ent.pos)
			}

			if(moveGoal.isDefined) getPath(moveGoal.get)

			if(!targetingEnt.isDefined && !moveGoal.isDefined && curPath.isEmpty) try {
				//pick a random place...

				val wps = getNearWaypoints(selfEnt.pos, maxNum = 5).map(_.pos)
				val wpAvg = if(wps.isEmpty) selfEnt.pos else wps.reduceLeft(_ + _) / wps.length

				val wpVec = selfEnt.pos - wpAvg
				val flatIshRandom = Point3D(math.random * 2 - 1, math.random * 0.2 - 0.1,
					math.random * 2 - 1).normal
				val vec = flatIshRandom * 70 + wpVec.normal * 50
				val hit = traceBody(CollisionDetection.SmallBox, footPos + Point3D(0, 1.5, 0), vec)

				log.info(s"random target $vec.normal res ${hit.headOption}")

				hit.headOption match {
					case Some(x) if x.dist > 1.2 =>
						var pos = vec.normal * (x.dist - 0.2) + selfEnt.pos
						while(getBlock(pos).btyp.isPassable && pos.y > 0)
							pos -= Point3D(0, 1, 0)

						moveGoal = Some(pos + Point3D(0, 1, 0))
					case _ =>
				}
			} catch {
				case x: FindChunkError => Nil
			}

			checkWaypoints
		case LongTick if joined =>
			//if(math.random < 0.3) direction = Point3D(math.random - 0.5, 0, math.random - 0.5).normal
			//direction = Point3D(1, 0, 0)

			if(dead) {
				context.system.scheduler.scheduleOnce(2.5.seconds, self, Respawn)
				joined = false
			}

			if(math.random < 0.03) {
				curPath = Nil
				updateEntity(selfId) { case ent: Player =>
					ent.copy(vel = ent.vel + Point3D.random * 0.2)
				}
			}

			//if(math.random < 0.2) jump()

			if(direction !~~ Point3D.zero && direction.length > 0.01) try {
				val l = 0.5
				val res = traceBody(CollisionDetection.UnitBox,
					selfEnt.pos + Point3D(0, -stanceDelta + 1, 0), direction * l)

				//if(res.head.dist < l) jump()
			} catch {
				case t: FindChunkError =>
			}

			log.debug(s"health = ${selfEnt.health}, pos = ${selfEnt.pos},  ground = ${selfEnt.onGround}, velL = ${selfEnt.vel.length}")
	}

	def processChatCommand(str: String) = str match {
		case "status" =>
			log.info(selfEnt.toString)
		case "die" => stream ! spr.ChatMessage("/kill")
		case x if x.startsWith("echo ") =>
			stream ! spr.ChatMessage(x.substring(5))
		case x => log.info("chat: " + x)
	}

	override def preStart {
		loadWaypoints()
		super.preStart
	}

	override def postStop {
		context.system.shutdown()
		super.postStop
	}

	def receive = connecting
}
