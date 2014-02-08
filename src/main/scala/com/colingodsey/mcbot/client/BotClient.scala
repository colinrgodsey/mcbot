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
import com.colingodsey.logos.collections.{Vec3, Dimensions}
import Dimensions.Three._
import protocol.ClientProtocol
import protocol.{ClientProtocol => cpr}
import protocol.{ServerProtocol => spr}
import com.colingodsey.mcbot.world._
import scala.Some
import com.colingodsey.mcbot.world.Player
import akka.event.LoggingReceive
import com.colingodsey.mcbot.client.WaypointManager.Waypoint
import com.colingodsey.collections.VecN

object BotClient {
	case class Settings(host: String, port: Int, username: String,
			wps: Seq[Waypoint],
			wpMasterRef: Option[ActorRef]) {
		def wpMaster = !wpMasterRef.isDefined
	}
	case object PhysicsTick
	case object Respawn
	case object LongTick
	case object PathTick
	case object SaveWaypoints
	case object Subscribe

	case class BotSnapshot(
		nearWaypoints: Set[Waypoint],
		curPos: Vec3,
		desire: VecN
	)

	case class BotPosition(curPos: Vec3)

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
}

class BotClient(settings: BotClient.Settings) extends Actor with ActorLogging
		with Stash with WorldClient with BotNavigation {
	import settings._
	import BotClient._
	import BotNavigation._

	log.info("Botclient ref " + self)

	implicit def ec = context.system.dispatcher

	val sessionUrl = "https://sessionserver.mojang.com/session/minecraft/join"

	val stream = context.actorOf(Props(classOf[NettyClientProtocolStream],
		protocol.ClientProtocol, host, port),
		name = "proto-stream")

	def safeSchedule(dur: FiniteDuration, ref: ActorRef, msg: Any) = {
		var c: Cancellable = null

		lazy val sched = context.system.scheduler

		def f {
			ref ! msg

			c = sched.scheduleOnce(dur)(f)
		}
		c = sched.scheduleOnce(dur * 2)(f)

		new Cancellable {
			override def cancel(): Boolean = c.cancel

			override def isCancelled: Boolean = c.isCancelled
		}
	}

	val tickDelta = 50.millis//50.millis
	val pingTimer = safeSchedule(
		tickDelta, self, PhysicsTick)
	val posTimer = safeSchedule(
		1.second, self, LongTick)
	val pathTimer = safeSchedule(
		1.second, self, PathTick)
	val waypointSaveTimer = safeSchedule(
		10.second, self, SaveWaypoints)

	val waypointFile = new File(s"./$username.wp.dat")
	val waypointSwapFile = new File(s"./$username.wp.tmp.dat")

	//context.system.scheduler.scheduleOnce(5.seconds, stream, spr.ChatMessage("/kill"))

	var uuid = "?"
	//var selfEnt = Entity(-1, onGround = false)
	var selfId = -1
	var timeOfDayTicks: Long = 0
	var stanceDelta = 1.62
	var joined = false
	var food = 1.0
	var lastPosEnt: Option[Player] = None
	var heldItem = 0
	var lastTime = curTime
	var subscribers = Set[ActorRef]()
	var desire = VecN.zero
	var isRaining = false

	//var discoverTokens: Double = 0
	var waterTokens: Double = 0

	var pluginMessage: Option[spr.PluginMessage] = None

	implicit val to = Timeout(30.seconds)

	context watch stream

	def curTime = System.currentTimeMillis / 1000.0

	def selfEnt = entities(selfId).asInstanceOf[Player]

	def footPos = selfEnt.pos - Vec3(0, stanceDelta, 0)
	def footBlockPos = footPos + Vec3(0, 0.5, 0)
	def footBlock = getBlock(footBlockPos)

	def timeOfDay = (timeOfDayTicks % 24000).toDouble / 24000
	def dayFac = math.sin(timeOfDay * math.Pi * 2)

	def isWpMaster = settings.wpMaster
	def wpMaster = settings.wpMasterRef.get

	val worldView: WorldView = this

	def dead = selfEnt.health <= 0
	def movementSpeed = selfEnt.prop("generic.movementSpeed") * movementSpeedModifier
	def maxHealth = selfEnt.prop("generic.maxHealth")

	def setDesires() {
		val homeFac = math.max((-dayFac), 0)
		val rainHome = if(isRaining) 100 else 0

		val water = waterTokens * 10
		val waterHome = water * 0.01

		val isLost0 = if(lastTransition.isDefined) {
			val transs = transFrom(lastTransition.get).map(qValue(_)("home"))
			if(!transs.isEmpty) {
				val sel = transs.max

				sel == 0
			} else false
		} else false

		var isLost = false

		val lostFac = if(isLost) 15 else 0

		desire = VecN("discover" -> (12.0 * dayFac + lostFac),
			"deadend" -> -(10 * dayFac + lostFac),
			"up" -> (if(isLost) 1.0 else 0.0),
			"home" -> (homeFac * 1000 + rainHome + waterHome),
			"water" -> -(10.0 + water))

		if(math.random < 0.2) log.info("desire " + desire)
	}

	def lookAt(vec: Vec3) {
		val alpha1 = -math.asin(vec.normal.x) / math.Pi * 180
		val alpha2 =  math.acos(vec.normal.z) / math.Pi * 180
		val yaw = if(alpha2 > 90) 180 - alpha1
		else alpha1

		val pitch = -math.asin(vec.normal.y) / math.Pi * 180

		updateEntity(selfId) { case ent: Player =>
			ent.copy(yaw = yaw, pitch = pitch)
		}
	}

	def randomPushSelf() {
		updateEntity(selfId) { case ent: Player =>
			ent.copy(vel = ent.vel + Vec3.random * 0.8)
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
		stream ! spr.ClientStatus.Respawn

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

	def jump(): Unit = {
		val eyeWater = footBlock.above.btyp.isWater
		val footWater = footBlock.btyp.isWater
		val js = if(footWater) jumpSpeed * 1.1
		else jumpSpeed

		if(!eyeWater && footWater && !selfEnt.onGround) {
			updateEntity(selfId) { case ent: Player =>
				ent.copy(vel = ent.vel + Vec3(0, js * 1.1, 0), onGround = false)
			}
		}

		if(selfEnt.onGround && !eyeWater) updateEntity(selfId) { case ent: Player =>
			ent.copy(vel = ent.vel + Vec3(0, js, 0), onGround = false)
		}/* else if(isWater) updateEntity(selfId) { case ent: Player =>
			ent.copy(vel = ent.vel + Vec3(0, jumpSpeed / 10, 0), onGround = false)
		}*/
	}

	def addVel(vel: Vec3) {
		updateEntity(selfId) { ent =>
			ent.entityCopy(vel = ent.vel + vel)
		}
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

	def makeBotSnapshot = {
		val wps = getNearWaypoints(selfEnt.pos, radius = 600, maxNum = 500)
		BotSnapshot(nearWaypoints = wps.toSet,
			curPos = selfEnt.pos, desire = desire)
	}

	def normal: Receive = worldClientReceive orElse blockChange orElse clientThink orElse unhandled

	def unhandled: Receive = {
		case packet: Packet =>
			log.info("Unhandled packet " + packet)
	}

	val clientThink: Receive = pathReceive orElse {
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
					Vec3.zero
			}
			guaranteePlayer()

			val lastPos = selfEnt.pos

			updateEntity(selfId) { case e: Player =>
				e.copy(pos = pos, yaw = yaw, vel = Vec3.zero,
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
				getPath(selfEnt.pos)
			} else
				log.info(s"Correction packet: ${selfEnt.pos - lastPos}. $lastPos -> ${selfEnt.pos}")
		case cpr.HeldItemChange(item) =>
			heldItem = item
		case cpr.TimeUpdate(newAge, newTimeOfDay) =>
			timeOfDayTicks = newTimeOfDay
		case cpr.KeepAlive(id) =>
			//println(spr.KeepAlive(id))
			stream ! spr.KeepAlive(id)
			//Thread.sleep(2000)
		case cpr.JoinGame(eid, gamemode, dim, difficulty, maxPlayers, level) =>
			log.info(s"Joined game: $level")
			selfId = eid
		case cpr.PluginMessage(chan, data) =>
			pluginMessage = Some(spr.PluginMessage(chan, data))
			//stream ! spr.ClientSettings("en_GB", 0, 0, false, 2, false)
			stream ! spr.ClientSettings("en_US", 16, 0, true, 0, true)
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
		case cpr.ChangeGameState(reason, value) => reason match {
			case 1 => //end raining
				isRaining = false
			case 2 => //start raining
				isRaining = true
			case _ =>
		}

		case PhysicsTick if joined && (curTime - lastTime) > 0.01 =>
			val ct = curTime
			//val dt = tickDelta.toMillis / 1000.0
			val dt = math.min(ct - lastTime, 0.1)

			val theta = System.currentTimeMillis * 0.0003

			//direction = Point3D(math.cos(theta), 0, math.sin(theta))

			try {
				val moveRes = move(selfId, dt, CollisionDetection.playerBody(stanceDelta),
					footBlock.btyp.isWater)

				if(!moveRes) {
					updateEntity(selfId) { case ent: Player =>
						ent.copy(pos = footBlockPos)
					}
					log.warning("trying to fix location to block location")
				}

				val curBlock = getBlock(selfEnt.pos)

				if(curBlock.btyp.isWater) {
					waterTokens += dt * 1
				}

				waterTokens -= 0.1 * dt
				waterTokens = math.min(math.max(waterTokens, 0), 100)

				val walkDir = if(footBlock.btyp.isWater)
					Vec3(direction.x, math.max(direction.y, 0), direction.z)
				else Vec3(direction.x, 0, direction.z)

				if(direction !~~ Vec3.zero) {
					lookAt(direction.normal)
				}

				//TODO: try not to max out velocity...
				if(walkDir !~~ Vec3.zero) {
					selfEnt.vel * walkDir.normal

					val xzVel = Vec3(selfEnt.vel.x, 0, selfEnt.vel.z)
					val vecPart = xzVel * walkDir.normal
					val remVec = xzVel - walkDir.normal * vecPart

					updateEntity(selfId) { case ent: Player =>
						ent.copy(vel = ent.vel - remVec * 5 * dt)
					}
				}

				lastTime = ct

				if(walkDir.length > epsilon) {
					//TODO: should this be dt or dt^2 ?
					/*val speed = if(footBlock.btyp.isWater) movementSpeed / 6
					else movementSpeed*/
					val moveVec = walkDir.normal * movementSpeed * dt

					//println(addLen, moveVec, moveLen, movementSpeed)
					updateEntity(selfId) { case ent: Player =>
						ent.copy(vel = ent.vel + moveVec)
					}
				}
			} catch {
				case x: FindChunkError =>
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

			if(posChanged)
				subscribers.foreach(_ ! BotPosition(selfEnt.pos))

			pathPhysicsTick(dt)
		case PhysicsTick => //log.info("dropped physics tick!")
		case Respawn => respawn
		case LongTick if joined =>
			//if(math.random < 0.3) direction = Point3D(math.random - 0.5, 0, math.random - 0.5).normal
			//direction = Point3D(1, 0, 0)

			setDesires()

			if(dead) {
				context.system.scheduler.scheduleOnce(2.5.seconds, self, Respawn)
				joined = false
			}

			if(direction !~~ Vec3.zero && direction.length > 0.01) try {
				val l = 0.5
				val res = traceBody(CollisionDetection.UnitBox,
					selfEnt.pos + Vec3(0, -stanceDelta + 1, 0), direction * l)

				//if(res.head.dist < l) jump()
			} catch {
				case t: FindChunkError =>
			}

			if(math.random < 0.2) subscribers.foreach(_ ! makeBotSnapshot)

			log.debug(s"health = ${selfEnt.health}, pos = ${selfEnt.pos},  ground = ${selfEnt.onGround}, velL = ${selfEnt.vel.length}")

		case wp: Waypoint =>
			addWaypoint(wp, isWpMaster)
			//subscribers.foreach(_ ! wp)

		case snap: BotSnapshot =>
			snap.nearWaypoints foreach { x =>
				addWaypoint(x, false)
			}

		case Subscribe =>
			subscribers += sender

			//log.info(s"$sender just subscribed!")

			context watch sender

			sender ! makeBotSnapshot
		case Terminated(ref) if subscribers(ref) =>
			subscribers -= ref
	}

	def processChatCommand(str: String) = str match {
		case "status" =>
			log.info(selfEnt.toString)
		case "follow" =>
			val follow = playerOpt("colinrgodsey")
			targetingEnt = follow.map(_.id)
			say("Following")
		case "mark home" =>
			if(lastWaypoint.isDefined) {
				val wp = lastWaypoint.get

				addWaypoint(wp.updateProperty("home", 100.0))

				say("New home at wp " + wp.pos)
				//println(lastWaypoint.get.property("home"))
			}
			println("home " + lastWaypoint)
		case "stop" =>
			say("Stopping goal")
			targetingEnt = None
			curPath = Stream()
			moveGoal = None
		case "die" => stream ! spr.ChatMessage("/kill")
		case x if x.startsWith("echo ") =>
			stream ! spr.ChatMessage(x.substring(5))
		case x => log.info("chat: " + x)
	}

	override def preStart {
		//loadWaypoints()
		settings.wps.foreach(addWaypoint)
		super.preStart
	}

	override def postStop {
		context.system.shutdown()
		super.postStop
	}

	def receive = connecting
}
