package com.colingodsey.mcbot.client

import akka.actor._
import com.colingodsey.mcbot.network.{NettyClientProtocolStream, NettyClientConnection, TCPConnection, ProtocolStream}
import com.colingodsey.mcbot.protocol
import com.colingodsey.mcbot.protocol._
import com.colingodsey.mcbot.protocol.ClientProtocol.Metadata
import scala.concurrent.duration._
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

object BotClient {
	case class Settings(host: String, port: Int)
	case object PhysicsTick
	case object Respawn
	case object LongTick
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

	val jumpSpeed = 10

	val flatNeighbs = Seq(
		Point3D(-1, 0, 0),
		Point3D(0, 0, -1),
		Point3D(1, 0, 0),
		Point3D(0, 0, 1)
	)

}

class BotClient(settings: BotClient.Settings) extends Actor with ActorLogging
		with Stash with WorldClient with PathFinding[Block, Point3D] {
	import settings._
	import BotClient._

	implicit def ec = context.dispatcher

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


	//context.system.scheduler.scheduleOnce(5.seconds, stream, spr.ChatMessage("/kill"))

	var uuid = "?"
	//var selfEnt = Entity(-1, onGround = false)
	var selfId = -1
	var timeOfDay: Long = 0
	var stanceDelta = 1.62
	var joined = false
	var food = 1.0
	var gettingPath = false
	var lastPosEnt: Option[Player] = None
	var heldItem = 0
	var lastTime = curTime
	var moveGoal: Option[Point3D] = None
	var curPath: Seq[Point3D] = Nil
	var direction = Point3D.zero//(math.random, 0, math.random).normal

	var pluginMessage: Option[spr.PluginMessage] = None

	implicit val to = Timeout(30.seconds)

	context watch stream

	def curTime = System.currentTimeMillis / 1000.0

	def selfEnt = entities(selfId).asInstanceOf[Player]

	def footPos = selfEnt.pos - Point3D(0, stanceDelta, 0)
	def footBlockPos = footPos + Point3D(0, 0.5, 0)
	def footBlock = getBlock(footBlockPos)

	def dead = selfEnt.health <= 0
	def movementSpeed = selfEnt.prop("generic.movementSpeed") * movementSpeedModifier
	def maxHealth = selfEnt.prop("generic.maxHealth")

	def legalNeighbors(state: Block): Stream[(Block, Point3D)] = {
		val posBlocks = flatNeighbs.toStream map { x =>
			(getBlock(state.globalPos.toPoint3D + x), x)
		}
		val localBottom = getBlock(state.globalPos.toPoint3D + Point3D(0, -1, 0))

		// XX topBlock
		// XX block
		// == bottomBlock
		val flatN = posBlocks filter { case (block, move) =>
			val p = block.globalPos.toPoint3D
			def topBlock = getBlock(p + Point3D(0, 1, 0))
			def bottomBlock = getBlock(p + Point3D(0, -1, 0))

			block.btyp.isPassable && topBlock.btyp.isPassable && !bottomBlock.btyp.isPassable
		}

		val lowerN = posBlocks flatMap { case (block, move) =>
			val p = block.globalPos.toPoint3D
			def topBlock = getBlock(p + Point3D(0, 1, 0))
			def lowerBlock = getBlock(p + Point3D(0, -1, 0))
			def bottomBlock = getBlock(p + Point3D(0, -2, 0))

			if(block.btyp.isPassable && topBlock.btyp.isPassable &&
					lowerBlock.btyp.isPassable && !bottomBlock.btyp.isPassable)
				Some(lowerBlock, move + Point3D(0, -1, 0))
			else None
		}

		lazy val upperPossible = getBlock(
			state.globalPos.toPoint3D + Point3D(0, 2, 0)).btyp.isPassable
		val upperN = posBlocks flatMap { case (block, move) =>
			if(!upperPossible) Nil
			else {
				val p = block.globalPos.toPoint3D
				def topBlock = getBlock(p + Point3D(0, 2, 0))
				def midBlock = getBlock(p + Point3D(0, 1, 0))
				//val bottomBlock = block

				if(!block.btyp.isPassable && topBlock.btyp.isPassable &&
						midBlock.btyp.isPassable)
					Some(midBlock, move + Point3D(0, 1, 0))
				else None
			}
		}

		if(localBottom.btyp.isPassable || !state.btyp.isPassable) Stream()
		else flatN #::: lowerN #::: upperN
	}

	//this accesses local state in another thread... dangerous
	def getPath(): Unit = moveGoal match {
		case Some(p) if !gettingPath =>
			gettingPath = true
			val fut = Future {
				val floorTargetBlock = getBlock(p + Point3D(0, -1, 0))
				val endTargetBlock = getBlock(p)

				val targetBlock = if(floorTargetBlock.btyp.isPassable) floorTargetBlock
				else endTargetBlock

				if(!targetBlock.btyp.isPassable) Nil
				else pathFrom(footBlock, targetBlock, 10000) match {
					case Some(path) =>
						val boff = Block.halfBlockVec
						var start = footBlock.globalPos.toPoint3D
						val res = path.toVector.map { v =>
							start += v

							start
						}

						res
					case _ => Nil
				}
			}

			fut onComplete { res =>
				gettingPath = false

				res match {
					case Failure(t) =>
						log.error(t, "getPath failed")
					case _ =>
				}
			}

			fut.map(PathFound) pipeTo self
		case _ if gettingPath =>
		case None => self ! PathFound(Nil)
	}

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

				if(direction.length > 0) {
					if(direction.length > 1) direction = direction.normal

					val moveVec = direction * movementSpeed * dt

					//println(addLen, moveVec, moveLen, movementSpeed)
					updateEntity(selfId) { case ent: Player =>
						ent.copy(vel = ent.vel + moveVec)
					}
				}
			}

			val follow = playerOpt("colinrgodsey")
			if(follow.isDefined) {
				val f = follow.get

				val vec = (f.pos - selfEnt.pos)
				val dir = Point3D(vec.x, 0, vec.z)

				//if(dir.length > 9) direction = dir.normal
				//else direction = Point3D.zero

				moveGoal = Some(f.pos)

				//lookAt(direction)
				//println(direction)
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

				if(vec.length < 0.5) {
					curPath = curPath.tail
					//if(!curPath.isEmpty) say("Next stop, " + curPath.headOption)
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
					len = dir.length / math.pow(i + 1, 2)
				} yield dir.normal * len

				val dir = dirs.reduce(_ + _)

				//val dir = (nextStep - footBlockPos)

				direction = Point3D(dir.x, 0, dir.z)
				val dl = direction.length

				direction = if(dl < 0.05) Point3D.zero
				else direction.normal
			} else {
				direction = Point3D.zero
			}
		case PhysicsTick => //not joined... ignore
		case Respawn => respawn
		case PathFound(path) =>
			var startIdx = 0
			var closest = 10000.0
			path.zipWithIndex foreach { case (x, idx) =>
				val pos = x + Point3D(0.5, 0, 0.5)
				val vec = footPos - pos
				if(vec.length < closest) {
					closest = vec.length
					startIdx = idx
				}
			}
			if(path.isEmpty) curPath = Nil
			else curPath = path.drop(startIdx)

			if(!curPath.isEmpty)
				println("curpath: " + curPath)
		case LongTick if joined =>
			//if(math.random < 0.3) direction = Point3D(math.random - 0.5, 0, math.random - 0.5).normal
			//direction = Point3D(1, 0, 0)

			if(dead) {
				context.system.scheduler.scheduleOnce(2.5.seconds, self, Respawn)
				joined = false
			} else {
				getPath()
			}

			//if(math.random < 0.2) jump()

			if(direction !~~ Point3D.zero) try {
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

	override def postStop {
		context.system.shutdown()
		super.postStop
	}

	def receive = connecting
}
