package com.colingodsey.mcbot.world

import com.colingodsey.logos.collections.{IPoint3D, Point3D}
import com.colingodsey.mcbot.protocol
import com.colingodsey.mcbot.protocol._
import com.colingodsey.mcbot.protocol.{ClientProtocol => cpr, ServerProtocol => spr, _}
import java.util.zip.{Inflater, Deflater}
import scala.collection.immutable.VectorBuilder
import akka.actor._
import akka.pattern._
import akka.event.LoggingAdapter
import scala.concurrent.{ExecutionContext, Future, blocking}

case class FindChunkError(x: Int, y: Int, z: Int, point: IPoint3D) extends Exception(
	s"chunk not found: $point for point $x, $y, $z")

trait WorldView {
	def entities: Map[Int, Entity]
	def chunks: Map[IPoint3D, Chunk]
	def players: Map[String, Int]

	def getChunk(x: Int, y: Int, z: Int): Chunk = {
		val point = IPoint3D(
			math.floor(x.toDouble / Chunk.dims.x).toInt,
			math.floor(y.toDouble / Chunk.dims.y).toInt,
			math.floor(z.toDouble / Chunk.dims.z).toInt)

		chunks.getOrElse(point,
			throw FindChunkError(x, y, z, point))
	}

	def getChunk(pos: Point3D): Chunk = getChunk(math.floor(pos.x).toInt,
		math.floor(pos.y).toInt, math.floor(pos.z).toInt)
	def getChunk(pos: IPoint3D): Chunk = getChunk(pos.x, pos.y, pos.z)

	def getBlock(pos: Point3D): Block = {
		if(pos.y < 0 || pos.y > 255)
			return NoBlock(pos.x.toInt, pos.y.toInt, pos.z.toInt)

		val chunk = getChunk(pos)

		chunk(math.floor(pos.x).toInt - chunk.x * Chunk.dims.x,
			math.floor(pos.y).toInt - chunk.y * Chunk.dims.y,
			math.floor(pos.z).toInt - chunk.z * Chunk.dims.z)
	}
}

trait World { wv: WorldView =>
	def log: LoggingAdapter

	var entities = Map[Int, Entity]()
	@volatile var chunks = Map[IPoint3D, Chunk]()
	var players = Map[String, Int]()

	def player(name: String) = entities(players(name)).asInstanceOf[Player]
	def playerOpt(name: String) = players.get(name).flatMap(entities.get(_).map(_.asInstanceOf[Player]))

	//TODO: add extractors to convert entity types.... case Player(player) =>   will convert
	def updateEntity(id: Int)(f: Entity => Entity) {
		//val updated = f(entities.getOrElse(id, Entity(id)))
		entities.get(id) match {
			case Some(x) =>
				val updated = f(x)
				entities += id -> updated
			case None =>
				log.warning(s"Update for ent $id that doesnt exist yet")
		}
	}
}

object WorldClient {
	case class RemoveChunks(poss: Set[IPoint3D])
	case class AddChunks(chunks: Set[Chunk])

	//the below can be run async!
	val processChunks: Function[Any, Any] = blocking {
		case cpr.ChunkData(chunkX, chunkZ, groundUp, bitmask, addBitmask, data) =>
			val decompdData = Chunk.inflateData(data.toArray)

			val cks = Chunk(chunkX, chunkZ, bitmask, addBitmask, groundUp, true, decompdData)

			if(groundUp && bitmask == 0) //empty column, remove
				RemoveChunks(cks.map(_.pos).toSet)
			else AddChunks(cks.toSet)
		case cpr.MapChunkBulk(chunkColumnCount, skylight, compdData, metas) =>
			var idx = 0
			val data = Chunk.inflateData(compdData)

			//loop through mapchunkmeta for each piece array type, store None/Some

			val cSize = Chunk.chunkSize(skylight)
			val lenArray = metas.map(meta =>
				Chunk.num1Bits(meta.primaryBitmask) * cSize +
						Chunk.biomeArrSize)

			require(lenArray.sum == data.length,
				s"Expected ${lenArray.sum} bytes, found ${data.length}. Lens: ${lenArray}")

			val cols = metas flatMap { case cpr.MapChunkMeta(chunkX, chunkZ, bitmask, addBitmask) =>
				val nChunks = Chunk.num1Bits(bitmask)

				val totSize = cSize * nChunks + Chunk.biomeArrSize

				val cks = Chunk(chunkX, chunkZ, bitmask, addBitmask,
					true, skylight, data.slice(idx, idx + totSize))

				require(nChunks == cks.count(!_.isEmpty))

				idx += totSize
				cks
			}
			AddChunks(cols.toSet)
	}
}

trait WorldClient extends World with WorldView with CollisionDetection {
	import CollisionDetection._
	import WorldClient._

	val ticksPerSecond = 20
	val movementSpeedModifier = ticksPerSecond * 2.15
	val velocityFactor = 1.0 / 8000 * ticksPerSecond

	def self: ActorRef

	implicit def ec: ExecutionContext

	//behaviors
	def loadingChunk()
	def chunkLoaded()


	def move(eid: Int, dt: Double, body: Body): Unit = updateEntity(eid) { ent =>
		import CollisionDetection._

		require(dt > epsilon)

		val traceFunc = body match {
			//case x: SphereBody => traceBody(x, _: Point3D, _: Point3D)
			case x: BoxBody =>
				//log.warning("box bodys dont work great yet...")
				traceBody(x, _: Point3D, _: Point3D)
		}

		//println(ent.pos, ent.onGround, "ENT")

		val (gravity, terminal, drag) = ent match {
			case _: LivingEntity => (32, 78.4, 0.4)
			//case Minecart(cart) =>
			case _ => (16, 39.2, 0.4)
		}

		val gravAcc = Point3D(0, -gravity, 0)

		var hitGround = false

		val dragFac = (1 - drag * dt)

		val evalVel = ent.vel.clamp(terminal) + gravAcc * dt
		val terminalVel = (evalVel * dragFac) clamp terminal

		//TODO: add real start solid detection
//println("start trymkove")
		def tryMove(pos: Point3D, vec: Point3D): (Point3D, Point3D) = {
			if(vec ~~ Point3D.zero) return (pos, vec)

			val res = traceFunc(pos, vec)

			res.headOption.getOrElse(StartSolid) match {
				case NoHit(_) => (pos + vec, vec)
				case StartSolid =>
					log.warning(s"Start solid! $pos over $vec")
					//(ent.pos + Point3D(0, 0.01, 0) * dt, Point3D.zero)
					(ent.pos, Point3D.zero)
				/*case SurfaceHit(d, norm) if d == vec.length => //rare, but could cause errors
					if(norm == Point3D(0, 1, 0)) hitGround = true
					(pos, vec)*/
				case SurfaceHit(d, norm)/* if d > 0*/ =>
					if((norm * Point3D(0, 1, 0)) == 1) hitGround = true

					/*val vecNormal = vec.normal
					val remainingD = vec.length - d
					val remainingVec = vecNormal * remainingD
					val subVector = norm * (remainingVec * norm)
					//println(d, norm, pos + vecNormal * d)
					val newPos = pos + vecNormal * d
					val nextVec = remainingVec - subVector*/

					val remainingVec = vec.normal * (vec.length - d)
					val remainingNormalD = remainingVec * norm
					val subVector = norm * remainingNormalD
					val nextVec = remainingVec - subVector
					val nextPos = pos + vec.normal * d

					if(nextVec ~~ Point3D.zero) (nextPos, Point3D.zero)
					else {
						//println(s"vec ${vec} just clipped to ${nextVec} on normal $norm moved $d of ${vec.length}")
						tryMove(nextPos, nextVec)
					}
				/*case TraceHit(d, norm) => //if d == 0
					//println("zero d")
					if(norm == Point3D(0, 1, 0)) hitGround = true
					(pos, Point3D.zero)*/
				case x =>
					//println(x)
					(pos, Point3D.zero)
			}
		}

		val moveVec = terminalVel * dt

		//log.debug(s"tracing from ${ent.pos} along vel $moveVec")

		val (newPos, postMoveVec) = try tryMove(ent.pos, moveVec) catch {
			case x: FindChunkError =>
				log.error(s"failed tracing from ${ent.pos}: ${x.getMessage}")
				(ent.pos, Point3D.zero)
				//(ent.pos + terminalVel * dt, terminalVel * dt)
		}

		try if(traceFunc(newPos, Point3D(0, 0.01, 0)) contains StartSolid) {
			log.warning("possible start solid")
			ent.entityCopy(vel = Point3D(math.random - 0.5,
				math.random - 0.5, math.random - 0.5).normal)
			return
		} catch {
			case t: Throwable =>
				log.warning(t.getMessage)
		}

		require(postMoveVec.length <= moveVec.length)

		val movedVec = newPos - ent.pos

		//if(movedVec !~~ Point3D.zero) println(s"moved $movedVec to $newPos")

		//val newVec = newPos - ent.pos
		val newVec = postMoveVec

		val resVel = (newVec / dt)// * dragFac// + gravAcc * dt

		val arggggVel = if(resVel.length > terminalVel.length) {
			log.error("wbad res length!!! " + (resVel.length - terminalVel.length))
			terminalVel
		} else resVel

		val xzOnly = Point3D(arggggVel.x, 0, arggggVel.z)

		val finalVel = if(xzOnly.length < 0.1) Point3D(0, arggggVel.y, 0)
		else arggggVel

//if(newVec !~~ Point3D.zero) println(newVec)
		//require(resVel.length <= tryVel.length, s"$resVel > $tryVel")
		//val finalVel = if(resVel.length > terminal) resVel.normal * terminal else resVel


		//val newVel = finalVel + gravAcc * dt

		ent.entityCopy(pos = newPos, vel = finalVel, onGround = hitGround)
	}

	def handleChunks(x: Any)(implicit ec: ExecutionContext) = {
		val fut = Future(WorldClient.processChunks(x))

		fut onFailure { case t: Throwable =>
			log.error(t, "handleChunks fail!")
		}

		fut pipeTo self
	}


	val blockChange: Actor.Receive = {
		case a @ cpr.BlockChange(x, y, z, blockID, blockMeta) => try {
			val bpos = IPoint3D(x, y & 0xFF, z)
			val chunk = getChunk(bpos)

			chunk.setTyp(bpos.x.toInt - chunk.x * Chunk.dims.x,
				bpos.y.toInt - chunk.y * Chunk.dims.y,
				bpos.z.toInt - chunk.z * Chunk.dims.z, blockID.x)
			chunk.setMeta(bpos.x.toInt - chunk.x * Chunk.dims.x,
				bpos.y.toInt - chunk.y * Chunk.dims.y,
				bpos.z.toInt - chunk.z * Chunk.dims.z, blockMeta)
		} catch {
			//case t: Throwable => log.error(t, "Failed blockchange!")
			case x: FindChunkError =>
				log.error(s"failed blockchange: ${x.getMessage}")
				//schedule retry here!
		}
		case a @ cpr.MultiBlockChange(x, z, numRecord, _) =>
			a.records foreach { case cpr.BlockRecord(meta, id, y, rz, rx) =>
				val blockChange = cpr.BlockChange(
					x * Chunk.dims.x + rx, y.toByte,
					z * Chunk.dims.z + rz, VarInt(id), meta)
				self ! blockChange
			}
	}



	val worldClientReceive: PartialFunction[Any, Unit] = {
		case cpr.EntityRelativeMove(eid, dx, dy, dz) => updateEntity(eid) { e =>
			val vec = Point3D(dx, dy, dz) / 32.0
			e.entityCopy(pos = e.pos + vec)
		}
		case cpr.EntityVelocity(eid, dx, dy, dz) => updateEntity(eid) { e =>
			e.entityCopy(vel = Point3D(dx, dy, dz) * velocityFactor)
		}
		case cpr.EntityStatus(eid, status) =>
			updateEntity(eid)(_.entityCopy(status = status))
		case cpr.EntityLookandRelativeMove(eid, dx, dy,
				dz, yaw, pitch) => updateEntity(eid) { e =>
			val vec = Point3D(dx, dy, dz) / 32.0
			e.entityCopy(yaw = yaw, pitch = pitch, pos = e.pos + vec)
		}
		case cpr.EntityLook(eid, yaw, pitch) => updateEntity(eid) { e =>
			e.entityCopy(yaw = yaw, pitch = pitch)
		}
		case cpr.EntityProperties(eid, props) => updateEntity(eid) { e =>
			e.entityCopy(props = props.map(x => x.key -> x).toMap)
		}
		case cpr.EntityTeleport(eid, x, y, z, yaw, pitch) => updateEntity(eid) { e =>
			val vec = Point3D(x, y, z) / 32.0
			e.entityCopy(yaw = yaw, pitch = pitch, pos = vec)
		}
		case cpr.EntityMetadata(eid, data) => updateEntity(eid) { e =>
			def update(meta: Seq[cpr.Metadata], updated: Entity): Entity = {
				if(meta.isEmpty) updated
				else (meta.head.key, updated) match {
					case (6, living: LivingEntity) => //health
						val health = meta.head.data.asInstanceOf[Float]
						living.livingCopy(health = health)
					case _ => updated
				}
			}

			update(data, e)
		}
		case cpr.EntityHeadLook(eid, angle) => updateEntity(eid) { case e: LivingEntity =>
			e.livingCopy(headAngle = angle)
		}
		case cpr.DestroyEntities(entIds) =>
			entIds.foreach(entities -= _)
		case x: cpr.EntityEquipment => //TODO: will need this later maybe
		case cpr.SpawnMob(eid, typ, x, y, z, pitch, headPitch, yaw, //TODO: FIXXX
				dx, dy, dz, metadata) =>
			val pos = Point3D(x, y, z) / 32.0
			val vel = Point3D(dx, dy, dz) * velocityFactor

			///new Entity(eid, )

			val ent = Mob(eid.x, typ, pos, vel, yaw, pitch, headAngle = headPitch)

			entities += eid.x -> ent
		case cpr.SpawnObject(VarInt(eid), typ, x, y, z, pitch, yaw, metaData, dx, dy, dz) =>
			val pos = Point3D(x, y, z) / 32.0
			val vel = Point3D(dx, dy, dz) * velocityFactor

			///new Entity(eid, )

			val ent = ObjectEntity(eid, typ, pos, vel, yaw, pitch)

			entities += eid -> ent
		case cpr.SpawnPlayer(VarInt(eid), uuid, name, x, y, z, yaw, pitch, item, mdata) =>
			val pos = Point3D(x, y, z) / 32.0
			val ent = Player(eid, pos = pos, yaw = yaw, pitch = pitch)

			entities += eid -> ent

			players += name -> eid

			worldClientReceive(cpr.EntityMetadata(eid, mdata))

		case x: cpr.ChunkData =>
			handleChunks(x)
			loadingChunk
		case x: cpr.MapChunkBulk =>
			handleChunks(x)
			loadingChunk

		case AddChunks(x) =>
			chunkLoaded
			chunks ++= x.map(a => a.pos -> a)
		case RemoveChunks(x) =>
			chunkLoaded
			chunks --= x

		case x: cpr.Animation =>
		case x: cpr.Effect =>
		case x: cpr.AttachEntity =>
	}
}
