package com.colingodsey.mcbot.world

import com.colingodsey.logos.collections.{IPoint3D, Point3D}
import com.colingodsey.mcbot.protocol
import com.colingodsey.mcbot.protocol._
import com.colingodsey.mcbot.protocol.{ClientProtocol => cpr, ServerProtocol => spr, _}
import java.util.zip.{Inflater, Deflater}
import scala.collection.immutable.VectorBuilder
import akka.actor.ActorLogging
import akka.event.LoggingAdapter

case class FindChunkError(x: Int, y: Int, z: Int, point: IPoint3D) extends Exception(
	s"chunk not found: $point for point $x, $y, $z")

trait World {
	def log: LoggingAdapter

	def selfId: Int //remove this later

	var entities = Map[Int, Entity]()
	var chunks = Map[IPoint3D, Chunk]()

	def getChunk(x: Int, y: Int, z: Int): Chunk = {
		val point = IPoint3D(
			math.floor(x.toDouble / Chunk.dims.x).toInt,
			math.floor(y.toDouble / Chunk.dims.y).toInt,
			math.floor(z.toDouble / Chunk.dims.z).toInt)

		chunks.getOrElse(point,
			throw FindChunkError(x, y, z, point))
	}

	def getChunk(pos: Point3D): Chunk = getChunk(pos.x.toInt, pos.y.toInt, pos.z.toInt)
	def getChunk(pos: IPoint3D): Chunk = getChunk(pos.x, pos.y, pos.z)

	def getBlock(pos: Point3D): Block = {
		val chunk = getChunk(pos)

		chunk(pos.x.toInt - chunk.x * Chunk.dims.x,
			pos.y.toInt - chunk.y * Chunk.dims.y,
			pos.z.toInt - chunk.z * Chunk.dims.z)
	}

	//TODO: add extractors to convert entity types.... case Player(player) =>   will convert
	def updateEntity(id: Int)(f: Entity => Entity) {
		//val updated = f(entities.getOrElse(id, Entity(id)))
		entities.get(id) match {
			case Some(x) =>
				val updated = f(x)
				entities += id -> updated
				//if(x != updated && id == selfId) log.info("Player change: " + updated)

				/*if((updated.pos - x.pos).length > 1000 && id == selfId)
					sys.error("Tried to move reaallly far!")*/
				//if(id == selfId) println(entities(id).onGround)
			case None =>
				log.warning(s"Update for ent $id that doesnt exist yet")
		}
	}
}

trait WorldClient extends World with CollisionDetection {
	import CollisionDetection._

	val ticksPerSecond = 20
	val movementSpeedModifier = ticksPerSecond * 2.15
	val velocityFactor = 1.0 / 8000 * ticksPerSecond

	def move(eid: Int, dt: Double, body: Body): Unit = updateEntity(eid) { ent =>
		import CollisionDetection._

		val traceFunc = body match {
			case x: SphereBody => traceBody(x, _: Point3D, _: Point3D)
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

		val evalVel = ent.vel.clamp(terminal) + gravAcc * dt
		val terminalVel = evalVel clamp terminal

		//TODO: add real start solid detection
//println("start trymkove")
		def tryMove(pos: Point3D, vec: Point3D): (Point3D, Point3D) = {
			if(vec == Point3D.zero) return (pos, vec)

			val res = traceFunc(pos, vec)
			//println(res)
			res foreach {
				case SurfaceHit(_, norm) if norm == Point3D(0, 1, 0) =>
					hitGround = true
				case _ =>
			}
			val realHits = res//res.filter(!_.isInstanceOf[StartHit])
//println(realHits.head, vec)
			realHits.headOption.getOrElse(NoHit(vec.length)) match {
				case NoHit(_) => (pos + vec, vec)
				case StartSolid =>
					log.warning("Start solid!")
					//(ent.pos + Point3D(0, 0.01, 0) * dt, Point3D.zero)
					(ent.pos, Point3D.zero)
				/*case SurfaceHit(d, norm) if d == vec.length => //rare, but could cause errors
					if(norm == Point3D(0, 1, 0)) hitGround = true
					(pos, vec)*/
				case SurfaceHit(d, norm)/* if d > 0*/ =>
					if(norm == Point3D(0, 1, 0)) hitGround = true

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
				//(ent.pos, Point3D.zero)
				(ent.pos + terminalVel * dt, terminalVel * dt)
		}

		require(postMoveVec.length <= moveVec.length)

		val movedVec = newPos - ent.pos

		//if(movedVec !~~ Point3D.zero) println(s"moved $movedVec to $newPos")

		//val newVec = newPos - ent.pos
		val newVec = postMoveVec

		val dragFac = (1 - drag * dt)
		val resVel = (newVec / dt) * dragFac// + gravAcc * dt
//if(newVec !~~ Point3D.zero) println(newVec)
		//require(resVel.length <= tryVel.length, s"$resVel > $tryVel")
		//val finalVel = if(resVel.length > terminal) resVel.normal * terminal else resVel


		//val newVel = finalVel + gravAcc * dt

		ent.entityCopy(pos = newPos, vel = resVel, onGround = hitGround)
	}

	val worldClientReceive: PartialFunction[Any, Unit] = {
		case cpr.EntityRelativeMove(eid, dx, dy, dz) => updateEntity(eid) { e =>
			val vec = Point3D(dx, dy, dz)
			e.entityCopy(pos = e.pos + vec)
		}
		case cpr.EntityVelocity(eid, dx, dy, dz) => updateEntity(eid) { e =>
			e.entityCopy(vel = Point3D(dx, dy, dz) * velocityFactor)
		}
		case cpr.EntityStatus(eid, status) =>
			updateEntity(eid)(_.entityCopy(status = status))
		case cpr.EntityLookandRelativeMove(eid, dx, dy,
				dz, yaw, pitch) => updateEntity(eid) { e =>
			val vec = Point3D(dx, dy, dz)
			e.entityCopy(yaw = yaw, pitch = pitch, pos = e.pos + vec)
		}
		case cpr.EntityLook(eid, yaw, pitch) => updateEntity(eid) { e =>
			e.entityCopy(yaw = yaw, pitch = pitch)
		}
		case cpr.EntityProperties(eid, props) => updateEntity(eid) { e =>
			e.entityCopy(props = props.map(x => x.key -> x).toMap)
		}
		case cpr.EntityTeleport(eid, x, y, z, yaw, pitch) => updateEntity(eid) { e =>
			val vec = Point3D(x, y, z)
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
			val pos = Point3D(x, y, z)
			val vel = Point3D(dx, dy, dz) * velocityFactor

			///new Entity(eid, )

			val ent = Mob(eid.x, typ, pos, vel, yaw, pitch, headAngle = headPitch)

			entities += eid.x -> ent
		case cpr.SpawnObject(VarInt(eid), typ, x, y, z, pitch, yaw, metaData, dx, dy, dz) =>
			val pos = Point3D(x, y, z)
			val vel = Point3D(dx, dy, dz) * velocityFactor

			///new Entity(eid, )

			val ent = ObjectEntity(eid, typ, pos, vel, yaw, pitch)

			entities += eid -> ent
		case cpr.SpawnPlayer(VarInt(eid), uuid, name, x, y, z, yaw, pitch, item, mdata) =>
			val pos = Point3D(x, y, z)
			val ent = Player(eid, pos = pos, yaw = yaw, pitch = pitch)

			entities += eid -> ent

			worldClientReceive(cpr.EntityMetadata(eid, mdata))
		case cpr.ChunkData(chunkX, chunkZ, groundUp, bitmask, addBitmask, data) =>
			val decompdData = Chunk.inflateData(data.toArray)

			val cks = Chunk(chunkX, chunkZ, bitmask, addBitmask, groundUp, true, decompdData)

			if(groundUp && bitmask == 0) //empty column, remove
				chunks --= cks.map(_.pos)
			else chunks ++= cks.map(x => x.pos -> x)
		case cpr.MapChunkBulk(chunkColumnCount, skylight, compdData, metas) =>
			var idx = 0
			val data = Chunk.inflateData(compdData.toArray)

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
			chunks ++= cols.map(x => x.pos -> x)
		case cpr.BlockChange(x, y, z, blockID, blockMeta) => try {
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
		}
		case a @ cpr.MultiBlockChange(x, z, numRecord, _) =>
			a.records foreach { case cpr.BlockRecord(meta, id, y, rz, rx) =>
				val blockChange = cpr.BlockChange(
					x + rx, y.toByte, z + rz, VarInt(id), meta)
				worldClientReceive(blockChange)
			}


		case x: cpr.Animation =>
		case x: cpr.Effect =>
		case x: cpr.AttachEntity =>
	}
}
