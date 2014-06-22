package com.colingodsey.mcbot.world

import com.colingodsey.logos.collections.{IVec3, Vec3}
import com.colingodsey.mcbot.protocol
import com.colingodsey.mcbot.protocol._
import com.colingodsey.mcbot.protocol.{ClientProtocol => cpr, ServerProtocol => spr, _}
import java.util.zip.{Inflater, Deflater}
import scala.collection.immutable.VectorBuilder
import akka.actor._
import akka.pattern._
import akka.event.LoggingAdapter
import scala.concurrent.{ExecutionContext, Future, blocking}

case class FindChunkError(x: Int, y: Int, z: Int, point: IVec3) extends Exception(
	s"chunk not found: $point for point $x, $y, $z")

//thread safe view of the mutable world
trait WorldView {
	private implicit def wv: WorldView = this

	def entities: Map[Int, Entity]
	def chunks: Map[IVec3, Chunk]
	def players: Map[String, Int]

	def getChunkAt(pos: IVec3): Chunk = chunks.getOrElse(pos, Chunk(pos.x, pos.y, pos.z))
		//throw FindChunkError(pos.x, pos.y, pos.z, pos))

	def getBlock(pos: Vec3): Block = Block(pos)

	def takeBlockDown(vec: Vec3): Block =
		takeBlockDown(getBlock(vec))

	def takeBlockDown(block: Block): Block = {
		var ptr = block.pos.toVec3

		//if(!block.isPassable) sys.error("block start solid!")
		if(!block.isPassable) return block

		while(getBlock(ptr).isPassable && ptr.y > 0) {
			ptr -= Vec3(0, 1, 0)
		}

		ptr += Vec3(0, 1, 0)

		getBlock(ptr)
	}

	def takeBlockDownWater(vec: Vec3): Block =
		takeBlockDownWater(getBlock(vec))

	def takeBlockDownWater(block: Block): Block = {
		var ptr = block.pos.toVec3

		//if(!block.isPassable) sys.error("block start solid!")
		if(!block.isPassable) return block
		if(block.btyp.isWater) return block

		while(getBlock(ptr).isPassable && ptr.y > 0 &&
				!getBlock(ptr).btyp.isWater) {
			ptr -= Vec3(0, 1, 0)
		}

		ptr += Vec3(0, 1, 0)

		val bl = getBlock(ptr)

		if(!bl.btyp.isWater && bl.below.btyp.isWater) bl.below
		else bl
	}

	def isPassable(bl: Block) = bl.isPassable
}

trait World { wv: WorldView =>
	def log: LoggingAdapter

	var entities = Map[Int, Entity]()
	@volatile var chunks = Map[IVec3, Chunk]()
	var players = Map[String, Int]()
	var isRaining = false

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
	case class RemoveChunks(poss: Set[IVec3])
	case class AddChunks(chunks: Set[Chunk])

	//the below can be run async!
	def processChunks(implicit wv: WorldView): Function[Any, Any] = blocking {
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

	implicit def wv: WorldView = this

	def self: ActorRef

	implicit def ec: ExecutionContext

	//behaviors
	def loadingChunk()
	def chunkLoaded()

	//TODO: for half blocks, just offset final position  with block depth
	def move(eid: Int, dt: Double, body: Body, isWater: Boolean): Boolean = {
		var retVal = true
		if(dt > epsilon) updateEntity(eid) { ent =>
			import CollisionDetection._

			require(dt > epsilon)

			val traceFunc = body match {
				//case x: SphereBody => traceBody(x, _: Point3D, _: Point3D)
				case x: BoxBody =>
					//log.warning("box bodys dont work great yet...")
					traceBody(x, _: Vec3, _: Vec3)
			}

			//println(ent.pos, ent.onGround, "ENT")

			val (gravity, terminal0, drag0) = ent match {
				case _: LivingEntity => (32, 78.4, 0.4)
				//case Minecart(cart) =>
				case _ => (16, 39.2, 0.4)
			}

			val terminal = if(isWater) terminal0 / 2 else terminal0
			val drag = if(isWater) drag0 * 1.3
			else if(ent.onGround) drag0 * 1.1
			else drag0

			val gravAcc = if(isWater) Vec3(0, -gravity / 20, 0)
			else Vec3(0, -gravity, 0)

			var hitGround = false

			val dragFac = (1 - drag * dt)

			val evalVel = ent.vel.clamp(terminal) + gravAcc * dt
			val terminalVel = (evalVel * dragFac) clamp terminal

			//TODO: add real start solid detection
			//println("start trymkove")
			def tryMove(pos: Vec3, vec: Vec3): Option[(Vec3, Vec3)] = {
				if(vec ~~ Vec3.zero) return None

				val res = traceFunc(pos, vec)

				res.headOption.getOrElse(StartSolid) match {
					case NoHit(_) => Some(pos + vec, vec)
					case StartSolid =>
						log.warning(s"Start solid! $pos over $vec")
						//(ent.pos + Point3D(0, 0.01, 0) * dt, Point3D.zero)
						//(ent.pos, Vec3D.zero)

						retVal = true

						val minY = body match {
							case x: BoxBody => x.points.map(_.y).min
							case _ => 0
						}

						val off = Vec3(0, minY - 0.5 + 0.1, 0)

						val blPos = Block.halfBlockVec + off + getBlock(ent.pos).pos

						//None
						Some(blPos, Vec3.zero)
					case SurfaceHit(d, norm)/* if d > 0*/ =>
						if((norm * Vec3(0, 1, 0)) == 1) hitGround = true

						require((norm * vec) < 0)

						val remainingVec = vec.normal * (vec.length - d)
						val remainingNormalD = remainingVec * norm
						val subVector = norm * remainingNormalD
						val nextVec = remainingVec - subVector
						val nextPos = pos + vec.normal * d

						if(nextVec ~~ Vec3.zero || nextVec.length <= 0.01) Some(nextPos, Vec3.zero)
						else {
							//println(s"vec ${vec} just clipped to ${nextVec} on normal $norm moved $d of ${vec.length}")
							tryMove(nextPos, nextVec)
						}
					case x =>
						//println(x)
						None
				}
			}

			val moveVec = terminalVel * dt

			//log.debug(s"tracing from ${ent.pos} along vel $moveVec")

			val moveRes = try tryMove(ent.pos, moveVec) catch {
				case x: FindChunkError =>
					log.error(s"failed tracing from ${ent.pos}: ${x.getMessage}")
					None
				//(ent.pos + terminalVel * dt, terminalVel * dt)
			}

			if(!moveRes.isDefined) {
				log.warning("Failed move!")
				return false
			}

			val Some((newPos, postMoveVec)) = moveRes

			val failed = try if(traceFunc(newPos, Vec3(0.1, 0.1, 0.1)) contains StartSolid) {
				log.warning("possible start solid")
				ent.entityCopy(vel = Vec3(math.random - 0.5,
					math.random - 0.5, math.random - 0.5).normal)
				true
			} else false catch {
				case t: Throwable =>
					log.error(t, "traceFunc fail!")
				true
			}

			if(failed) ent
			else {
				require(postMoveVec.length <= moveVec.length)

				val newVec = postMoveVec

				val resVel = (newVec / dt)// * dragFac// + gravAcc * dt

				val arggggVel = if(resVel.length > terminalVel.length) {
					//log.error("wbad res length!!! " + (resVel.length - terminalVel.length))
					terminalVel
				} else resVel

				val xzOnly = Vec3(arggggVel.x, 0, arggggVel.z)

				val finalVel = if(xzOnly.length < 0.1) Vec3(0, arggggVel.y, 0)
				else arggggVel

				ent.entityCopy(pos = newPos, vel = finalVel, onGround = hitGround)
			}
		}

		retVal
	}

	def handleChunks(x: Any) = {
		val fut = Future(WorldClient.processChunks.apply(x))

		fut onFailure { case t: Throwable =>
			log.error(t, "handleChunks fail!")
		}

		fut pipeTo self
	}


	val blockChange: Actor.Receive = {
		case a @ cpr.BlockChange(x, y, z, blockID, blockMeta) => try {
			val bpos = IVec3(x, y & 0xFF, z)
			val chunk = getChunkAt(bpos)

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
			val vec = Vec3(dx, dy, dz) / 32.0
			e.entityCopy(pos = e.pos + vec)
		}
		case cpr.EntityVelocity(eid, dx, dy, dz) => updateEntity(eid) { e =>
			e.entityCopy(vel = Vec3(dx, dy, dz) * velocityFactor)
		}
		case cpr.EntityStatus(eid, status) =>
			updateEntity(eid)(_.entityCopy(status = status))
		case cpr.EntityLookandRelativeMove(eid, dx, dy,
				dz, yaw, pitch) => updateEntity(eid) { e =>
			val vec = Vec3(dx, dy, dz) / 32.0
			e.entityCopy(yaw = yaw, pitch = pitch, pos = e.pos + vec)
		}
		case cpr.EntityLook(eid, yaw, pitch) => updateEntity(eid) { e =>
			e.entityCopy(yaw = yaw, pitch = pitch)
		}
		case cpr.EntityProperties(eid, props) => updateEntity(eid) { e =>
			e.entityCopy(props = props.map(x => x.key -> x).toMap)
		}
		case cpr.EntityTeleport(eid, x, y, z, yaw, pitch) => updateEntity(eid) { e =>
			val vec = Vec3(x, y, z) / 32.0
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
			val pos = Vec3(x, y, z) / 32.0
			val vel = Vec3(dx, dy, dz) * velocityFactor

			///new Entity(eid, )

			val ent = Mob(eid.x, typ, pos, vel, yaw, pitch, headAngle = headPitch)

			entities += eid.x -> ent
		case cpr.SpawnObject(VarInt(eid), typ, x, y, z, pitch, yaw, metaData, dx, dy, dz) =>
			val pos = Vec3(x, y, z) / 32.0
			val vel = Vec3(dx, dy, dz) * velocityFactor

			///new Entity(eid, )

			val ent = ObjectEntity(eid, typ, pos, vel, yaw, pitch)

			entities += eid -> ent
		case cpr.SpawnPlayer(VarInt(eid), uuid, name, x, y, z, yaw, pitch, item, mdata) =>
			val pos = Vec3(x, y, z) / 32.0
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

		case cpr.ChangeGameState(reason, value) => reason match {
			case 1 => //end raining
				isRaining = false
			case 2 => //start raining
				isRaining = true
			case _ =>
		}

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
