package com.colingodsey.mcbot.client

import com.infomatiq.jsi.rtree.RTree
import com.colingodsey.logos.collections.Vec3
import com.infomatiq.jsi.{Point, Rectangle}
import gnu.trove.TIntProcedure
import scala.collection.immutable.VectorBuilder
import com.colingodsey.mcbot.protocol._
import java.io.{DataOutputStream, FileOutputStream, FileInputStream, File}
import scala.concurrent.{ExecutionContext, Future, blocking}
import scala.util.Failure
import akka.actor.ActorLogging
import akka.event.LoggingAdapter
import java.nio.file.{Paths, Path, StandardCopyOption, Files}
import com.colingodsey.mcbot.world.{CollisionDetection, FindChunkError}
import com.colingodsey.collections.{MapVector, VecN, PathFinding}
import com.colingodsey.ai.{BoltzmannSelector, QLPolicy, QLearning}

object WaypointManager extends Protocol {
	case class Connection(destId: Int, distance: Double, weights: Map[String, Double] = Map()) {
		def weight(str: String) = weights.getOrElse(str, 0.0)
	}

	case class Waypoint(id: Int, pos: Vec3,
			connections: Map[Int, Connection] = Map()) {
		val rect = new Rectangle(pos.x.toFloat, pos.z.toFloat, 1, 1)

		def connection(x: Int) =
			connections.get(x).getOrElse(Connection(x, 0))

		def connectsTo(otherId: Int) = connections.get(otherId).isDefined

		//hacky storin this in here...
		def property(name: String) =
			connections.get(id).flatMap(_.weights.get(name)).getOrElse(0.0)

		def updateProperty(name: String, value: Double) = {
			val conn = connection(id)
			val newConn = conn.copy(weights = conn.weights + (name -> value))

			copy(connections = connections + (id -> newConn))
		}
	}

	case class WaypointTransition(fromId: Int, destId: Int)

	implicit object WaypointSnapshot extends LocalPacketCompanion[WaypointSnapshot](0) {
		import LengthCodec.IntLengthCodec
		implicit val pointCodec = codecFrom3(Vec3.apply)
		implicit val connCodec = codecFrom3(Connection.apply)
		implicit val wpCodec = codecFrom3(Waypoint.apply)

		val codec = codecFrom1(WaypointSnapshot.apply)
	}
	case class WaypointSnapshot(waypoints: Seq[Waypoint]) extends Packet {
		def comp = WaypointSnapshot.asInstanceOf[PacketCompanion[this.type]]
	}

	val packets = Set[PacketCompanion[_]](WaypointSnapshot)
}

trait WaypointManager extends QLPolicy[WaypointManager.WaypointTransition, VecN] {
	import WaypointManager._

	def getShortPath(from: Vec3, to: Vec3): Seq[Vec3]
	def log: LoggingAdapter
	def desireMap: Map[String, Double]
	implicit def ec: ExecutionContext

	def desire = MapVector(desireMap)
	def γ: Double = 0.8
	def α0: Double = 0.2
	def selector = BoltzmannSelector.default
	val initialValue: VecN = MapVector.zero

	private val rTree = {
		val t = new RTree

		t.init(null)

		t
	}

	private var _waypoints = Map[Int, Waypoint]()
	private var _nextWaypointId = 1

	var savingWaypoints = false

	val waypointFile = new File("./waypoints.dat")
	val waypointSwapFile = new File("./waypoints.tmp.dat")

	def transFrom(wpId: Int): Set[WaypointTransition] = {
		val from = waypoints(wpId)

		waypoints(wpId).connections.iterator.flatMap { case (id, _) =>
			val to = waypoints(id)
			val path = getShortPath(from.pos, to.pos)

			if(path.isEmpty) None
			else Some(WaypointTransition(wpId, id))
		}.toSet
	}

	def transFrom(trans: WaypointTransition): Set[WaypointTransition] =
		transFrom(trans.destId)

	def qValue(transition: WaypointTransition): VecN = {
		val from = waypoints(transition.fromId)

		MapVector(from.connection(transition.destId).weights)
	}


	def finder(dest: Vec3) = new PathFinding[Waypoint, Connection] {
		def maxPathLength: Int = 256

		def legalNeighbors(state: Waypoint): Stream[(Waypoint, Connection)] = {
			val wps = state.connections.toStream map { case (id, conn) =>
				val w = waypoints(id)

				w -> conn
			}

			wps sortBy { case (wp, conn) =>
				(dest - wp.pos).length + conn.distance
			}
		}
	}

	def maxVerticalHeight = 256

	def waypoints = _waypoints
	def nextWaypointId = _nextWaypointId

	def makeWaypointSnapshot =
		WaypointSnapshot(waypoints.values.toSeq)

	def loadWaypointSnapshot(snapshot: WaypointSnapshot) = {
		snapshot.waypoints.foreach(addWaypoint)

		for {
			(id, wp) <- waypoints.iterator
			(toId, con) <- wp.connections
			if !waypoints.get(toId).isDefined
		} {
			log.error("Removing bad connection to " + toId)
			addWaypoint(wp.copy(connections = wp.connections - toId))
		}
	}

	def loadWaypoints() {
		if(!waypointFile.canRead) {
			log.info("No waypoint file!")
			return
		}

		val src = DataSource(new FileInputStream(waypointFile))

		try loadWaypointSnapshot(src.read[WaypointSnapshot](
			WaypointSnapshot.codec)) finally src.close
	}

	def saveWaypoints() {
		if(savingWaypoints) return
		savingWaypoints = true

		val dest = new DataDest {
			val stream = new DataOutputStream(
				new FileOutputStream(waypointSwapFile))
		}

		val snapshot = makeWaypointSnapshot

		Future(blocking(dest.write(snapshot)(WaypointSnapshot.codec))) onComplete {
			case Failure(t) =>
				log.error(t,  "failed to save waypoints!")
				dest.stream.close
				savingWaypoints = false
			case _ =>
				log.info("saved waypoints")

				Files.move(Paths.get(waypointSwapFile.toString),
					Paths.get(waypointFile.toString),
					StandardCopyOption.REPLACE_EXISTING)
				dest.stream.close
				savingWaypoints = false
		}
	}

	def addWaypoint(waypoint: Waypoint) = {
		removeWaypoint(waypoint.id)

		_waypoints += waypoint.id -> waypoint
		if(waypoint.id >= _nextWaypointId) _nextWaypointId = waypoint.id + 1

		rTree.add(waypoint.rect, waypoint.id)
	}

	def removeWaypoint(id: Int) = _waypoints.get(id) match {
		case Some(x) =>
			rTree.delete(x.rect, x.id)
			_waypoints -= id
			true
		case None => false
	}

	//reward the damn thing
	def reinforce(trans: WaypointTransition, reward: VecN) {
		val WaypointTransition(fromId, toId) = trans
		val from = waypoints(fromId)
		val to = waypoints(toId)

		if(!from.connectsTo(toId)) return

		val newQ = update(trans, reward)

		val conn = toId -> from.connection(toId).copy(
			weights = newQ.weights)

		log.info(s"Reinforcing $reward to $newQ")

		addWaypoint(from.copy(connections = from.connections + conn))
	}

	def connectWaypoints(fromId: Int, toId: Int) {
		val from = waypoints(fromId)
		val to = waypoints(toId)

		val trans = WaypointTransition(fromId, toId)

		if(from.connections.get(toId) == None) {
			val path = getShortPath(from.pos, to.pos)

			if(path.isEmpty) log.warning("Bad connect!")
			else {
				val conn = toId -> Connection(toId, path.length)
				addWaypoint(from.copy(connections = from.connections + conn))

				reinforce(trans, MapVector("discover" -> 100.0))

				log.info("New connection!")
			}
		} else {
			//TODO: is this right?
			val disc = qValue(trans)("discover") * 0.1
			reinforce(trans, MapVector("discover" -> -disc))
		}
	}

	def disconnectWaypoints(fromId: Int, toId: Int) {
		val from = waypoints(fromId)

		log.info("Removed connection!")

		from.copy(connections = from.connections - toId)
	}

	def getNearWaypoints(pos: Vec3,
			radius: Double = maxVerticalHeight, maxNum: Int = 20): Seq[Waypoint] = {
		var list = new VectorBuilder[Waypoint]()
		var n = 0
		val proc = new TIntProcedure {
			def execute(x: Int) = {
				val w = _waypoints(x)
				if((w.pos - pos).length < radius) {
					list += w
					n += 1
				}

				n < maxNum
			}
		}

		rTree.nearestN(new Point(pos.x.toFloat, pos.z.toFloat), proc,
			maxNum * 4, radius.toFloat)

		list.result.sortBy(w => (pos - w.pos).length)
	}

	//TODO: use qleanr
	/*def reinforce(fromId: Int, toId: Int, props: Set[String]) = {
		val from = waypoints(fromId)
		val to = waypoints(toId)

		from.connections.get(to.id) match {
			case Some(conn) =>
				conn.weights ++ props.map { prop =>
					val old = conn.weights.getOrElse(prop, 0.0)

				}
			case None =>
		}
	}*/


	//get set of waypoints in radius at least equal to the maxHeight / 2
	/*def getNearestWaypoint(pos: Point3D, maxRadius: Double = 200) = {
		var list = Seq[Waypoint]()

		val proc = new TIntProcedure {
			def execute(x: Int) = {
				val w = _waypoints(x)
				if((w.pos - pos).length < maxRadius) {
					list :+= w
					false
				} else true
			}
		}

		rTree.nearestN(new Point(pos.x.toFloat, pos.z.toFloat),
			proc, 100, maxRadius.toFloat)

		list.headOption
	}*/
}
