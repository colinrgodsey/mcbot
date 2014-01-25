package com.colingodsey.mcbot.client

import com.infomatiq.jsi.rtree.RTree
import com.colingodsey.logos.collections.Vec3D
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
import com.colingodsey.collections.{VecN, PathFinding}
import com.colingodsey.ai.{BoltzmannSelector, QLPolicyMaker, QLearning}

object WaypointManager extends Protocol {
	case class Connection(destId: Int, distance: Double, weights: Map[String, Double] = Map()) {
		def weight(str: String) = weights.getOrElse(str, 0.0)
	}

	case class Waypoint(id: Int, pos: Vec3D,
			connections: Map[Int, Connection] = Map()) {
		val rect = new Rectangle(pos.x.toFloat, pos.z.toFloat, 1, 1)

		def connection(x: Int) =
			connections.get(x).getOrElse(Connection(x, 0))

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
		implicit val pointCodec = codecFrom3(Vec3D.apply)
		implicit val connCodec = codecFrom3(Connection.apply)
		implicit val wpCodec = codecFrom3(Waypoint.apply)

		val codec = codecFrom1(WaypointSnapshot.apply)
	}
	case class WaypointSnapshot(waypoints: Seq[Waypoint]) extends Packet {
		def comp = WaypointSnapshot.asInstanceOf[PacketCompanion[this.type]]
	}

	val packets = Set[PacketCompanion[_]](WaypointSnapshot)
}

trait WaypointManager {
	import WaypointManager._

	def getShortPath(from: Vec3D, to: Vec3D): Seq[Vec3D]
	def log: LoggingAdapter
	implicit def ec: ExecutionContext

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

	class QLearner(desireMap: Map[String, Double])
			extends QLPolicyMaker[WaypointManager.WaypointTransition, VecN]
			with QLearning[WaypointManager.WaypointTransition, VecN] {
		def qLearning = this
		def selector = BoltzmannSelector.default

		val desireVector = VecN(desireMap)
		val gamma: Double = 0.8
		val alphaScale: Double = 1
		val initialValue: Double = 0

		def transFrom(trans: WaypointTransition): Set[WaypointTransition] = {
			val fromId = trans.destId
			waypoints(fromId).connections.iterator.map { case (id, _) =>
				WaypointTransition(fromId, id)
			}.toSet
		}

		def qValue(transition: WaypointTransition): Double = {
			val from = waypoints(transition.fromId)
			val to = waypoints(transition.destId)

			val weights = desireMap map { case (prop, w) =>
				from.connection(to.id).weight(prop) * w
			}

			weights
		}
	}

	def finder(dest: Vec3D) = new PathFinding[Waypoint, Connection] {
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

	def loadWaypointSnapshot(snapshot: WaypointSnapshot) =
		snapshot.waypoints.foreach(addWaypoint)

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

	def connectWaypoints(fromId: Int, toId: Int) {
		val from = waypoints(fromId)
		val to = waypoints(toId)

		if(from.connections.get(toId) == None) {
			val path = getShortPath(from.pos, to.pos)

			if(path.isEmpty) log.warning("Bad connect!")
			else {
				val conn = toId -> Connection(toId, path.length)
				addWaypoint(from.copy(connections = from.connections + conn))
				log.info("New connection!")
			}
		}
	}

	def disconnectWaypoints(fromId: Int, toId: Int) {
		val from = waypoints(fromId)

		log.info("Removed connection!")

		from.copy(connections = from.connections - toId)
	}

	def getNearWaypoints(pos: Vec3D,
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
	def reinforce(fromId: Int, toId: Int, props: Set[String]) = {
		val from = waypoints(fromId)
		val to = waypoints(toId)

		from.connections.get(to.id) match {
			case Some(conn) =>
				conn.weights ++ props.map { prop =>
					val old = conn.weights.getOrElse(prop, 0.0)

				}
			case None =>
		}
	}


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
