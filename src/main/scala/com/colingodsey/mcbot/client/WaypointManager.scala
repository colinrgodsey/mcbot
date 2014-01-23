package com.colingodsey.mcbot.client

import com.infomatiq.jsi.rtree.RTree
import com.colingodsey.logos.collections.Point3D
import com.infomatiq.jsi.{Point, Rectangle}
import gnu.trove.TIntProcedure
import scala.collection.immutable.VectorBuilder

case class Connection(destId: Int, distance: Double, weights: Map[Any, Double] = Map())

case class Waypoint(id: Int, pos: Point3D,
		connections: Map[Int, Connection] = Map()) {
	val rect = new Rectangle(pos.x.toFloat, pos.z.toFloat, 1, 1)
}

case class WaypointSnapshot(
		ids: Set[Int],
		positions: Map[Int, (Double, Double, Double)]
)

trait WaypointManager {
	private val rTree = {
		val t = new RTree

		t.init(null)

		t
	}

	private var _waypoints = Map[Int, Waypoint]()
	private var _nextWaypointId = 1

	def getPath(from: Point3D, to: Point3D): Seq[Point3D]

	def finder(dest: Point3D) = new PathFinding[Waypoint, Connection] {
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


	def makeWaypointSnapshot = {
		WaypointSnapshot(
			_waypoints.keySet,
			_waypoints.values.map { w =>
				w.id -> (w.pos.x, w.pos.y, w.pos.z)
			}.toMap
		)
	}

	def loadWaypointSnapshot(snapshot: WaypointSnapshot) = {
		snapshot.positions foreach { case (id, (x, y, z)) =>
			addWaypoint(Waypoint(id, Point3D(x, y, z)))
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
			val path = getPath(from.pos, to.posw)

			if(path.isEmpty) println("Bad connect!")
			else {
				val conn = toId -> Connection(toId, path.length)
				addWaypoint(from.copy(connections = from.connections + conn))
				println("New connection!")
			}
		}
	}

	def getNearWaypoints(pos: Point3D,
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

trait WaypointBot extends WaypointManager { bot: BotClient =>
	def waypointMinDistance = 10.0

	var lastWaypoint: Option[Waypoint] = None

	def getPath(from: Point3D, to: Point3D): Seq[Point3D] = {
		val floorTargetBlock = getBlock(to + Point3D(0, -1, 0))
		val endTargetBlock = getBlock(to)
		val targetBlock = if(floorTargetBlock.btyp.isPassable) floorTargetBlock
		else endTargetBlock

		val floorStartBlock = getBlock(from + Point3D(0, -1, 0))
		val endStartBlock = getBlock(from)
		val startBlock = if(floorStartBlock.btyp.isPassable) floorStartBlock
		else endStartBlock

		val finder = new PathFinder(targetBlock, 256)

		finder.pathFrom(startBlock, targetBlock, 4000).toSeq.flatten
	}

	def checkWaypoints {
		val closest = getNearWaypoints(bot.selfEnt.pos, waypointMinDistance)

		closest.headOption match {
			case None if bot.selfEnt.onGround =>
				val w = new Waypoint(nextWaypointId, bot.footBlockPos)

				val closer = getNearWaypoints(bot.selfEnt.pos, waypointMinDistance * 2)

				bot.log.info("Adding waypoint " + w)
				addWaypoint(w)

				if(lastWaypoint.isDefined)
					connectWaypoints(lastWaypoint.get.id, w.id)

				lastWaypoint = Some(w)
			case a @ Some(x) if a != lastWaypoint && lastWaypoint.isDefined =>

				connectWaypoints(lastWaypoint.get.id, x.id)
				connectWaypoints(x.id, lastWaypoint.get.id)

				lastWaypoint = Some(x)
			case Some(x) => lastWaypoint = Some(x)
			case _ =>
		}
	}
}