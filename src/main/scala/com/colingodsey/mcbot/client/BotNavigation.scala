package com.colingodsey.mcbot.client

import com.colingodsey.logos.collections.Vec3
import com.colingodsey.mcbot.world._
import scala.concurrent.duration.Deadline
import scala.concurrent.{Future, ExecutionContext, blocking}
import akka.event.LoggingAdapter
import scala.util.Failure
import akka.actor.{ActorLogging, Actor, ActorRef}
import akka.pattern._
import scala.util.Failure
import scala.Some
import com.colingodsey.mcbot.world.Player
import com.colingodsey.collections.{VecN, MapVector}
import com.colingodsey.mcbot.client.WorldTile.TileState

object BotNavigation {
	case class PathFound(path: Seq[Vec3])
	case class MoveFound(move: Option[Vec3])
}

trait BotMovement extends BotClientView with BotClientControl
		with CollisionDetection with ActorLogging with Actor {
	implicit val worldView: WorldView
	import worldView._

	var curPath: Stream[Block] = Stream()
	var lastPathNodePoint = Vec3.zero
	var lastPathTime = 0.0

	private def curTime = System.currentTimeMillis / 1000.0

	def pathPhysicsTick(dt: Double) = try {
		val oldPath = curPath

		if((curTime - lastPathTime) > 5) {
			log.info("Path timed out")
			lastPathTime = curTime
			curPath = Stream.empty
		}

		//find closest next step
		if(!curPath.isEmpty) {
			val nextStep = curPath.head.center
			val vec = nextStep - footBlockPos
			val goingUp = vec.y > 0

			val closer = curPath.take(3).zipWithIndex.sortBy { case (p, idx) =>
				val dy = footBlock.pos.y - p.y
				val dv = p.center - footBlockPos

				val abD = Vec3(selfEnt.vel.x, 0, selfEnt.vel.z)

				val dirFac = if(abD.length > 0) {
					dv * abD.normal
				} else 0

				if(dy > 0 && dy <= 2)
					Vec3(dv.x, 0, dv.z).length
				else if(p.y.toInt != footBlock.pos.y) 100000
				else if(dirFac <= 0) dv.length * 100
				//else dv.length// * dirFac
				else dv.length / dirFac
			}

			val closestIdx = closer.headOption.map(_._2).getOrElse(0)

			/*if(!closer.isEmpty && closestIdx >= 1 && !goingUp) {
				val clVec = curPath(closestIdx - 1) - footBlockPos + Block.halfBlockVec
				if(clVec.length < 2) {
					curPath = curPath.drop(closestIdx)
					lastPathTime = curTime
				}
			}*/
		}

		val minLength = if(footBlock.btyp.isWater) 0.7 else 0.2

		//drop head node if we're close
		if(!curPath.isEmpty) {
			val nextStep = curPath.head.center
			val vec = nextStep - footBlockPos
			val goingUp = vec.y > 0

			//CollisionDetection
			if(vec.length < minLength) {
				lastPathNodePoint = curPath.head.center
				curPath = curPath.tail
				lastPathTime = curTime
				//if(!curPath.isEmpty) say("Next stop, " + curPath.headOption)
				/*if(curPath.isEmpty) */setDirection(Vec3.zero)
				//log.info("visited path node")
			} else if(vec.length > 2 && math.abs(vec.y) < epsilon) {
				setDirection(Vec3.zero)
				randomPushSelf()
				curPath = Stream()
				lastPathTime = curTime
				log.info("ditching path!")
			}
		}

		//if(footBlock.btyp.isWater) jump()

		//if we still have a path
		if(!curPath.isEmpty) {
			val nextStep = curPath.head

			//get really close to the center of our block
			/*if(nextStep.y != footPos.y && dv.length > 0.18 && selfEnt.onGround) {
				direction = dv

			} else */{
				if(nextStep.y > footPos.y) jump()

				//make round movements
				val dirs = for {
					i <- 0 until 3
					if i < curPath.length
					step = curPath(i).center
					dir0 = step - footBlockPos
					dir = Vec3(dir0.x, 0, dir0.z)
					//dir = dir0
					len = dir.length / math.pow(i + 1, 6)
					if dir.length > 0 && dir0.length > 0
				} yield dir0.normal * len

				//val dir = if(dirs.isEmpty) Vec3.zero else dirs.reduce(_ + _)
				val dir = nextStep.center - footBlockPos

				val centerVec0 = lastPathNodePoint - footBlockPos
				//val centerVec0 = footBlock.center - footBlockPos
				val centerVec = Vec3(centerVec0.x, 0, centerVec0.z)

				if(centerVec.length > 0 && centerVec0.y == 0 &&
						selfEnt.vel.length < 0.2 && selfEnt.onGround) {
					addVel(centerVec.normal * 0.02 * dt)
					//direction = Vec3.zero
				}

				//val centerAddDir2 = footBlockPos - (Block.halfBlockVec + footBlock.pos)

				setDirection(dir.normal * 40)// + centerAddDir + centerAddDir2 * 10
			}
		}

		//ignore directly above blocks
		if(!curPath.isEmpty && !footBlock.btyp.isWater) {
			if(curPath.head.below.isPassable) curPath = curPath.tail
		}

		//if(curPath.isEmpty && footBlock.btyp.isWater) direction += Vec3(0, 1, 0)

		if(curPath.isEmpty && !oldPath.isEmpty) resetGoal()
	} catch {
		case x: FindChunkError =>
	}
}

trait BotPathing extends BotClientView {
	implicit val worldView: WorldView
	import worldView._

	def shortPathLength = 700
	def defaultMaxPathLength = 35

	//always need a to, atleast for heuristics
	def getPath(start: Block, to: Block, maxIter: Int = shortPathLength,
			maxLength: Int = defaultMaxPathLength)
			(cond: Block => Boolean = _ == to): Seq[Block] = blocking {
		val finder = new BlockPathFinder(worldView, to, maxLength)
		val paths = finder pathsFrom start

		val iter = paths.iterator
		var n = 0
		//val res = new VectorBuilder[Seq[Move]]

		if(start == to) return Nil

		while(iter.hasNext && n < maxIter) {
			val (state, moves) = iter.next

			if(cond(state)) {
				return moves.reverse
			}

			n += 1
		}

		Nil
	}

	def pathTo(from: Vec3, to: TileState): Seq[Block] = {
		val startBlock = takeBlockDownWater(Block(from))
		val endBlock = getBlock(to.pos)

		getPath(startBlock, endBlock, 500)(bl => to.contains(bl.center) > 0)
	}

	def getShortPath(from: Vec3, to: Vec3): Seq[Block] = blocking {
		try {
			val endTargetBlock = getBlock(to)
			val endStartBlock = getBlock(from)

			if(!endTargetBlock.isPassable || !endStartBlock.isPassable) Nil
			else {
				val targetBlock = takeBlockDownWater(endTargetBlock)
				val startBlock = takeBlockDownWater(endStartBlock)

				getPath(startBlock, targetBlock, 700)()
			}
		} catch {
			case t: FindChunkError =>
				Nil
		}
	}


}
/*
trait BotNavigation extends WaypointManager with CollisionDetection with BotClientView {
	import BotNavigation._
	import WaypointManager._
	import BotClient._

	val worldView: WorldView
	import worldView._

	def selfId: Int

	def jump()
	def lookAt(vec: Vec3)
	def randomPushSelf()
	def addVel(vel: Vec3)

	def log: LoggingAdapter
	def self: ActorRef
	implicit def ec: ExecutionContext

	var lastWaypointId: Option[Int] = None
	var gettingPath = false
	var moveGoal: Option[Vec3] = None
	var curPath: Stream[Vec3] = Stream()
	var lastPathTime = 0.0
	var lastPathNodePoint = Vec3.zero
	var direction = Vec3.zero//(math.random, 0, math.random).normal
	var targetingEnt: Option[Int] = None
	var lastTransition: Option[WaypointTransition] = None
	var lastQ: Option[VecN] = None

	def maxPathLength: Int = 40

	def waypointMinDistance = 10.0

	def lastWaypoint = lastWaypointId map waypoints

	private def curTime = System.currentTimeMillis / 1000.0

	def getPath(goal: Vec3): Unit = if(!gettingPath) {
		log.info("getting path")
		gettingPath = true
		val fut = scala.concurrent.future {
			//TODO: if in water, just pick block closest to goal

			val targetBlock = takeBlockDownWater(getBlock(goal))

			val finder = new BlockPathFinder(worldView, targetBlock, maxPathLength)

			val dl = Deadline.now

			val startBlock = takeBlockDownWater(footBlock)

			if(!targetBlock.isPassable) Nil
			else blocking {
				val r = finder.pathFrom(startBlock, targetBlock, 4500)

				val elapsed = -dl.timeLeft.toSeconds
				if(elapsed > 1) log.info(s"Pathing took $elapsed seconds")

				r
			} match {
				case Some(path) =>
					var start = startBlock.pos.toVec3
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
				case Failure(t: FindChunkError) =>
					//log.error(t, "getPath failed")
				case Failure(t) =>
					log.error(t, "getPath failed")
				case _ => //log.info("Path finished")
			}
		}

		fut.map(PathFound) pipeTo self
	} else self ! PathFound(Nil)
//maxPathLength
	def getShortPath(from: Vec3, to: Vec3): Seq[Vec3] = blocking {
		try {
			val endTargetBlock = getBlock(to)
			val endStartBlock = getBlock(from)

			if(!endTargetBlock.isPassable || !endStartBlock.isPassable) Nil
			else {
				val targetBlock = takeBlockDownWater(endTargetBlock)
				val startBlock = takeBlockDownWater(endStartBlock)

				roughPathFrom(startBlock, targetBlock, 700).toSeq.flatten
			}
		} catch {
			case t: FindChunkError => Nil
		}
	}

	def getLongPath(from: Vec3, to: Vec3): Seq[Vec3] = blocking {
		try {
			val endTargetBlock = getBlock(to)
			val endStartBlock = getBlock(from)

			if(!endTargetBlock.isPassable || !endStartBlock.isPassable) Nil
			else {
				val targetBlock = takeBlockDownWater(endTargetBlock)
				val startBlock = takeBlockDownWater(endStartBlock)

				val finder = new BlockPathFinder(worldView, targetBlock, maxPathLength)

				//roughPathFrom(startBlock, targetBlock, 700).toSeq.flatten
				finder.pathFrom(startBlock, targetBlock, 80800).toSeq.flatten
			}
		} catch {
			case t: FindChunkError => Nil
		}
	}

	def roughPathFrom(start: Block, to: Block, of: Int = 1000): Option[Seq[Vec3]] = blocking {
		val finder = new BlockPathFinder(worldView, to, 35)
		val paths = finder.pathsFrom(start)

		val iter = paths.iterator
		var n = 0
		//val res = new VectorBuilder[Seq[Move]]

		while(iter.hasNext && n < of) {
			val (state, moves) = iter.next

			val vec = to.pos - state.pos

			if(state == to) {// || vec.length <= 3) {
				//res += moves
				//println(n, moves.length)
				return Some(moves.reverse)
			}

			n += 1
		}

		None
	}

	/*def visitWaypoint(id: Int) = {
		waypoints(id).updateProperty("visited", curTime)
		waypoints(id).updateProperty("nvisits",
			waypoints(id).property("nvisits") + 1)
	}

	def checkWaypoints(): Unit = try blocking {
		val closestA = getNearWaypoints(footBlockPos,
				waypointMinDistance * 2) filter { x =>
			getBlock(x.pos).isPassable
		}
		//filter out accessible ones
		val closest = closestA filter { wp =>
			lazy val p = getShortPath(selfEnt.pos, wp.pos)
			lazy val p2 = getShortPath(wp.pos, selfEnt.pos)

			val isSuperClose = (takeBlockDown(footBlockPos).pos -
					takeBlockDown(wp.pos).pos).length < 4

			//either super close, or has path smaller than 30
			//must be slightly smaller than max path search range
			isSuperClose || (!p.isEmpty && p.length < 13 &&
					!p2.isEmpty && p2.length < 13)
		}

		closest.headOption match {
			case None if selfEnt.onGround =>

				val newPath = lastWaypoint.toSeq.flatMap(x =>
					//getShortPath(x.pos, selfEnt.pos))
					getShortPath(selfEnt.pos, x.pos))

				val closer = getNearWaypoints(selfEnt.pos, waypointMinDistance * 2)

				if(!newPath.isEmpty &&
						lastWaypoint.map(_.connections.size).getOrElse(0) == 0) {
					log.info("ehhhhhh random waypoint!")
					/*if(lastWaypoint.isDefined) {
						removeWaypoint(lastWaypoint.get)
						randomPushSelf
					}*/
				}

				//if(!lastWaypoint.isDefined || newPath.isEmpty) {
				if(true && (selfEnt.onGround || footBlock.btyp.isWater)) {
					val posDelta = closestA.headOption.map(
						_.pos).getOrElse(Vec3.zero) - selfEnt.pos

					if(posDelta.length < 4) {
						log.info(s"bad new waypoint!! ${takeBlockDown(selfEnt.pos).pos} ${closestA.headOption.map(wp => takeBlockDown(wp.pos).pos)} ${closest.headOption}")
						//direction = Vec3.random
						randomPushSelf()
					} else {
						//val w = new Waypoint(nextWaypointId, selfEnt.pos)
						val w = new Waypoint(selfEnt.pos.hashCode, selfEnt.pos)
						log.info("Adding waypoint " + w)
						addWaypoint(w)

						if(lastWaypoint.isDefined) {
							connectWaypoints(lastWaypointId.get, w.id)
							connectWaypoints(w.id, lastWaypointId.get)
							lastTransition = Some(WaypointTransition(
								lastWaypointId.get, w.id))
							reinforce(lastTransition.get,
								MapVector("discover" -> 100.0),
								Set())

							reinforce(lastTransition.get.swap, VecN.zero,
								Set())
						}

						visitWaypoint(w.id)
						lastWaypointId = Some(w.id)
					}
				} //else lastWaypoint = None
			case Some(x) if Some(x.id) != lastWaypointId && lastWaypointId.isDefined =>
				connectWaypoints(lastWaypointId.get, x.id)
				connectWaypoints(x.id, lastWaypointId.get)
				visitWaypoint(x.id)
				lastTransition = Some(WaypointTransition(lastWaypointId.get, x.id))

				val from = lastWaypoint.get

				val dt = math.min(math.max(curTime - from.property("visited"), 1), 3600)
				val disc = qValue(lastTransition.get)("discover") / (dt)
				val water = qValue(lastTransition.get)("water") / 200
				val reinMap = VecN("discover" -> -disc, "water" -> -water)

				//im pretty sure the ignore has to be there...
				reinforce(lastTransition.get,
					reinMap, Set(lastTransition.get.fromId))

				reinforce(lastTransition.get.swap,
					VecN.zero, Set(lastTransition.get.destId))

				lastWaypointId = Some(x.id)
			case Some(x) =>
				visitWaypoint(x.id)
				lastWaypointId = Some(x.id)
			case None =>
				//lastWaypoint = None
		}


	} catch {
		case x: FindChunkError =>
			log.info("waypoint stop " + x)
	}*/



	def pathPhysicsTick(dt: Double) = try {
		val oldPath = curPath

		//find closest next step
		if(!curPath.isEmpty) {
			val nextStep = curPath.head + Block.halfBlockVec
			val vec = nextStep - footBlockPos
			val goingUp = vec.y > 0

			val closer = curPath.take(3).zipWithIndex.sortBy { case (p, idx) =>
				val dy = footBlock.pos.y - p.y
				val dv = p + Block.halfBlockVec - footBlockPos

				val abD = Vec3(selfEnt.vel.x, 0, selfEnt.vel.z)

				val dirFac = if(abD.length > 0) {
					dv * abD.normal
				} else 0

				if(dy > 0 && dy <= 2)
					Vec3(dv.x, 0, dv.z).length
				else if(p.y.toInt != footBlock.pos.y) 100000
				else if(dirFac <= 0) dv.length * 100
				//else dv.length// * dirFac
				else dv.length / dirFac
			}

			val closestIdx = closer.headOption.map(_._2).getOrElse(0)

			/*if(!closer.isEmpty && closestIdx >= 1 && !goingUp) {
				val clVec = curPath(closestIdx - 1) - footBlockPos + Block.halfBlockVec
				if(clVec.length < 2) {
					curPath = curPath.drop(closestIdx)
					lastPathTime = curTime
				}
			}*/
		}

		val minLength = if(footBlock.btyp.isWater) 0.7 else 0.2

		//drop head node if we're close
		if(!curPath.isEmpty) {
			val nextStep = curPath.head + Block.halfBlockVec
			val vec = nextStep - footBlockPos
			val goingUp = vec.y > 0

			//CollisionDetection
			if(vec.length < minLength) {
				lastPathNodePoint = curPath.head + Block.halfBlockVec
				curPath = curPath.tail
				lastPathTime = curTime
				//if(!curPath.isEmpty) say("Next stop, " + curPath.headOption)
				/*if(curPath.isEmpty) */direction = Vec3.zero
				//log.info("visited path node")
			} else if(vec.length > 2 && math.abs(vec.y) < epsilon) {
				direction = Vec3.zero
				randomPushSelf()
				curPath = Stream()
				lastPathTime = curTime
				log.info("ditching path!")
			}
		}

		//if(footBlock.btyp.isWater) jump()

		//if we still have a path
		if(!curPath.isEmpty) {
			val nextStep = curPath.head

			//get really close to the center of our block
			/*if(nextStep.y != footPos.y && dv.length > 0.18 && selfEnt.onGround) {
				direction = dv

			} else */{
				if(nextStep.y > footPos.y) jump()

				//make round movements
				val dirs = for {
					i <- 0 until 3
					if i < curPath.length
					step = curPath(i) + Block.halfBlockVec
					dir0 = step - footBlockPos
					dir = Vec3(dir0.x, 0, dir0.z)
					//dir = dir0
					len = dir.length / math.pow(i + 1, 6)
					if dir.length > 0 && dir0.length > 0
				} yield dir0.normal * len

				//val dir = if(dirs.isEmpty) Vec3.zero else dirs.reduce(_ + _)
				val dir = nextStep + Block.halfBlockVec - footBlockPos

				val centerVec0 = lastPathNodePoint - footBlockPos
				//val centerVec0 = footBlock.center - footBlockPos
				val centerVec = Vec3(centerVec0.x, 0, centerVec0.z)

				if(centerVec.length > 0 && centerVec0.y == 0 &&
						selfEnt.vel.length < 0.2 && selfEnt.onGround) {
					addVel(centerVec.normal * 0.02 * dt)
					//direction = Vec3.zero
				}

				//val centerAddDir2 = footBlockPos - (Block.halfBlockVec + footBlock.pos)

				direction = dir.normal * 40// + centerAddDir + centerAddDir2 * 10
			}
		}

		//ignore directly above blocks
		if(!curPath.isEmpty && !footBlock.btyp.isWater) {
			if(getBlock(curPath.head).below.isPassable) curPath = curPath.tail
		}

		//if(curPath.isEmpty && footBlock.btyp.isWater) direction += Vec3(0, 1, 0)

		if(curPath.isEmpty && !oldPath.isEmpty) moveGoal = None
	} catch {
		case x: FindChunkError =>
	}
/*
	var findingMove = false
	def findMove() {
		if(findingMove) return
		findingMove = true

		val target = entities(targetingEnt.get)
		val closestWps = getNearWaypoints(target.pos, maxNum = 6,
			radius = 300)

		_innerPollForGoal(closestWps).map(MoveFound) recover { case x: Throwable =>
			log.error(x, "Failed poll for goal!")
			MoveFound(None)
		} pipeTo self
	}

	def _innerPollForGoal(closestWps: Seq[Waypoint]) = Future(blocking {
		val target = entities(targetingEnt.get)
		val curWp = lastWaypoint.get

		//if closer to target then closest waypoint is to target

		log.info("Polling for closest waypoint to target")

		val targetClosest = closestWps.toStream.filter { wp =>
				try !getShortPath(wp.pos, target.pos).isEmpty catch {
					case x: FindChunkError => false
				}
			}

		targetClosest.headOption match {
			case Some(x) if x.id != curWp.id =>
				val waypointPath = wpPathFinder(target.pos).pathFrom(curWp, x, of = 2000).toSeq.flatten
				if(waypointPath.isEmpty) {
					log.info("No wp path to closest wp by entw")
					None
				} else {
					val nextWp = waypoints(waypointPath.head.destId)

					log.info("Tracking wp " + nextWp)
					Some(nextWp.pos)
				}
			//getPath(target.pos)
			case _ => None
		}
	})

	//pick a random place...
	def findNewRandomWp(deadend: Boolean = true): Unit = try blocking {
		val curWpId = lastWaypoint
		val nearWps = getNearWaypoints(selfEnt.pos, maxNum = 10) filter { x =>
			val vec = x.pos - selfEnt.pos
			//val trace = traceRay(selfEnt.pos, vec, footPos)
			Some(x.id) != curWpId// && trace.dist >= vec.length
		}

		val wpCenter = if(nearWps.isEmpty) selfEnt.pos
		else nearWps.foldLeft(Vec3.zero)(_ + _.pos) / nearWps.length

		//smaller is detracting
		def wpWeight(wp: Waypoint) = {
			val old = curTime - wp.property("visited")
			//young nodes are favorable (that sounds terrible)
			val timeWeight = math.max(20 - old, 20) * 0.5
			//tend towards newer. 1 visit a second
			/*val visitWeight = (curWp.property("nvisits") -
					wp.property("nvisits")) * 1.0*/

			timeWeight// + visitWeight
		}

		/*val wps = getNearWaypoints(selfEnt.pos, maxNum = 5) map { wp =>
			val old = curTime - wp.property("visited")
			val times = wp.property("nvisits")
			val dir = wp.pos - selfEnt.pos

			if(Some(wp.id) == lastWaypoint.map(_.id))
				Point3D.zero
			else if(old > 0) dir.normal / old * times
			else dir
		}*/

		//TODO: redo this with more specific properties added to the node
		//visiting often adds a 'familiar' property, etc.

		//cast out rays when you get to the most unfamilar node

		val hintDir = nearWps.map { wp =>
			val weight = 1//wpWeight(wp)
			val vec = wp.pos - selfEnt.pos

			if(vec.length > 0) vec.normal * weight else Vec3.zero
		}.foldLeft(Vec3.zero)(_ + _)

		val wpAvg = Vec3(hintDir.x, 0.01, hintDir.z).normal

		def getRandomVec = {
			val weightVec = lastTransition.map {
				case WaypointTransition(toid, fromid) =>
					val dwp = waypoints(toid).pos
					val wp = waypoints(fromid).pos

					(dwp - wp).normal
			}.getOrElse(Vec3.zero)

			val flatIshRandom = Vec3(math.random * 2 - 1, math.random * 1.5 - 0.75,
				math.random * 2 - 1).normal

			val vec = flatIshRandom * (math.random * 25 + 5) - hintDir * 15
			// weightVec * math.random * 10
			// - wpAvg.normal * 10 * math.random

			val pos = footBlockPos + Vec3(0, math.random * 2 + 0.25, 0)
			val hit = traceRay(pos, vec, footPos)

			hit match {
				case x if x.dist > 1.2 =>
					val hitPos = pos + vec.normal * (x.dist - 0.2)

					var bl = takeBlockDownWater(getBlock(hitPos))

					Some(bl.pos.toVec3 + Block.halfBlockVec)
				case _ => None
			}
		}

		def getCylinderPoints(sphereRad: Double, nPointPerCircle: Int, yCuts: Int) = {
			val minCircle = -yCuts
			val maxCircle = yCuts

			val nCircles = maxCircle - minCircle

			val thetaOffs = math.Pi * math.random

			for {
				i <- (0 until nCircles).toStream
				yOff = i + minCircle
				yFac = math.abs(yOff) + 1
				rad = sphereRad// / yFac
				nPoints = nPointPerCircle//(nPointPerCircle / yFac)
				j <- 0 until nPoints
				theta = thetaOffs + j * (math.Pi * 2) / nPoints
			} yield Vec3(math.cos(theta) * rad, yOff * 2,
				math.sin(theta) * rad)
		}

		val detractPoint = lastTransition.map(
			x => waypoints(x.fromId).pos).getOrElse(selfEnt.pos)

		val bigRad = waypointMinDistance.toInt * (1 + math.random * 1.3) + 3

		//val samplePoints = Vector.fill(100)(getRandomVec).flatten
		val samplePoints =
			getCylinderPoints(waypointMinDistance + 1, 8, 3) #:::
			getCylinderPoints(bigRad, 16, 3)

		val sampleBlocks = samplePoints.flatMap { x =>
			val bl = getBlock(x + footBlockPos + Vec3(0, math.random * 2, 0))

			if(bl.isPassable)
				Some(takeBlockDownWater(bl))
			else None
		}

		val connIds = (for {
			wp <- lastWaypoint.toSeq
			(otherId, conn) <- wp.connections
		} yield otherId).toSet

		//TODO: use mutable state to make a streaming set
		val blFiltered = sampleBlocks.toSet.toStream.filter { bl =>
			val p = getShortPath(footBlockPos, bl.pos)

			val tooCloseWps = getNearWaypoints(bl.pos,
					maxNum = 5, radius = waypointMinDistance) filter { wp =>
				//def path = getShortPath(wp.pos, bl.globalPos)

				val finder = wpPathFinder(wp.pos, 7)
				def wpPath = finder.pathFrom(lastWaypoint.getOrElse(wp), wp)

				//TODO: check WP route instead of connections
				if(Some(wp.id) == lastWaypointId) true
				else if(!lastWaypoint.isDefined) false
				/*else connIds(wp.id) &&
						path.length < (waypointMinDistance * 1)*/
				else !wpPath.isEmpty
			}

			!p.isEmpty && tooCloseWps.isEmpty
		}


		val possiblePoints: Stream[Vec3] = blFiltered.sortBy[Double] { bl =>
			val endPos = bl.pos.toVec3 + Block.halfBlockVec
			val vec = endPos - footBlockPos

			//val hintFac = hintDir * x
			//val unexploredWeight = (endPos - wpCenter).length

			val nearbyWps = getNearWaypoints(endPos,
				maxNum = 20, radius = waypointMinDistance / 2)

			val brandNewFac = if(nearbyWps.isEmpty) 600
			else 1//(nearbyWps.head.pos - selfEnt.pos).length

			//val nearWpFac = 1 / (nearbyWps.length + 1)
			//val closenessFac = nearbyWps.map(x => (x.pos - footBlockPos).length).sum
			val closenessFac = if(nearbyWps.isEmpty) 1.0
			else nearbyWps.map(x => (x.pos - footBlockPos).length).sum / (nearbyWps.length)
			//val closenessFac = (endPos - detractPoint).length

			val discoverFac = if(nearbyWps.isEmpty) 1.0
			else desire("discover") + 1.0

			//-1 * brandNewFac * closenessFac
			if(bl.btyp.isWater) -vec.length + 100 else -vec.length


			///*discoverFac * */brandNewFac * closenessFac * -vec.length
			// * hintFac * -1//x.length// - unexploredWeight * 0.5

		}.toStream.map(_.pos.toVec3)



		possiblePoints.headOption match {
			case Some(pos) if (pos - footBlockPos).length > 1.2 =>
				log.info(s"random target res ${possiblePoints.headOption}")
				moveGoal = Some(pos)
				lookAt(pos)
				getPath(moveGoal.get)
			case a =>
				log.info("failed to get random targeT! best "+ a)
				randomPushSelf()
				direction = Vec3.random + Vec3(0, 2, 0)
				if(lastTransition.isDefined && deadend) {
					reward(lastTransition.get, MapVector("deadend" -> 10.0))
				}
		}
	} catch {
		case x: FindChunkError =>
	}

	def selectWaypoint(): Boolean = {
		if(!lastWaypoint.isDefined || (!selfEnt.onGround &&
				!footBlock.btyp.isWater) || !curPath.isEmpty) return false

		val lastWp = lastWaypoint.get
		val trans = transFrom(lastWaypointId.get, Set(lastWaypointId.get))

		val curWpPath = getLongPath(footBlockPos, lastWp.pos)

		if(curWpPath.isEmpty && (footBlockPos - lastWp.pos).length > 4) {
			//lastWaypoint = None
			log.info("Bailing on current wp")
			false
		} else if(trans.isEmpty) {
			log.info("Failed WP select from " + lastWaypoint)
			false
		} else if(desire.length > 0) {
			val selTrans = policy(trans)
			/*val selTrans = selector.selectFrom(trans) { x =>
				val destWp = waypoints(x.destId)
				val age = curTime - destWp.property("visited") + 1
				qValue(x) * desire.normal * age
			}*/
			val sel = waypoints(selTrans.destId)
			val vec = selfEnt.pos - sel.pos
			val selQ = qValue(selTrans)

			if(vec.length < 5) false
			/*else if((lastWp.pos - footBlockPos).length > 6) {
				moveGoal = Some(lastWp.pos)
				log.info("centering around wp")
				true
			}*/ else {
				moveGoal = Some(sel.pos)
				lastQ = Some(selQ)

				val path = getLongPath(selfEnt.pos, sel.pos)

				getPath(sel.pos)

				if(path.length < 2) {
					//disconnectWaypoints(lastWaypointId.get, sel.id)

					log.info("bad wp sel!")
					moveGoal = None
					lastWaypointId = None
					lastTransition = None
					false
				} else {
					log.info("selected node w q-value " + qValue(selTrans))
					lookAt(sel.pos)

					true
				}
			}
		} else false
	}

	def checkGoalPath() {
		if(targetingEnt.isDefined) {
			val ent = entities(targetingEnt.get)
			if(!moveGoal.isDefined || math.random < 0.15) moveGoal = Some(ent.pos)
		}

		if(math.random < 0.01 && moveGoal.isDefined) {
			curPath = Stream()
			getPath(moveGoal.get)
			lastPathTime = curTime
			log.info("Randomly clearing path")
			return
		}

		if(math.random < 0.01 && false) {
			moveGoal = None
			curPath = Stream()
			lastPathTime = curTime
			log.info("Randomly clearing goal")
		}

		if(moveGoal.isDefined && curPath.isEmpty) getPath(moveGoal.get)
		else if(selfEnt.onGround) checkWaypoints()

		if(curPath.isEmpty && !targetingEnt.isDefined && !moveGoal.isDefined &&
				curPath.isEmpty && lastWaypoint.isDefined) {
			val wpQ = lastTransition.map(qValue).getOrElse(MapVector.zero)
			val selQ = (for {
				trans <- lastTransition
				wpid <- lastWaypointId
				transs = transFrom(wpid, Set(wpid))//transFrom(trans)
				if(!transs.isEmpty)
				if Some(trans.destId) != lastTransition.map(_.fromId)
				sel = policy(transs)
				q = qValue(sel)
			} yield q).getOrElse(wpQ)

			//if(desire("discover") > wpQ("discover") || math.random < 0.1) findNewRandomWp
			//println(selQ("discover"),  wpQ("discover"), desire("discover"))
println(selQ -> wpQ, lastTransition)
			val discovering = (desire("discover") > 10 &&
					wpQ("home") > 0.01)

			if(discovering && math.random < 0.05)
				findNewRandomWp(false)
			if(!moveGoal.isDefined && selQ("discover") <= wpQ("discover")
					&& math.random < 0.8 && discovering)
				findNewRandomWp()


			if(!moveGoal.isDefined && !selectWaypoint())
				findNewRandomWp()

		} else if(targetingEnt.isDefined && curPath.isEmpty
				&& lastWaypoint.isDefined && !moveGoal.isDefined) {
			findMove()
		}
	}

	def pathReceive: Actor.Receive = {
		case MoveFound(x) =>
			moveGoal = x
			findingMove = false
		case SaveWaypoints =>
			saveWaypoints()
		case PathFound(path) =>
			var startIdx = 0
			var closest = 10000.0
			/*path.zipWithIndex foreach { case (x, idx) =>
				val pos = x + Point3D(0.5, 0, 0.5)
				val vec = footPos - pos
				if(vec.length < closest && idx < 4) {
					closest = vec.length
					startIdx = idx
				}
			}*/

			/*if(moveGoal.isDefined && path.isEmpty) {
				val dir = moveGoal.get - selfEnt.pos

				direction = dir
			}*/

			if(startIdx > 0) println("SKIPPED " + startIdx)

			//TODO: below comment dissallows connections along an indentified path
			if(selfEnt.onGround/* && (curPath.isEmpty || targetingEnt.isDefined)*/) checkWaypoints()

			if(path.isEmpty) {
				moveGoal = None
				log.info("stopping. path done")
			} else {
				val oldCurPath = curPath
				curPath = path.toStream.drop(startIdx)
				//if(curPath != oldCurPath && !curPath.isEmpty) lastPathTime = curTime
			}

			if(!curPath.isEmpty) {
				println("curpath: " + curPath)
				println("dir " + direction)
			} else {
				randomPushSelf()
				log.info("no path!")
			}

			direction = Vec3(0, 0, 0)
		case PathTick if selfId != -1 && !dead && joined && !gettingPath => try {
			//lastCurPath = curPath

			if((curTime - lastPathTime) > 4 && !curPath.isEmpty) {
				val vec = curPath.headOption.map(
					x => (x - footBlock.pos).length).getOrElse(-1)
				log.info(s"path timeout. len $vec dir $direction vel ${selfEnt.vel}")
				randomPushSelf()
				direction = Vec3.random + Vec3(0, 2, 0)
				curPath = Stream()
				moveGoal = None
				lastPathTime = curTime
			}

			if(selfEnt.onGround || footBlock.btyp.isWater) checkGoalPath()
		} catch {
			case x: FindChunkError =>
		}
		case PathTick =>
	}*/
}*/
