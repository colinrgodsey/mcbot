package com.colingodsey.mcbot.client

import com.colingodsey.logos.collections.Point3D
import com.colingodsey.mcbot.world._
import scala.concurrent.duration.Deadline
import scala.concurrent.{ExecutionContext, blocking}
import akka.event.LoggingAdapter
import scala.util.Failure
import akka.actor.{Actor, ActorRef}
import akka.pattern._
import scala.util.Failure
import scala.Some
import com.colingodsey.mcbot.world.Player

object BotNavigation {
	case class PathFound(path: Seq[Point3D])
}

trait BotNavigation extends WaypointManager with CollisionDetection {
	import BotNavigation._
	import WaypointManager._
	import BotClient._

	val worldView: WorldView
	import worldView._

	def footPos: Point3D
	def footBlockPos: Point3D
	def footBlock: Block
	def selfEnt: Player

	def dead: Boolean
	def joined: Boolean

	def jump()
	def randomPushSelf()

	def log: LoggingAdapter
	def self: ActorRef
	implicit def ec: ExecutionContext

	var lastWaypoint: Option[Waypoint] = None
	var gettingPath = false
	var moveGoal: Option[Point3D] = None
	var curPath: Seq[Point3D] = Nil
	var direction = Point3D.zero//(math.random, 0, math.random).normal
	var targetingEnt: Option[Int] = None

	def maxPathLength: Int = 100

	def waypointMinDistance = 10.0

	def getPath(goal: Point3D): Unit = if(!gettingPath) {
		gettingPath = true
		val fut = scala.concurrent.future {
			val floorTargetBlock = getBlock(goal + Point3D(0, -1, 0))
			val endTargetBlock = getBlock(goal)

			val targetBlock = if(floorTargetBlock.btyp.isPassable) floorTargetBlock
			else endTargetBlock

			val finder = new BlockPathFinder(worldView, targetBlock, maxPathLength)

			val dl = Deadline.now

			val startBlock = footBlock

			if(!targetBlock.btyp.isPassable) Nil
			else blocking {
				val r = finder.pathFrom(startBlock, targetBlock, 1500)

				val elapsed = -dl.timeLeft.toSeconds
				if(elapsed > 1) log.info(s"Pathing took $elapsed seconds")

				r
			} match {
				case Some(path) =>
					val boff = Block.halfBlockVec
					var start = startBlock.globalPos.toPoint3D
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
				case _ => log.info("Path finished")
			}
		}

		fut.map(PathFound) pipeTo self
	} else self ! PathFound(Nil)

	def getShortPath(from: Point3D, to: Point3D): Seq[Point3D] = {
		val floorTargetBlock = getBlock(to + Point3D(0, -1, 0))
		val endTargetBlock = getBlock(to)
		val targetBlock = if(floorTargetBlock.btyp.isPassable) floorTargetBlock
		else endTargetBlock

		val floorStartBlock = getBlock(from + Point3D(0, -1, 0))
		val endStartBlock = getBlock(from)
		val startBlock = if(floorStartBlock.btyp.isPassable) floorStartBlock
		else endStartBlock

		val finder = new BlockPathFinder(worldView, targetBlock, 128)

		finder.pathFrom(startBlock, targetBlock, 900).toSeq.flatten
	}

	def checkWaypoints: Unit = try {
		val closest = getNearWaypoints(selfEnt.pos, waypointMinDistance)

		closest.headOption match {
			case None if selfEnt.onGround =>
				val w = new Waypoint(nextWaypointId, footBlockPos)

				val closer = getNearWaypoints(selfEnt.pos, waypointMinDistance * 2)

				log.info("Adding waypoint " + w)
				addWaypoint(w)

				if(lastWaypoint.isDefined)
					connectWaypoints(lastWaypoint.get.id, w.id)

			case a @ Some(x) if a != lastWaypoint && lastWaypoint.isDefined =>

				connectWaypoints(lastWaypoint.get.id, x.id)
				connectWaypoints(x.id, lastWaypoint.get.id)
			case _ =>
		}

		lastWaypoint = closest.headOption
	} catch {
		case x: FindChunkError =>
			log.info("waypoint stop " + x)
	}

	def pathPhysicsTick(dt: Double) = {
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
	}

	def pathReceive: Actor.Receive = {
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
			} else randomPushSelf()
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
		case PathTick =>
	}
}
