package com.colingodsey.mcbot.client

import com.colingodsey.logos.collections.Vec3
import com.colingodsey.mcbot.world._
import scala.concurrent.duration.Deadline
import scala.concurrent.{Future, ExecutionContext, blocking}
import akka.event.LoggingAdapter
import scala.util.Failure
import akka.actor.{Actor, ActorRef}
import akka.pattern._
import scala.util.Failure
import scala.Some
import com.colingodsey.mcbot.world.Player
import com.colingodsey.ai.{QLPolicy, QLearning}
import com.colingodsey.collections.{VecN, MapVector}

object BotNavigation {
	case class PathFound(path: Seq[Vec3])
	case class MoveFound(move: Option[Vec3])
}

trait BotNavigation extends WaypointManager with CollisionDetection {
	import BotNavigation._
	import WaypointManager._
	import BotClient._

	val worldView: WorldView
	import worldView._

	def footPos: Vec3
	def footBlockPos: Vec3
	def footBlock: Block
	def selfEnt: Player

	def dead: Boolean
	def joined: Boolean

	def jump()
	def randomPushSelf()

	def log: LoggingAdapter
	def self: ActorRef
	implicit def ec: ExecutionContext

	var lastWaypointId: Option[Int] = None
	var gettingPath = false
	var moveGoal: Option[Vec3] = None
	var curPath: Seq[Vec3] = Nil
	var lastPathTime = 0.0
	var direction = Vec3.zero//(math.random, 0, math.random).normal
	var targetingEnt: Option[Int] = None
	var lastTransition: Option[WaypointTransition] = None
	var lastQ: Option[VecN] = None

	def maxPathLength: Int = 100

	def waypointMinDistance = 10.0

	def lastWaypoint = lastWaypointId map waypoints

	private def curTime = System.currentTimeMillis / 1000.0

	def getPath(goal: Vec3): Unit = if(!gettingPath) {
		log.info("getting path")
		gettingPath = true
		val fut = scala.concurrent.future {
			val floorTargetBlock = getBlock(goal + Vec3(0, -1, 0))
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
				case Failure(t: FindChunkError) =>
					//log.error(t, "getPath failed")
				case Failure(t) =>
					log.error(t, "getPath failed")
				case _ => //log.info("Path finished")
			}
		}

		fut.map(PathFound) pipeTo self
	} else self ! PathFound(Nil)

	def takeBlockDown(vec: Vec3): Block =
		takeBlockDown(getBlock(vec))

	def takeBlockDown(block: Block): Block = {
		var ptr = block.globalPos.toPoint3D

		if(!block.btyp.isPassable) sys.error("block start solid!")

		while(getBlock(ptr).btyp.isPassable && ptr.y > 0)
			ptr -= Vec3(0, 1, 0)

		ptr += Vec3(0, 1, 0)

		getBlock(ptr)
	}

	def getShortPath(from: Vec3, to: Vec3): Seq[Vec3] = blocking {
		try {
			val endTargetBlock = getBlock(to)
			val targetBlock = takeBlockDown(endTargetBlock)

			val endStartBlock = getBlock(from)
			val startBlock = takeBlockDown(endStartBlock)

			//val finder = new BlockPathFinder(worldView, targetBlock, 128)

			roughPathFrom(startBlock, targetBlock, 1900).toSeq.flatten
		} catch {
			case t: FindChunkError => Nil
		}
	}

	def roughPathFrom(start: Block, to: Block, of: Int = 1000): Option[Seq[Vec3]] = blocking {
		val finder = new BlockPathFinder(worldView, to, 128)
		val paths = finder.pathsFrom(start)

		val iter = paths.iterator
		var n = 0
		//val res = new VectorBuilder[Seq[Move]]

		while(iter.hasNext && n < of) {
			val (state, moves) = iter.next

			val vec = to.globalPos - state.globalPos

			if(state == to || vec.length <= 3) {
				//res += moves
				//println(n, moves.length)
				return Some(moves.reverse)
			}

			n += 1
		}

		None
	}

	def visitWaypoint(id: Int) = {
		waypoints(id).updateProperty("visited", curTime)
		waypoints(id).updateProperty("nvisits",
			waypoints(id).property("nvisits") + 1)
	}

	def checkWaypoints(): Unit = try blocking {
		val closestA = getNearWaypoints(footBlockPos,
				waypointMinDistance)
		val closest = closestA filter { wp =>
			lazy val p = getShortPath(selfEnt.pos, wp.pos)

			//either super close, or no path
			takeBlockDown(footBlockPos) == takeBlockDown(wp.pos) || !p.isEmpty
		}

		closest.headOption match {
			case None if selfEnt.onGround =>

				val newPath = lastWaypoint.toSeq.flatMap(x =>
					getShortPath(x.pos, selfEnt.pos))

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
				if(true) {
					val posDelta = closestA.headOption.map(
						_.pos).getOrElse(Vec3.zero) - selfEnt.pos

					if(posDelta.length < 4) {
						log.info(s"bad new waypoint!! ${selfEnt.pos} ${closestA.headOption} ${closest.headOption}")
						//direction = Vec3.random
						randomPushSelf()
					} else {
						val w = new Waypoint(nextWaypointId, selfEnt.pos)
						log.info("Adding waypoint " + w)
						addWaypoint(w)

						if(lastWaypoint.isDefined) {
							connectWaypoints(lastWaypointId.get, w.id)
							connectWaypoints(w.id, lastWaypointId.get)
							lastTransition = Some(WaypointTransition(
								lastWaypointId.get, w.id))
							reinforce(lastTransition.get,
								MapVector("discover" -> 100.0),
								Set(lastWaypointId.get, w.id))
							val revTran = WaypointTransition(w.id, lastWaypointId.get)
							reinforce(revTran,
								MapVector("discover" -> 10.0),
								Set(lastWaypointId.get, w.id))
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

				if(from.connectsTo(x.id)) {
					val disc = qValue(lastTransition.get)("discover") * 0.1
					reinforce(lastTransition.get,
						MapVector(), Set(/*from.id, */x.id))//"discover" -> -disc))
				}
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
	}



	def pathPhysicsTick(dt: Double) = {
		//drop head node if we're close
		if(!curPath.isEmpty) {
			val nextStep = curPath.head + Block.halfBlockVec

			val vec = (nextStep - footBlockPos)

			if(vec.length < 0.4) {
				curPath = curPath.tail
				lastPathTime = curTime
				//if(!curPath.isEmpty) say("Next stop, " + curPath.headOption)
				/*if(curPath.isEmpty) */direction = Vec3.zero
				log.info("visited path node")
			}

			if(vec.length > 2) {
				direction = Vec3.zero
				curPath = Nil
				lastPathTime = curTime
				log.info("bad path!")
			}
		}

		//if we still have a path
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

			val dir = dirs.head//reduce(_ + _)

			direction = dir.normal * 4

			/*if(selfEnt.vel.length < 0.01)
				direction += Vec3.random*/
		} //else direction = Point3D.zero
	}

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
				val waypointPath = finder(target.pos).pathFrom(curWp, x, of = 2000).toSeq.flatten
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

	def findNewRandomWp(): Unit = try {
		//pick a random place...

		val curWpId = lastWaypoint
		val nearWps = getNearWaypoints(selfEnt.pos, maxNum = 10) filter { x =>
			val vec = x.pos - selfEnt.pos
			val trace = traceRay(selfEnt.pos, vec, footPos)
			Some(x.id) != curWpId && trace.dist >= vec.length
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
			val weight = wpWeight(wp)
			val vec = wp.pos - selfEnt.pos

			if(vec.length > 0) vec.normal * weight else Vec3.zero
		}.foldLeft(Vec3.zero)(_ + _)

		val wpAvg = Vec3(hintDir.x, 0.01, hintDir.z).normal

		def getRandomVec = {
			val flatIshRandom = Vec3(math.random * 2 - 1, math.random * 0.8 - 0.4,
				math.random * 2 - 1).normal
			val vec = flatIshRandom * (math.random * 25 + 5)// + wpAvg.normal * 10
			val hit = traceRay(selfEnt.pos, vec, footPos)

			hit match {
				case x if x.dist > 1.2 =>
					val hitPos = selfEnt.pos + vec.normal * (x.dist - 0.2)
					selfEnt.pos
					var pos = getBlock(hitPos).globalPos.toPoint3D + Block.halfBlockVec
					while(getBlock(pos).btyp.isPassable && pos.y > 0)
						pos -= Vec3(0, 1, 0)

					Some(pos + Vec3(0, 1, 0))
				case _ => None
			}
		}

		val rVecs = Vector.fill(30)(getRandomVec).flatten.sortBy { x =>
			val hintFac = hintDir * x
			val endPos = selfEnt.pos + x
			val unexploredWeight = math.min((wpCenter - endPos).length, 10)

			val nearbyWps = getNearWaypoints(endPos,
				maxNum = 20, radius = waypointMinDistance / 2.0)

			val brandNewFac = if(nearbyWps.isEmpty) 600
			else (nearbyWps.head.pos - selfEnt.pos).length

			val discoverFac = if(nearbyWps.isEmpty) 1.0
			else desire("discover") + 1.0

			discoverFac * brandNewFac/* * hintFac*/ * -x.length// - unexploredWeight * 0.5
		}

		val filtered = rVecs.toStream filter { pos =>
			val p = getShortPath(selfEnt.pos, pos)

			!p.isEmpty
		}

		filtered.headOption match {
			case Some(pos) if (pos - footBlockPos).length > 1.2 =>
				log.info(s"random target res ${filtered.headOption}")
				moveGoal = Some(pos)
				getPath(moveGoal.get)
			case a =>
				log.info("failed to get random targeT! best "+ a)
				randomPushSelf()
				direction = Vec3.random
		}
	} catch {
		case x: FindChunkError =>
	}

	def selectWaypoint(): Boolean = {
		if(!lastWaypoint.isDefined || !selfEnt.onGround) return false

		val lastWp = lastWaypoint.get
		val trans = transFrom(lastWaypointId.get, Some(lastWaypointId.get))

		val curWpPath = getShortPath(selfEnt.pos, lastWp.pos)

		if(curWpPath.isEmpty) {
			//lastWaypoint = None
			log.info("Bailing on current wp")
			false
		} else if(trans.isEmpty) {
			log.info("Failed WP select")
			false
		} else {
			//val selTrans = policy(trans)
			val selTrans = selector.selectFrom(trans) { x =>
				val destWp = waypoints(x.destId)
				val age = curTime - destWp.property("visited") + 1
				qValue(x) * desire.normal * age
			}
			val sel = waypoints(selTrans.destId)
			val vec = selfEnt.pos - sel.pos
			val selQ = qValue(selTrans)

			if(vec.length < 5) false
			else if((lastWp.pos - footBlockPos).length > 6) {
				moveGoal = Some(lastWp.pos)
				log.info("centering around wp")
				true
			} else {
				moveGoal = Some(sel.pos)
				lastQ = Some(selQ)

				val path = getShortPath(selfEnt.pos, sel.pos)

				getPath(sel.pos)

				if(path.length < 2) {
					disconnectWaypoints(lastWaypointId.get, sel.id)

					log.info("bad wp sel!")
					false
				} else {
					log.info("selected node w q-value " + qValue(selTrans))

					true
				}
			}
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

			if(path.isEmpty) {
				direction = Vec3.zero//curPath = Nil
				moveGoal = None
				log.info("stopping. path done")
			} else {
				val oldCurPath = curPath
				curPath = path.drop(startIdx)
				if(curPath != oldCurPath) lastPathTime = curTime
			}

			if(!curPath.isEmpty) {
				println("curpath: " + curPath)
				println("dir " + direction)
			} else {
				randomPushSelf()
				log.info("no path!")
			}

			direction = Vec3.zero

			if(selfEnt.onGround) checkWaypoints()
		case PathTick if !dead && joined && !gettingPath && selfEnt.onGround =>
			//lastCurPath = curPath

			if((curTime - lastPathTime) > 5) {
				val vec = curPath.headOption.map(
					x => (x - footBlock.globalPos).length).getOrElse(-1)
				log.info(s"path timeout. len $vec dir $direction vel ${selfEnt.vel}")
				randomPushSelf()
				curPath = Nil
				moveGoal = None
				lastPathTime = curTime
			}

			if(targetingEnt.isDefined) {
				val ent = entities(targetingEnt.get)
				if(!moveGoal.isDefined || math.random < 0.15) moveGoal = Some(ent.pos)
			}

			if(math.random < 0.05) {
				moveGoal = None
				curPath = Nil
				lastPathTime = curTime
			}

			if(moveGoal.isDefined && curPath.isEmpty) getPath(moveGoal.get)
			else if(selfEnt.onGround) checkWaypoints()

			if(curPath.isEmpty && !targetingEnt.isDefined && !moveGoal.isDefined &&
					curPath.isEmpty && lastWaypoint.isDefined) {
				val wpQ = lastTransition.map(qValue).getOrElse(MapVector.zero)
				val selQ = (for {
					trans <- lastTransition
					transs = transFrom(trans)
					if(!transs.isEmpty)
					sel = policy(transs)
					q = qValue(sel)
				} yield q).getOrElse(wpQ)

				//if(desire("discover") > wpQ("discover") || math.random < 0.1) findNewRandomWp

				/*if(desire("discover") > 10 && selQ("discover") <= wpQ("discover"))
					findNewRandomWp()
				else */if(math.random < 0.05) findNewRandomWp()
				//else if(!selectWaypoint()) findNewRandomWp()
				else {
					//if(wpQ("discover") > 20 && math.random < 0.15) findNewRandomWp()
					if(!moveGoal.isDefined) {
						if(!selectWaypoint()) findNewRandomWp()
					}
				}
			} else if(targetingEnt.isDefined && curPath.isEmpty
					&& lastWaypoint.isDefined && !moveGoal.isDefined) {
				findMove()
			}
		case PathTick =>
	}
}
