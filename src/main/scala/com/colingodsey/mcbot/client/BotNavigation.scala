package com.colingodsey.mcbot.client

import com.colingodsey.logos.collections.Point3D
import com.colingodsey.mcbot.world.{Block, WorldView}
import scala.concurrent.duration.Deadline
import scala.concurrent.blocking
import akka.event.LoggingAdapter
import scala.util.Failure
import com.colingodsey.collections.PathFinding

trait BotNavigation {
	var gettingPath = false

	val worldView: WorldView

	def footPos: Point3D
	def footBlock: Block

	def log: LoggingAdapter

	import worldView._

	def getPath(goal: Point3D): Unit = if(!gettingPath) {
		gettingPath = true
		val fut = scala.concurrent.future {
			val floorTargetBlock = getBlock(goal + Point3D(0, -1, 0))
			val endTargetBlock = getBlock(goal)

			val targetBlock = if(floorTargetBlock.btyp.isPassable) floorTargetBlock
			else endTargetBlock

			val finder = new PathFinder(targetBlock)

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

	class PathFinder(dest: Block, val maxPathLength: Int = BotClient.this.maxPathLength) extends PathFinding[Block, Point3D] {

		val destPos = dest.globalPos.toPoint3D

		def legalNeighbors(state: Block): Stream[(Block, Point3D)] = {
			val delta = destPos - state.globalPos

			val sortedNs = flatNeighbs.sortBy(_ * delta)

			val posBlocks = sortedNs.toStream map { x =>
				(getBlock(state.globalPos.toPoint3D + x), x)
			}
			def localBottom = getBlock(state.globalPos.toPoint3D + Point3D(0, -1, 0))

			if(localBottom.btyp.isPassable || !state.btyp.isPassable) return Stream()

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

				if(block.btyp.isPassable &&
						!bottomBlock.btyp.isPassable &&
						topBlock.btyp.isPassable &&
						lowerBlock.btyp.isPassable)
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

			//do flat first..
			//flatN #::: lowerN #::: upperN    //use this for non a*
			(flatN #::: lowerN #::: upperN).sortBy { case (block, moves) =>
				val moveVec = block.globalPos - state.globalPos
				-(moveVec * delta)// - moveVec.length
			}
		}
	}
}
