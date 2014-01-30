package com.colingodsey.mcbot.world

import com.colingodsey.collections.PathFinding
import com.colingodsey.logos.collections.Vec3

class BlockPathFinder(val worldView: WorldView, dest: Block, val maxPathLength: Int) extends PathFinding[Block, Vec3] {
	import worldView._

	val destPos = dest.globalPos.toPoint3D

	private val flatNeighbs = Seq(
		Vec3(-1, 0, 0),
		Vec3(0, 0, -1),
		Vec3(1, 0, 0),
		Vec3(0, 0, 1)
	)

	private val corners = Seq(
		Vec3(1, 0, 1) -> Set(Vec3(1, 0, 0), Vec3(0, 0, 1)),
		Vec3(1, 0, -1) -> Set(Vec3(1, 0, 0), Vec3(0, 0, -1)),
		Vec3(-1, 0, 1) -> Set(Vec3(-1, 0, 0), Vec3(0, 0, 1)),
		Vec3(-1, 0, -1) -> Set(Vec3(-1, 0, 0), Vec3(0, 0, -1))
	)

	def legalNeighbors(state: Block): Stream[(Block, Vec3)] = {
		val delta = destPos - state.globalPos

		val sortedNs = flatNeighbs//.sortBy(_ * delta)

		val posBlocks = sortedNs.toStream map { x =>
			(getBlock(state.globalPos.toPoint3D + x), x)
		}
		def localBottom = getBlock(state.globalPos.toPoint3D + Vec3(0, -1, 0))

		if(localBottom.btyp.isPassable || !state.btyp.isPassable) return Stream()

		// XX topBlock
		// XX block
		// == bottomBlock
		val flatN = posBlocks filter { case (block, move) =>
			val p = block.globalPos.toPoint3D
			def topBlock = getBlock(p + Vec3(0, 1, 0))
			def bottomBlock = getBlock(p + Vec3(0, -1, 0))

			block.btyp.isPassable && topBlock.btyp.isPassable &&
					!bottomBlock.btyp.isPassable
		}

		val availFlatN = flatN.map(_._2).toSet

		val cornerN = corners.toStream filter { case (corner, reqs) =>
			(reqs -- availFlatN).isEmpty
		} flatMap { case (corner, reqs) =>
			val p = state.globalPos.toPoint3D + corner
			val block = getBlock(p)
			def topBlock = getBlock(p + Vec3(0, 1, 0))
			def bottomBlock = getBlock(p + Vec3(0, -1, 0))

			val r = !bottomBlock.btyp.isPassable &&
				topBlock.btyp.isPassable &&
				block.btyp.isPassable

			if(r) Some(block -> corner)
			else None
		}

		val lowerN = posBlocks flatMap { case (block, move) =>
			val p = block.globalPos.toPoint3D
			def topBlock = getBlock(p + Vec3(0, 1, 0))

			lazy val lowerBlock = takeBlockDown(block)
			lazy val bottomBlock = getBlock(
				lowerBlock.globalPos - Vec3(0, 1, 0))

			def dropLength = {
				block.globalPos.y - lowerBlock.globalPos.y
			}

			if(block.btyp.isPassable &&
					topBlock.btyp.isPassable &&
					!bottomBlock.btyp.isPassable &&
					dropLength >= 1 && dropLength <= 4)
				Some(lowerBlock, move + Vec3(0, -dropLength, 0))
			else None
		}

		lazy val upperPossible = getBlock(
			state.globalPos.toPoint3D + Vec3(0, 2, 0)).btyp.isPassable
		val upperN = posBlocks flatMap { case (block, move) =>
			if(!upperPossible) Nil
			else {
				val p = block.globalPos.toPoint3D
				def topBlock = getBlock(p + Vec3(0, 2, 0))
				def midBlock = getBlock(p + Vec3(0, 1, 0))
				//val bottomBlock = block

				if(!block.btyp.isPassable && topBlock.btyp.isPassable &&
						midBlock.btyp.isPassable)
					Some(midBlock, move + Vec3(0, 1, 0))
				else None
			}
		}

		//do flat first..
		//flatN #::: lowerN #::: upperN    //use this for non a*
		(cornerN #::: flatN #::: lowerN #::: upperN).sortBy { case (block, moves) =>
			/*var moveVec = block.globalPos - state.globalPos
			val weight = if((moveVec * moveVec) != 0.8) 1 else 1
			-(moveVec * delta * weight)// - moveVec.length*/
			val vec = destPos - block.globalPos

			vec.length
		}
	}
}