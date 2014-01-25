package com.colingodsey.mcbot.world

import com.colingodsey.collections.PathFinding
import com.colingodsey.logos.collections.Vec3D

class BlockPathFinder(val worldView: WorldView, dest: Block, val maxPathLength: Int) extends PathFinding[Block, Vec3D] {
	import worldView._

	val destPos = dest.globalPos.toPoint3D

	private val flatNeighbs = Seq(
		Vec3D(-1, 0, 0),
		Vec3D(0, 0, -1),
		Vec3D(1, 0, 0),
		Vec3D(0, 0, 1)
	)

	def legalNeighbors(state: Block): Stream[(Block, Vec3D)] = {
		val delta = destPos - state.globalPos

		val sortedNs = flatNeighbs.sortBy(_ * delta)

		val posBlocks = sortedNs.toStream map { x =>
			(getBlock(state.globalPos.toPoint3D + x), x)
		}
		def localBottom = getBlock(state.globalPos.toPoint3D + Vec3D(0, -1, 0))

		if(localBottom.btyp.isPassable || !state.btyp.isPassable) return Stream()

		// XX topBlock
		// XX block
		// == bottomBlock
		val flatN = posBlocks filter { case (block, move) =>
			val p = block.globalPos.toPoint3D
			def topBlock = getBlock(p + Vec3D(0, 1, 0))
			def bottomBlock = getBlock(p + Vec3D(0, -1, 0))

			block.btyp.isPassable && topBlock.btyp.isPassable && !bottomBlock.btyp.isPassable
		}

		val lowerN = posBlocks flatMap { case (block, move) =>
			val p = block.globalPos.toPoint3D
			def topBlock = getBlock(p + Vec3D(0, 1, 0))
			def lowerBlock = getBlock(p + Vec3D(0, -1, 0))
			def bottomBlock = getBlock(p + Vec3D(0, -2, 0))

			if(block.btyp.isPassable &&
					!bottomBlock.btyp.isPassable &&
					topBlock.btyp.isPassable &&
					lowerBlock.btyp.isPassable)
				Some(lowerBlock, move + Vec3D(0, -1, 0))
			else None
		}

		lazy val upperPossible = getBlock(
			state.globalPos.toPoint3D + Vec3D(0, 2, 0)).btyp.isPassable
		val upperN = posBlocks flatMap { case (block, move) =>
			if(!upperPossible) Nil
			else {
				val p = block.globalPos.toPoint3D
				def topBlock = getBlock(p + Vec3D(0, 2, 0))
				def midBlock = getBlock(p + Vec3D(0, 1, 0))
				//val bottomBlock = block

				if(!block.btyp.isPassable && topBlock.btyp.isPassable &&
						midBlock.btyp.isPassable)
					Some(midBlock, move + Vec3D(0, 1, 0))
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