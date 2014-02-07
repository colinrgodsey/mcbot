package com.colingodsey.mcbot.world

import com.colingodsey.collections.PathFinding
import com.colingodsey.logos.collections.Vec3

class BlockPathFinder(val worldView: WorldView, dest: Block, val maxPathLength: Int) extends PathFinding[Block, Vec3] {
	import worldView._

	val destPos = dest.pos.toPoint3D

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

	def legalNeighbors(state: Block): Stream[(Block, Vec3)] =
		if(state.btyp.isWater) legalNeighborsWater(state)
		else legalNeighborsLand(state)

	def legalNeighborsWater(state: Block): Stream[(Block, Vec3)] = {
		val topBlocks = flatNeighbs.toStream.map(_ + Vec3(0, 1, 0)) map { x =>
			(getBlock(state.pos.toPoint3D + x), x)
		}

		val ns = flatNeighbs.toStream #:::
				flatNeighbs.toStream.map(_ - Vec3(0, 1, 0))

		val posBlocks = ns map { x =>
			(getBlock(state.pos.toPoint3D + x), x)
		}

		/*topBlocks.filter { case (bl, mv) =>
			bl.isPassable && bl.above.isPassable
		}.sortBy { case (block, moves) =>
			val vec = destPos - block.globalPos

			vec.length
		} #::: posBlocks.filter { case (bl, mv) =>
			bl.isPassable && bl.above.isPassable
		}.sortBy { case (block, moves) =>
			val vec = destPos - block.globalPos

			vec.length
		}*/

		(topBlocks #::: posBlocks).filter { case (bl, mv) =>
			bl.isPassable && bl.above.isPassable
		}.sortBy { case (block, moves) =>
			val vec = destPos - block.pos

			vec.length
		}
	}

	def legalNeighborsLand(state: Block): Stream[(Block, Vec3)] = {
		val delta = destPos - state.pos

		val sortedNs = flatNeighbs//.sortBy(_ * delta)

		val posBlocks = sortedNs.toStream map { x =>
			(getBlock(state.pos.toPoint3D + x), x)
		}
		def localBottom =
			if(!state.btyp.isWater && state.below.btyp.isWater) state.below
			else takeBlockDownWater(getBlock(state.pos.toPoint3D))

		if(!isPassable(state)) return Stream()

		if(localBottom != state) {
			val dy = localBottom.pos.y - state.pos.y

			if(math.abs(dy) > 4) return Stream()

			return Stream(localBottom -> Vec3(0, dy, 0))
		}

		// XX topBlock
		// XX block
		// == bottomBlock
		val flatN = posBlocks filter { case (block, move) =>
			val p = block.pos.toPoint3D
			def topBlock = getBlock(p + Vec3(0, 1, 0))
			def bottomBlock = getBlock(p + Vec3(0, -1, 0))

			isPassable(block) && topBlock.isPassable &&
					!isPassable(bottomBlock)
		}

		val availFlatN = flatN.map(_._2).toSet

		val cornerN = corners.toStream filter { case (corner, reqs) =>
			(reqs -- availFlatN).isEmpty
		} flatMap { case (corner, reqs) =>
			val p = state.pos.toPoint3D + corner
			val block = getBlock(p)
			def topBlock = getBlock(p + Vec3(0, 1, 0))
			def bottomBlock = getBlock(p + Vec3(0, -1, 0))

			val r = !isPassable(bottomBlock) &&
				topBlock.isPassable &&
				block.isPassable

			if(r) Some(block -> corner)
			else None
		}

		val lowerN = posBlocks flatMap { case (block, move) =>
			val p = block.pos.toPoint3D
			def topBlock = getBlock(p + Vec3(0, 1, 0))

			lazy val lowerBlock = takeBlockDownWater(block)
			lazy val bottomBlock = getBlock(
				lowerBlock.pos - Vec3(0, 1, 0))

			def dropLength = {
				block.pos.y - lowerBlock.pos.y
			}

			if(block.isPassable &&
					topBlock.isPassable &&
					!bottomBlock.isPassable &&
					dropLength >= 1 && dropLength <= 4)
				Some(block/*lowerBlock*/, move/* + Vec3(0, -dropLength, 0)*/)
			else None
		}

		lazy val upperPossible = getBlock(
			state.pos.toPoint3D + Vec3(0, 2, 0)).isPassable
		val upperN = posBlocks flatMap { case (block, move) =>
			if(!upperPossible) Nil
			else {
				val p = block.pos.toPoint3D
				def topBlock = getBlock(p + Vec3(0, 2, 0))
				def midBlock = getBlock(p + Vec3(0, 1, 0))
				//val bottomBlock = block

				if(!isPassable(block) && topBlock.isPassable &&
						midBlock.isPassable)
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
			val vec = destPos - block.pos

			vec.length
		}
	}
}