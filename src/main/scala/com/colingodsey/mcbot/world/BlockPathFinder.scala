package com.colingodsey.mcbot.world

import com.colingodsey.collections.PathFinding
import com.colingodsey.logos.collections.Vec3

class BlockPathFinder(val worldView: WorldView, dest: Block,
		val maxPathLength: Int) extends PathFinding[Block, Block] {
	import worldView._

	val destPos = dest.pos.toVec3

	private val flatNeighbs0 = Seq(
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

	def legalNeighbors(state: Block): Stream[Block] =
		if(state.btyp.isWater) legalNeighborsWater(state)
		else legalNeighborsLand(state)

	def legalNeighborsWater(state: Block): Stream[Block] = {
		val flatNeighbs = flatNeighbs0.map(x => Block(state.pos.toVec3 + x))

		val posBlocksSet = (flatNeighbs.toStream #:::
				flatNeighbs.toStream.map(_.below) #:::
				flatNeighbs.toStream.map(_.above)).toSet - state

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

		posBlocksSet.toSeq.filter { block =>
			block.isPassable && block.above.isPassable
		}.sortBy { block =>
			val vec = destPos - block.pos

			vec.length
		}.sortBy { block =>
		/*var moveVec = block.globalPos - state.globalPos
		val weight = if((moveVec * moveVec) != 0.8) 1 else 1
		-(moveVec * delta * weight)// - moveVec.length*/
			val vec = destPos - block.pos

			vec.length
		}.toStream
	}

	def legalNeighborsLand(state: Block): Stream[Block] = {
		val delta = destPos - state.pos

		val sortedNs = flatNeighbs0//.sortBy(x => (state.pos + x - destPos).length)

		val posBlocks = sortedNs.toStream map { x =>
			getBlock(state.pos.toVec3 + x)
		}

		val onGround = !state.below.isPassable


		/*def localBottom =
			if(!state.btyp.isWater && state.below.btyp.isWater) state.below
			else takeBlockDownWater(getBlock(state.pos.toPoint3D))

		if(localBottom != state) {
			val dy = localBottom.pos.y - state.pos.y

			if(math.abs(dy) > 4) return Stream()

			return Stream(localBottom -> Vec3(0, dy, 0))
		}*/

		// XX topBlock
		// XX block
		// == bottomBlock
		val flatN = posBlocks.filter { block =>
			block.isPassable && block.above.isPassable &&
				!block.below.isPassable
		}

		val availFlatN = flatN.map(_.pos - state.pos).toSet

		val cornerN = corners.toStream filter { case (corner, reqs) =>
			(reqs -- availFlatN).isEmpty
		} flatMap { case (corner, reqs) =>
			val p = state.pos.toVec3 + corner
			val block = getBlock(p)

			val r = !block.below.isPassable &&
				block.above.isPassable &&
				block.isPassable

			if(r) Some(block)
			else None
		}

		val lowerN = posBlocks flatMap { block =>
			val p = block.pos.toVec3
			//def topBlock = getBlock(p + Vec3(0, 1, 0))

			lazy val lowerBlock = takeBlockDownWater(block)

			def dropLength = {
				block.pos.y - lowerBlock.pos.y
			}

			if(block.isPassable &&
					block.above.isPassable &&
					block.below.isPassable &&
					dropLength >= 1 && dropLength <= 4)
				//Some(lowerBlock, move + Vec3(0, -dropLength, 0))
				Some(block)
			else None
		}

		lazy val upperPossible = getBlock(
			state.pos.toVec3 + Vec3(0, 2, 0)).isPassable &&
			onGround

		/*val upperN = posBlocks flatMap { case (block, move) =>
			if(!upperPossible) Nil
			else {
				val p = block.pos.toPoint3D
				def topBlock = getBlock(p + Vec3(0, 2, 0))

				if(!block.isPassable && topBlock.isPassable &&
						block.above.isPassable)
					Some(block.above, move + Vec3(0, 1, 0))
				else None
			}
		}*/
		val upperNRes = posBlocks flatMap { block =>
			if(!upperPossible) Nil
			else {
				val p = block.pos.toVec3
				def topBlock = getBlock(p + Vec3(0, 2, 0))

				if(!block.isPassable && topBlock.isPassable &&
						block.above.isPassable)
					Some(block.above)
				else None
			}
		}
		//use bock directly above us
		val upperN = if(upperPossible && !upperNRes.isEmpty) Stream(state.above)
		else Stream()

		val dropB = if(state.below.isPassable) Stream(state.below)
		else Stream()

		//do flat first..
		//flatN #::: lowerN #::: upperN    //use this for non a*
		(dropB #::: cornerN #::: flatN #::: lowerN #::: upperN).sortBy { block =>
			/*var moveVec = block.globalPos - state.globalPos
			val weight = if((moveVec * moveVec) != 0.8) 1 else 1
			-(moveVec * delta * weight)// - moveVec.length*/
			val vec = destPos - block.pos

			vec.length
		}.map(x => x -> x)
	}
}