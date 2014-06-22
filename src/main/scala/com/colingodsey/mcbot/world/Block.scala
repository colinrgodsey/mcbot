package com.colingodsey.mcbot.world

import com.colingodsey.logos.collections.{Vec3, IVec3}
import com.colingodsey.mcbot.world

object Block {
	case object Air extends ABlockType(0)
	//
	case object Stone extends ASolidBlockType(1)
	case object Grass extends ASolidBlockType(2)
	case object Dirt extends ASolidBlockType(3)
	//
	case object Bedrock extends ASolidBlockType(7)
	case object FlowWater extends ABlockType(8) {
		override def isWater = true
	}
	case object Water extends ABlockType(9) {
		override def isWater = true
	}
	//
	case object FlowLava extends ABlockType(10)
	case object Lava extends ABlockType(11)
	//
	case object Sand extends ASolidBlockType(12)
	case object Gravel extends ASolidBlockType(13)
	case object GoldOre extends ASolidBlockType(14)
	//
	case object Torch extends ABlockType(50)
	//
	case object RedstoneWire extends ABlockType(55)
	//
	case object WoodDoor extends ADoorType(64)
	case object Rail extends ABlockType(66)
	case object IronDoor extends ADoorType(71)
	//
	case object Fence extends AFenceBlockType(85)
	//
	case object Gate extends AFenceBlockType(107)
	//
	case object NetherFence extends AFenceBlockType(113)

	case class Unknown(typ: Int) extends BlockType {
		require(typ >= 0)

		//plant block IDs
		//val isPassable: Boolean = if(typ >= 31 && typ <= 40) true else false
		val isPassable: Boolean = typ match {
			case _ if typ >= 31 && typ <= 40 => true
			case 59 => true
			case _ => false
		}
	}

	trait BlockType {
		def typ: Int
		def isPassable: Boolean

		def isFence = false
		def isDoor = false
		def isWater = false
	}

	trait SolidBlockType extends BlockType {
		def isPassable: Boolean = false
	}

	val typeSet = Set[BlockType](Air, Stone, Grass, Dirt, Bedrock, FlowWater,
		FlowLava, Sand, Gravel, GoldOre, Torch, RedstoneWire, Fence, Gate,
		WoodDoor, IronDoor, NetherFence, Rail, Lava, Water)

	val halfBlockVec = Vec3.one / 2

	protected abstract class ADoorType(val typ: Int) extends BlockType {
		override def isDoor = true
		def isPassable: Boolean = false
	}

	protected abstract class ABlockType(val typ: Int) extends BlockType {
		def isPassable: Boolean = true
	}
	protected abstract class ASolidBlockType(val typ: Int) extends SolidBlockType
	protected abstract class AFenceBlockType(val typ: Int) extends SolidBlockType {
		override def isFence = true
	}

	val types = {
		val typeMap = typeSet.iterator.map(x => x.typ -> x).toMap

		val res = for(i <- 0 to 256) yield typeMap.getOrElse(
			i, Unknown(i))

		res.toArray
	}

	def apply(pos: Vec3): Block = new Block(IVec3(pos))
}

final case class Block(pos: IVec3) extends Equals {
	import Chunk._

	def chunk(implicit wv: WorldView) = wv.getChunkAt(chunkCoord)

	def typ(implicit wv: WorldView): Int = chunk.typ(cx, cy, cz)
	def meta(implicit wv: WorldView): Int = chunk.meta(cx, cy, cz)
	def light(implicit wv: WorldView): Int = chunk.light(cx, cy, cz)
	def skyLight(implicit wv: WorldView): Int = chunk.skyLight(cx, cy, cz)
	def biome(implicit wv: WorldView): Int = chunk.biome(cx, cy, cz)

	def below(implicit wv: WorldView) = wv.getBlock(pos - Vec3(0, 1, 0))
	def above(implicit wv: WorldView) = wv.getBlock(pos.toVec3 + Vec3(0, 1, 0))

	def btyp(implicit wv: WorldView) = Block.types(typ)

	def isPassable(implicit wv: WorldView): Boolean = {
		val bel = below

		(if(btyp.isDoor) {
			!door.isClosed
		} else btyp.isPassable) && !bel.btyp.isFence
	}

	def x = pos.x
	def y = pos.y
	def z = pos.z

	def cx = chunkPos.x
	def cy = chunkPos.y
	def cz = chunkPos.z

	//require(Chunk.dims = IVec3(16, 16, 16))
	lazy val chunkPos = IVec3(x & 15, y & 15, z & 15)

	lazy val chunkCoord = IVec3(
		math.floor(x.toDouble / Chunk.dims.x).toInt,
		math.floor(y.toDouble / Chunk.dims.y).toInt,
		math.floor(z.toDouble / Chunk.dims.z).toInt)

	def center: Vec3 = pos.toVec3 + Block.halfBlockVec

	def canEqual(that: Any) = that.isInstanceOf[Block]

	override def equals(that: Any) = that match {
		case x: Block if x.pos == pos => true
		case _ => false
	}

	object door {
		//require door
		def isBottom(implicit wv: WorldView) = //((meta >>> 3) & 0x1) == 0
			!below.btyp.isDoor
		def opensRight(implicit wv: WorldView): Boolean = if(!isBottom) (meta & 0x1) == 0
		else above.door.opensRight

		def isClosed(implicit wv: WorldView): Boolean = if(isBottom) ((meta >>> 2) & 0x1) == 0
		else below.door.isClosed
	}
}
