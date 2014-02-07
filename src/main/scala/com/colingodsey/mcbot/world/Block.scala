package com.colingodsey.mcbot.world

import com.colingodsey.logos.collections.{Vec3, IPoint3D}
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

	def apply(id: Int) = types(id)
}

final case class ChunkBlock private[world] (
		x: Int, y: Int, z: Int, chunk: Chunk)(implicit val wv: WorldView) extends Block {
	import Chunk._
	
	require(x >= 0 && x < dims.x, "bad x " + x + " " + chunk.pos)
	require(y >= 0 && y < dims.y, "bad y " + y + " " + chunk.pos)
	require(z >= 0 && z < dims.z, "bad z " + z + " " + chunk.pos)

	def typ: Int = chunk.typ(x, y, z)
	def meta: Int = chunk.meta(x, y, z)
	def light: Int = chunk.light(x, y, z)
	def skyLight: Int = chunk.skyLight(x, y, z)
	def biome: Int = chunk.biome(x, y, z)

	def isPassable: Boolean = {
		val bel = below

		(if(btyp.isDoor) {
			!door.isClosed
		} else btyp.isPassable) && !bel.btyp.isFence
	}

	lazy val globalPos: IPoint3D = IPoint3D(x + chunk.x * dims.x,
		y + chunk.y * dims.y, z + chunk.z * dims.z)
}

trait Block extends Equals {
	import Chunk._

	def typ: Int
	def meta: Int
	def light: Int
	def skyLight: Int
	def biome: Int

	def wv: WorldView

	def below = wv.getBlock(globalPos - Vec3(0, 1, 0))
	def above = wv.getBlock(globalPos.toPoint3D + Vec3(0, 1, 0))

	def btyp = Block.types(typ)

	def isPassable: Boolean

	def x: Int
	def y: Int
	def z: Int

	def pos = IPoint3D(x, y, z)
	//def globalPos = IPoint3D(x, y, z)
	def globalPos: IPoint3D

	object door {
		//require door
		def isBottom = //((meta >>> 3) & 0x1) == 0
			!below.btyp.isDoor
		def opensRight: Boolean = if(!isBottom) (meta & 0x1) == 0
		else above.door.opensRight

		def isClosed: Boolean = if(isBottom) ((meta >>> 2) & 0x1) == 0
		else below.door.isClosed
	}
}

case class NoBlock(x: Int, y: Int, z: Int)(implicit val wv: WorldView) extends Block {
	import Chunk._

	def typ: Int = Block.Air.typ
	def meta: Int = 0
	def light: Int = 0
	def skyLight: Int = 0
	def biome: Int = 0
	def isPassable = true

	def globalPos: IPoint3D = pos
}