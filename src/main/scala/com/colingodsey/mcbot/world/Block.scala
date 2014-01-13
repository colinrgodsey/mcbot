package com.colingodsey.mcbot.world

import com.colingodsey.logos.collections.{Point3D, IPoint3D}
import com.colingodsey.mcbot.world

object Block {
	case object Air extends ABlockType(0)
	//
	case object Stone extends ASolidBlockType(1)
	case object Grass extends ASolidBlockType(2)
	case object Dirt extends ASolidBlockType(3)
	//
	case object Bedrock extends ASolidBlockType(7)
	case object Water extends ASolidBlockType(8)
	//
	case object Lava extends ASolidBlockType(10)
	//
	case object Sand extends ASolidBlockType(12)
	case object Gravel extends ASolidBlockType(13)
	case object GoldOre extends ASolidBlockType(14)

	case class Unknown(typ: Int) extends BlockType {
		//plant block IDs
		val isPassable: Boolean = if(typ >= 31 && typ <= 40) true else false
	}

	trait BlockType {
		def typ: Int
		def isPassable: Boolean
	}

	trait SolidBlockType extends BlockType {
		def isPassable: Boolean = false
	}

	val typeSet = Set[BlockType](Air, Stone, Grass, Dirt, Bedrock, Water,
		Lava, Sand, Gravel, GoldOre)

	val halfBlockVec = Point3D.one / 2

	protected abstract class ABlockType(val typ: Int) extends BlockType {
		def isPassable: Boolean = true
	}
	protected abstract class ASolidBlockType(val typ: Int) extends SolidBlockType

	val types = {
		val typeMap = typeSet.iterator.map(x => x.typ -> x).toMap

		val res = for(i <- 0 to 256) yield typeMap.getOrElse(
			i, Unknown(i))

		res.toArray
	}

	def apply(id: Int) = types(id)
}

final case class ChunkBlock private[world] (
		x: Int, y: Int, z: Int, chunk: Chunk) extends Block {
	import Chunk._
	
	require(x >= 0 && x < dims.x, "bad x " + x + " " + chunk.pos)
	require(y >= 0 && y < dims.y, "bad y " + y + " " + chunk.pos)
	require(z >= 0 && z < dims.z, "bad z " + z + " " + chunk.pos)

	def typ: Int = chunk.typ(x, y, z)
	def meta: Int = chunk.meta(x, y, z)
	def light: Int = chunk.light(x, y, z)
	def skyLight: Int = chunk.skyLight(x, y, z)
	def biome: Int = chunk.biome(x, y, z)

	def globalPos: IPoint3D = IPoint3D(x + chunk.x * dims.x,
		y + chunk.y * dims.y, z + chunk.z * dims.z)
}

trait Block {
	import Chunk._

	def typ: Int
	def meta: Int
	def light: Int
	def skyLight: Int
	def biome: Int

	def btyp = Block.types(typ)

	def x: Int
	def y: Int
	def z: Int

	def pos = IPoint3D(x, y, z)
	//def globalPos = IPoint3D(x, y, z)
	def globalPos: IPoint3D
}

case class NoBlock(x: Int, y: Int, z: Int) extends Block {
	import Chunk._

	def typ: Int = Block.Air.typ
	def meta: Int = 0
	def light: Int = 0
	def skyLight: Int = 0
	def biome: Int = 0

	def globalPos: IPoint3D = pos
}