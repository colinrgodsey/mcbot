package com.colingodsey.mcbot.world

import com.colingodsey.mcbot.protocol
import com.colingodsey.logos.collections._
import scala.concurrent.blocking
import java.util.zip.{InflaterInputStream, Inflater}
import scala.collection.immutable.VectorBuilder

object Chunk {
	val dims = IVec3(16, 16, 16)
	val numBlocks = dims.x * dims.y * dims.z

	val biomeArrSize = dims.x * dims.y

	val defaultSkyValue = 15

	def apply(x: Int, y: Int, z: Int): Chunk =
		new MutableChunk(x, y, z)

	def apply(x: Int, z: Int, bitmask: Short, addBitmask: Short,
			groundUp: Boolean, skylight: Boolean,
			data: IndexedSeq[Byte])(implicit wv: WorldView): Seq[Chunk] = {
		val size = chunkSize(skylight)

		require(addBitmask == 0)
		require(data.length > 0 || bitmask == 0)

		val intBitmask = bitmask & 0xFFFF

		var readIdx = 0
		def read(n: Int) = {
			val oldIdx = readIdx
			readIdx += n
			require(data.length >= readIdx, s"expected $n found ${data.length - oldIdx} total ${data.length}")
			data.slice(oldIdx, readIdx)
		}

		val nChunks = num1Bits(intBitmask)

		val types = read(nChunks * numBlocks)
		val meta = read(nChunks * numBlocks / 2)
		val light = read(nChunks * numBlocks / 2)
		val slight = if(skylight) Some(read(nChunks * numBlocks / 2)) else None
		//val add = read(nChunks * numBlocks / 2) only if bitmask values
		val biome = if(groundUp) Some(read(biomeArrSize).toArray) else None//Array.fill[Byte](255)(0)

		var yChunkIdx = 0
		for(y <- 0 until 16) yield {
			if((intBitmask & (1 << y)) != 0) {
				val idx = numBlocks * yChunkIdx
				val nextIdx = numBlocks * (yChunkIdx + 1)
				yChunkIdx += 1

				require(types.length >= nextIdx)

				new MutableChunk(x, y, z,
					Some(types.slice(idx, nextIdx).toArray),
					Some(meta.slice(idx / 2, nextIdx / 2).toArray),
					Some(light.slice(idx / 2, nextIdx / 2).toArray),
					slight.map(_.slice(idx / 2, nextIdx / 2).toArray),
					Array(),//add.slice(idx / 2, nextIdx / 2).toArray,
					biome)
			} else
				new MutableChunk(x, y, z, blockBiome = biome)
		}
	}

	//add is assumed to be 0 here
	def chunkSize(skylight: Boolean) =
		numBlocks * 2 + (if(skylight) numBlocks / 2 else 0)// + numBlocks / 2

	def xyzToIdx(x: Int, y: Int, z: Int) = // 0000YYYY ZZZZXXXX
		(y * dims.z + z) * dims.x + x // y * 16 * 16 + z * 16 + x

	def idxToXyz(idx: Int) = {
		val y = idx >>> 8 & 0x0F
		val z = idx >>> 4 & 0x0F
		val x = idx & 0x0F

		IVec3(x, y, z)
	}

	def xyzHalfIdxGet(x: Int, y: Int, z: Int, arr: Array[Byte]): Int = {
		val idx = xyzToIdx(x, y, z)
		val isOdd = (idx & 1) == 1
		val hidx = idx / 2

		if(!isOdd) (arr(hidx) >>> 4) & 0xF
		else arr(hidx) & 0xF
	}

	def xyzHalfIdxSet(x: Int, y: Int, z: Int, value: Int, arr: Array[Byte]) {
		val idx = xyzToIdx(x, y, z)
		val isOdd = (idx & 1) == 1
		val hidx = idx / 2
		val cur = arr(hidx)

		val maskedValue = value & 0x0F

		arr(hidx) = (if(!isOdd) (maskedValue << 4) | (cur & 0x0F)
		else maskedValue | (cur & 0xF0)).toByte
	}

	def num1Bits(value: Long): Int = {
		var n = 0
		for(i <- 0 until 32; if (value & (1L << i)) != 0) n += 1
		n
	}

	def inflateData(bytes: Iterable[Byte]) = blocking {
		val is = new InflaterInputStream(bytes)
		val builder = new VectorBuilder[Byte]

		var byte = is.read

		while(byte != -1) {
			builder += byte.toByte
			byte = is.read
		}

		is.close
		builder.result()
	}
}

trait Chunk extends Equals { chunk =>
	import Chunk._

	def x: Int
	def y: Int
	def z: Int

	implicit def wv: WorldView

	def typ(x: Int, y: Int, z: Int): Int
	def meta(x: Int, y: Int, z: Int): Int
	def light(x: Int, y: Int, z: Int): Int
	def skyLight(x: Int, y: Int, z: Int): Int
	def biome(x: Int, y: Int, z: Int): Int

	def isEmpty: Boolean

	def setTyp(x: Int, y: Int, z: Int, value: Int): Chunk
	def setMeta(x: Int, y: Int, z: Int, value: Int): Chunk
	def setLight(x: Int, y: Int, z: Int, value: Int): Chunk
	def setSkyLight(x: Int, y: Int, z: Int, value: Int): Chunk
	//def setBiome(x: Int, y: Int, z: Int, value: Int): Chunk

	lazy val pos: IVec3 = IVec3(x, y, z)

	override def equals(that: Any) = that match {
		case x: Chunk => pos == x.pos
		case _ => false
	}

	override def canEqual(that: Any) = that match {
		case x: Chunk => true
		case _ => false
	}
}

class MutableChunk(val x: Int, val y: Int, val z: Int,
		private var blockType: Option[Array[Byte]] = None,
		private var blockMeta: Option[Array[Byte]] = None,
		private var blockLight: Option[Array[Byte]] = None,
		private var blockSkylight: Option[Array[Byte]] = None,
		private val blockAddData: Array[Byte] = Array(),
		private val blockBiome: Option[Array[Byte]] = None
		//data: IndexedSeq[Byte]
		)(implicit val wv: WorldView) extends Chunk {
	import Chunk._

	def byteSize = dataIdx

	private var dataIdx = 0

	def isEmpty: Boolean = blockType.isEmpty

	def typ(x: Int, y: Int, z: Int): Int =
		blockType.map(_(xyzToIdx(x, y, z)).toInt).getOrElse(Block.Air.typ) & 0xFF
	def meta(x: Int, y: Int, z: Int): Int =
		blockMeta.map(xyzHalfIdxGet(x, y, z, _)).getOrElse(0)
	def light(x: Int, y: Int, z: Int): Int =
		blockLight.map(xyzHalfIdxGet(x, y, z, _)).getOrElse(0)
	def skyLight(x: Int, y: Int, z: Int): Int =
		blockSkylight.map(xyzHalfIdxGet(x, y, z, _)).getOrElse(Chunk.defaultSkyValue)
	def biome(x: Int, y: Int, z: Int): Int =
		blockBiome.map(_(z * dims.x + x) & 0xFF).getOrElse(0)

	require(255.toInt.toByte == -1, "mind... melting....")

	def setTyp(x: Int, y: Int, z: Int, value: Int) = {
		if(!blockType.isDefined)
			blockType = Some(Array.fill(numBlocks)(Block.Air.typ.toByte))
		blockType.get(xyzToIdx(x, y, z)) = (value & 0xFF).toByte
		this
	}
	def setMeta(x: Int, y: Int, z: Int, value: Int) = {
		if(!blockMeta.isDefined)
			blockMeta = Some(Array.fill[Byte](numBlocks / 2)(0))
		xyzHalfIdxSet(x, y, z, value, blockMeta.get)
		this
	}
	def setLight(x: Int, y: Int, z: Int, value: Int) = {
		if(!blockLight.isDefined)
			blockLight = Some(Array.fill[Byte](numBlocks / 2)(0))
		xyzHalfIdxSet(x, y, z, value, blockLight.get)
		this
	}
	def setSkyLight(x: Int, y: Int, z: Int, value: Int) = {
		//TODO: this should do something....
		println("set skylight does nothing")
		//xyzHalfIdxSet(x, y, z, value, blockMeta)
		this
	}
}
