package com.colingodsey.logos.collections

import java.nio.ByteBuffer
import scala.collection.IndexedSeqOptimized

final class ByteBufferSeq(buf: ByteBuffer) extends IndexedSeq[Byte]
		with IndexedSeqOptimized[Byte, IndexedSeq[Byte]] {
	def apply(idx : Int): Byte = buf.get(idx)
	def length = buf.remaining

	override def stringPrefix: String = "ByteBufferSeq"

	override def seq = this

	override def slice(start: Int, end: Int): IndexedSeq[Byte] = Cord(this).slice(start, end)

	//protected[this] def newBuilder: mutable.Builder[Byte, IndexedSeq[Byte]] = IndexedSeq.newBuilder[Byte]

	//def seq: IndexedSeq[Byte] = this
}
