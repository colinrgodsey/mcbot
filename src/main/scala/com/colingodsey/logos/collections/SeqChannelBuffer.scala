package com.colingodsey.logos.collections

import org.jboss.netty.buffer.{ChannelBufferFactory, ChannelBuffer, AbstractChannelBuffer}
import java.nio.channels.{GatheringByteChannel, ScatteringByteChannel}
import java.io.{IOException, OutputStream, InputStream}
import java.nio.{ReadOnlyBufferException, ByteBuffer, ByteOrder}

object SeqChannelBuffer {
	private final class AbstractSeqChannelBuffer(val seq: IndexedSeq[Byte]) extends SeqChannelBuffer

	def apply(seq: IndexedSeq[Byte]): SeqChannelBuffer = new AbstractSeqChannelBuffer(seq)
}

trait SeqChannelBuffer extends AbstractChannelBuffer {
	def seq: IndexedSeq[Byte]

	def toChannelBuffer: ChannelBuffer = this

	setIndex(0, seq.length)

	def roExp = throw new ReadOnlyBufferException

	def array: Array[Byte] = roExp
	def arrayOffset: Int = roExp
	def setByte(index: Int, value: Int): Unit = roExp
	def setBytes(a: Int, b: ChannelBuffer, c: Int, d: Int): Unit = roExp
	def setBytes(a: Int, b: Array[Byte], c: Int, d: Int): Unit = roExp
	def setBytes(a: Int, b: ByteBuffer): Unit = roExp
	def setShort(a: Int, b: Int): Unit = roExp
	def setMedium(a: Int, b: Int): Unit = roExp
	def setInt(a: Int, b: Int): Unit = roExp
	def setLong(a: Int, b: Long): Unit = roExp
	def setBytes(a: Int, b: InputStream, c: Int): Int = roExp
	def setBytes(a: Int, b: ScatteringByteChannel, c: Int): Int = roExp
	def factory: ChannelBufferFactory = roExp
	override def discardReadBytes: Unit = roExp

	def hasArray: Boolean = false
	def isDirect: Boolean = false
	def order: ByteOrder = ByteOrder.BIG_ENDIAN

	def getBytes(index: Int, out: GatheringByteChannel, length: Int): Int = {
		val rlength = math.min(length, seq.length - index)
		if(rlength > 0)
			out.write(toByteBuffer(index, rlength))

		rlength
	}

	def getBytes(index: Int, out: OutputStream, length: Int): Unit =
		for(i <- 0 until length) out.write(seq(index + i))

	def getBytes(index: Int, out: Array[Byte], dstIndex: Int, length: Int): Unit =
		for(i <- 0 until length) out(dstIndex + i) = seq(index + i)

	def getBytes(index: Int, out: ChannelBuffer, dstIndex: Int, length: Int): Unit =
		for(i <- 0 until length) out.setByte(dstIndex + i, seq(index + i))

	def getBytes(index: Int, out: ByteBuffer): Unit =
		for(i <- 0 until seq.length) out.put(seq(i))

	def duplicate: SeqChannelBuffer = this
	def copy(index: Int, len: Int): SeqChannelBuffer =
		SeqChannelBuffer(seq.slice(index, index + len))

	def slice(index: Int, len: Int): SeqChannelBuffer =
		SeqChannelBuffer(seq.slice(index, index + len))

	def getUnsignedMedium(index: Int): Int =
		(seq.apply(index)     & 0xff) << 16 |
		(seq.apply(index + 1) & 0xff) <<  8 |
		 seq.apply(index + 2) & 0xff

	def getInt(index: Int): Int =
		(seq.apply(index)     & 0xff) << 24 |
		(seq.apply(index + 1) & 0xff) << 16 |
		(seq.apply(index + 2) & 0xff) <<  8 |
		 seq.apply(index + 3) & 0xff
	
	def getLong(index: Int): Long =
		(seq(index).toLong     & 0xff) << 56 |
		(seq(index + 1).toLong & 0xff) << 48 |
		(seq(index + 2).toLong & 0xff) << 40 |
		(seq(index + 3).toLong & 0xff) << 32 |
		(seq(index + 4).toLong & 0xff) << 24 |
		(seq(index + 5).toLong & 0xff) << 16 |
		(seq(index + 6).toLong & 0xff) <<  8 |
		 seq(index + 7).toLong & 0xff

	def getByte(index: Int): Byte = seq(index)

	def getShort(index: Int): Short =
		(seq(index) << 8 | seq(index + 1) & 0xFF).toShort

	def toByteBuffer(index: Int, length: Int): ByteBuffer =
		ByteBuffer.wrap(seq.slice(index, index + length).toArray)

	def capacity: Int = seq.length
}
