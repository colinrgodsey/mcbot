package com.colingodsey.logos

import org.jboss.netty.buffer.ChannelBuffer
import scala.collection.{IndexedSeqOptimized, immutable}

import java.nio.ByteBuffer
import akka.actor.ActorLogging
import akka.event.LoggingAdapter
import java.io.InputStream

package object collections {
	object IVec3 {
		def apply(point: Vec3): IVec3 = IVec3(
			math.floor(point.x).toInt,
			math.floor(point.y).toInt,
			math.floor(point.z).toInt)
	}

	final case class IVec3(x: Int, y: Int, z: Int) {
		def toVec3 = Vec3(x, y, z)
	}

	implicit def IPoint3DToPoint3D(x: IVec3) = x.toVec3

	implicit final class ChannelBufferSeq(buf: ChannelBuffer) extends
			immutable.IndexedSeq[Byte] with IndexedSeqOptimized[Byte, immutable.IndexedSeq[Byte]] {
		val offset = buf.readerIndex
		val length = buf.readableBytes

		override def seq = this

		def apply(idx: Int) = buf.getByte(offset + idx)

		override def slice(from: Int, until: Int): immutable.IndexedSeq[Byte] =
			buf.slice(from + offset, until - from)

		override def stringPrefix = "ChannelBufferSeq"
	}

	implicit def seqToChannelBuffer(seq: IndexedSeq[Byte]): SeqChannelBuffer =
		SeqChannelBuffer(seq)

	implicit def byteBufferSeq(buf: ByteBuffer): ByteBufferSeq = new ByteBufferSeq(buf)

	implicit class ActorLoggingExt(val ac: LoggingAdapter) extends AnyVal {
		def ifdebug(msg: => String) {
			if(ac.isDebugEnabled) ac.debug(msg)
		}
	}

	implicit class SeqInputStream(seq: Iterable[Byte]) extends InputStream {
		private val iterator = seq.iterator

		private var _position = 0

		def position = _position

		def read: Int = {
			if(!iterator.hasNext) return -1

			_position += 1

			iterator.next & 0xFF
		}
	}
}
