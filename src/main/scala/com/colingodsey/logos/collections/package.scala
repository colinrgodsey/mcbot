package com.colingodsey.logos

import org.jboss.netty.buffer.ChannelBuffer
import scala.collection.{IndexedSeqOptimized, immutable}

import java.nio.ByteBuffer
import akka.actor.ActorLogging
import akka.event.LoggingAdapter

package object collections {
	final case class IPoint3D(x: Int, y: Int, z: Int) {
		def toPoint3D = Point3D(x, y, z)
	}

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
}
