package com.colingodsey.logos.collections

import scala.collection.generic.{GenericCompanion, GenericTraversableTemplate, CanBuildFrom, SeqFactory}
import scala.collection.immutable.VectorBuilder
import scala.collection.{GenTraversableOnce, IndexedSeqLike, IndexedSeqOptimized, mutable}

object Cord extends SeqFactory[Cord] {
	def apply[@specialized(Byte) T](seq: IndexedSeq[T]*): Cord[T] = {
		val b = newBuilder[T]

		seq foreach b.++=

		b.result
	}

	private class CordReusableCBF extends GenericCanBuildFrom[Nothing] {
		override def apply() = newBuilder[Nothing]
	}

	private val CordReusableCBF: GenericCanBuildFrom[Nothing] = new CordReusableCBF

	override lazy val ReusableCBF = CordReusableCBF
	//scala.collection.IndexedSeq.ReusableCBF.asInstanceOf[GenericCanBuildFrom[Nothing]]

	def newBuilder[A]: mutable.Builder[A, Cord[A]] = new CordBuilder[A]

	implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, Cord[A]] =
		CordReusableCBF.asInstanceOf[GenericCanBuildFrom[A]]

	private val NIL = new Cord[Nothing](Nil.toIndexedSeq, 0)
	override def empty[A]: Cord[A] = NIL
}

final class CordBuilder[A] extends mutable.Builder[A, Cord[A]] {
	def chunkMin = 512
	//def chunkMaxAppend = 512

	private val seqsBuilder = new VectorBuilder[IndexedSeq[A]]()
	private val vecBuilder = new VectorBuilder[A]()
	private var lastVec: Option[IndexedSeq[A]] = None

	private var len = 0
	private var buffered = 0

	private def buffering = buffered != 0

	vecBuilder.sizeHint(chunkMin)

	def +=(elem: A): this.type = {
		if(lastVec.isDefined) {
			if(buffered < chunkMin) {
				//copy last intp accum buffer
				lastVec.get foreach vecBuilder.+=
				lastVec = None
			} else {
				//append last, start new accum
				appendFinalChunk(lastVec.get)
				lastVec = None
				buffered = 0
			}
		}

		vecBuilder += elem
		buffered += 1

		this
	}

	override def ++=(xs: TraversableOnce[A]): this.type = {
		if(xs.isEmpty) return this

		xs match {
			case x: Cord[A] if !x.isSliced =>
				x.seqs.foreach(appendChunk)
			case x: IndexedSeq[A] => appendChunk(x)
			case x =>
				//appendChunk(x.toIndexedSeq)
				x foreach +=
		}

		this
	}

	def result: Cord[A] = {
		flushBuffer

		val res = new Cord(seqsBuilder.result, len)

		//println(res.trie)

		res
	}

	def clear {
		seqsBuilder.clear
		vecBuilder.clear
		//vecAccum = Vector()
		buffered = 0
		len = 0
		lastVec = None
	}

	@inline private def flushBuffer = if(buffering) {
		if(lastVec.isDefined)
			appendFinalChunk(lastVec.get)
		else
			appendFinalChunk(vecBuilder.result)

		lastVec = None
		vecBuilder.clear
		buffered = 0
	}

	@inline private def appendChunk(chunk: IndexedSeq[A]) {
		if(lastVec.isDefined && (buffered + chunk.length) < chunkMin) {
			//start accum buffer
			lastVec.get foreach vecBuilder.+=
			lastVec = None
			chunk foreach vecBuilder.+=
			buffered += chunk.length
		} else {
			flushBuffer
			lastVec = Some(chunk)
			buffered = chunk.length
		}
	}

	@inline private def appendFinalChunk(chunk: IndexedSeq[A]) {
		seqsBuilder += chunk
		len += chunk.length
	}
}

final class Cord[@specialized(Byte) +T](
		val seqs: IndexedSeq[IndexedSeq[T]],
		val length: Int,
		private[collections] val isSliced: Boolean = false,
		private val offset: Int = 0,
		private val inTrie: Option[IntIndexTrie[T]] = None) extends IndexedSeq[T]
			with scala.collection.immutable.IndexedSeq[T]
			with IndexedSeqOptimized[T, Cord[T]]
			with GenericTraversableTemplate[T, Cord]
			with IndexedSeqLike[T, Cord[T]] {
	require(!(!isSliced && offset != 0))

	private[collections] lazy val trie = inTrie.getOrElse(new IntIndexTrie(seqs))

	//@inline final def isSliced = offset != 0 || lengthOpt.isDefined

	override def companion = Cord
	override def stringPrefix = "Cord"

	override def copyToArray[B >: T](xs: Array[B]) {
		var idx = 0
		iterator foreach { x =>
			xs(idx) = x
			idx += 1
		}
	}

	override def iterator: Iterator[T] = if(isSliced || isEmpty) super.iterator
	else {
		var seqIdx = 0
		var curIter = seqs(seqIdx).iterator

		Iterator.fill(length) {
			if(!curIter.hasNext) {
				seqIdx += 1
				curIter = seqs(seqIdx).iterator
			}

			curIter.next
		}
	}

	def concat[U >: T](seq: IndexedSeq[U]): Cord[U] =
		(Cord.newBuilder[U] ++= this ++= seq).result

	override def ++[B >: T, That](that: GenTraversableOnce[B])(
			implicit bf: CanBuildFrom[Cord[T], B, That]): That =
		if((bf eq IndexedSeq.ReusableCBF) || (bf eq Seq.ReusableCBF))
			concat(that.toIndexedSeq).asInstanceOf[That]
		else super.++[B, That](that)

	@inline final def ++[U >: T](that: IndexedSeq[U]): Cord[U] = concat(that)

	@inline final def <->[U >: T](that: IndexedSeq[U]): Cord[U] = concat(that)

	@inline final def :+[U >: T](x: U): Cord[U] =
		(Cord.newBuilder[U] ++= this += x).result

	@inline final def +:[U >: T](x: U): Cord[U] =
		(Cord.newBuilder[U] += x ++= this).result

	@inline final override def slice(from: Int, until: Int): Cord[T] = {
		val l = until - from
		val o = from + offset

		require(from <= length, "from > length!")
		require(until <= length, "until > length")

		new Cord(seqs, l, offset = o,
			isSliced = true, inTrie = Some(trie))
	}

	/*override def drop(n: Int): Cord[T] =
		if (n <= 0) sys.error("Cant drop " + n)//super.drop(n)
		else if(n == 0) this
		else slice(n, length - n)*/

	@inline final def apply(idx: Int): T = {
		require(idx < length, s"Cannot get idx $idx of Cord len $length")

		val realIdx = idx + offset

		//up to and including idx
		trie(realIdx)
	}

	def debugString = trie.toString
}

object IntIndexTrie {
	sealed abstract class TrieNode[+T] {
		//def startIdx: Int
		def length: Int
		def apply(idx: Int): T

		def isEmpty: Boolean

	}

	/**
	 *
	 * @param bitIdx the bit that will be applied to get the one value
	 * @param zero node containing indexes for bitMask
	 * @param one node containing indexes for bitMask | oneMask
	 * @tparam T item type
	 */
	final case class TrieBranch[T](bitIdx: Int,
			zero: TrieNode[T], one: TrieNode[T]) extends TrieNode[T] {
		//lazy val length = left.length + right.length
		val oneMask = 1 << bitIdx  //0x001000
		val oneFullMask = (1 << (bitIdx + 1)) - 1 //0x001111

		@inline final def isEmpty: Boolean = length == 0

		//def startIdx = leftMask
		val length = zero.length + one.length

		def apply(idx: Int): T = {
			val isOne = (idx & oneMask) != 0

			if(isOne) one(idx)
			else zero(idx)
		}


	}
	final case class TrieLeaf[T](indexOff: Int, seq: IndexedSeq[T]) extends TrieNode[T] {
		require(!seq.isEmpty)

		//def startIdx = indexOff
		@inline final def length = seq.length
		@inline final def isEmpty: Boolean = length == 0

		val endIdx = indexOff + length

		@inline final def apply(idx: Int): T = seq(idx - indexOff)

		//def contains(idx: Int) = idx >= indexOff && idx < endIdx
		@inline final def outsideOf(sIdx: Int, eIdx: Int) =
			(eIdx <= indexOff) || (sIdx >= endIdx)
		@inline final def intersects(sIdx: Int, eIdx: Int) = !outsideOf(sIdx, eIdx)

		override def toString = s"TrieLeaf($indexOff, ${seq.stringPrefix}[len: ${seq.length}])"

	}

	final case object TrieTerminator extends TrieNode[Nothing] {
		//def startIdx: Int = ???
		def length: Int = 0
		def isEmpty: Boolean = true

		def apply(idx: Int): Nothing = ???
	}
}

final class IntIndexTrie[@specialized(Byte) +T](val seqs: Seq[IndexedSeq[T]]) extends IntIndexTrieLike[T]

//split an int bitwise from hi to low bit
trait IntIndexTrieLike[+T] extends IntIndexTrie.TrieNode[T] with Iterable[T] {
	import IntIndexTrie._

	def seqs: Seq[IndexedSeq[T]]

	lazy val length: Int = seqs.foldLeft(0)((acc, x) => acc + x.length)

	def iterator = seqs.foldLeft(Iterator[T]())((acc, x) => acc ++ x.iterator)

	def apply(idx: Int): T = headNode(idx)

	//override def toString = toJSON.toString

	lazy val numSigBits = {
		var len: Int = length
		var bits = 0

		while(len != 0) {
			len = len >> 1
			bits = bits + 1
		}

		bits
	}

	//for each bit, finds nodes that contain both 0 and 1 for that bit
	private def create[U >: T](bitIdx: Int, leftMask: Int, leafs: Seq[TrieLeaf[U]]): TrieNode[U] = {
		if(leafs.length == 1) leafs.head
		else if(leafs.isEmpty || bitIdx < 0) TrieTerminator
		else {
			val segSize = 1 << bitIdx
			val oneMask = segSize + leftMask
			val zeroMask = leftMask
			var zeros = Vector[TrieLeaf[U]]()
			var ones = Vector[TrieLeaf[U]]()

			val iterator = leafs.iterator
			while(iterator.hasNext) {
				val x = iterator.next

				if(x.intersects(zeroMask, oneMask)) zeros :+= x
				if(x.intersects(oneMask, oneMask + segSize)) ones :+= x
			}

			val zero = create(bitIdx - 1, zeroMask, zeros)
			val one = create(bitIdx - 1, oneMask, ones)

			if(zero.isEmpty) one
			else if(one.isEmpty) zero
			else TrieBranch(bitIdx, zero, one)
		}
	}

	private lazy val headNode = {
		var leafs = Vector[TrieLeaf[T]]()
		var pos = 0

		seqs foreach { seq =>
			leafs :+= TrieLeaf[T](pos, seq)
			pos += seq.length
		}

		require(numSigBits > 0)
		create(math.min(numSigBits + 1, Integer.SIZE - 2), 0, leafs)
	}
}