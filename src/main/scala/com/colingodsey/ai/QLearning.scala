package com.colingodsey.ai

import scala.annotation.tailrec
import com.colingodsey.logos.collections._
import com.colingodsey.collections.VecN


trait Selector {
	def selectFrom[T](policy: Map[T, Double]): T

	def selectFrom[T](vals: Set[T])(f: T => Double): T =
		selectFrom(vals.map(x => x -> f(x)).toMap)
}

object BoltzmannSelector {
	def weightedSegmentSelect[K, V](chances0: Map[K, V])
			(implicit num: Numeric[V]): K = {

		val chances = chances0 map { case (k, v) =>
			k -> math.max(num toDouble v, 0)
		}
		val (keys0, values) = chances.unzip
		val keys = keys0.toSeq.sortBy(_ => math.random)
		val sel = math.random * values.sum

		@tailrec def iter(acc: Double, keyList: Iterable[K]): K = {
			val key = keyList.head
			val entryValue = chances(key)
			val max = acc + entryValue

			//inside range
			if (sel <= max) key
			else iter(max, keyList.tail)
		}

		iter(0.0, keys)
	}

	lazy val default = apply()
}

case class BoltzmannSelector(temperature: Double = 0.1) extends Selector {
	def selectFrom[T](policy: Map[T, Double]): T = {
		require(temperature > 0.0)

		val pl = policy//.toSeq.sortBy(_ => math.random)
		val distribution = pl map { case (key, x) =>
				val v = math.exp((x - pl.head._2) / temperature)
				(key, v)
			}

		BoltzmannSelector weightedSegmentSelect distribution
	}

}
/*
object QLPolicy {
	def apply[T, U <: VecLike[U]](initialValue: U, desire: U,
			gamma: Double = 0.8, alphaScale: Double = 1.0,
			selector: Selector = BoltzmannSelector.default)
			(f: T => U)(transF: T => Set[T]) = {
		val dat = (gamma, alphaScale, initialValue, selector, desire)

		new QLPolicy[T, U] {
			val (γ, α0, initialValue,
				selector, desire) = dat
			def transFrom(trans: T): Set[T] = transF(trans)

			def qValue(transition: T): U = f(transition)
		}
	}
}

trait QLPolicy[T, U <: VecLike[U]] extends QLearning[T, U] {
	def transFrom(trans: T): Set[T]
	def selector: Selector

	def desire: U

	//compare the end state
	def maxQ(justTransitioned: T): U = {
		val values = transFrom(justTransitioned).iterator.map(qValue)

		values.toStream.sortBy(-desire.normal * _).headOption.getOrElse(initialValue)
	}

	//new q value for trans
	def update(trans: T, reward: U): U =
		update(trans, reward, maxQ(trans))

	def policy(transitions: Set[T]): T =
		selector.selectFrom(transitions) { x =>
			qValue(x) * desire.normal
		}
}

object QLearning {
	/*def apply[T, U <: VecLike[U]](initialValue: U, gamma: Double = 0.8,
			alphaScale: Double = 1.0)(f: T => U) = {
		val dat = (gamma, alphaScale, initialValue)

		new QLearning[T, U] {
			val (γ, α0, initialValue) = dat

			def qValue(transition: T): U = f(transition)
		}
	}*/
}

//cue, behavior, reward
trait QLearning[T, U <: VecLike[U]] extends QLearningValues {
	def initialValue: U //best at 0 usually

	def qValue(transition: T): U
	def desire: U

	//maxQForToS - max Q of the neighbors of the dest state
	//TODO: needs to return a vector of Q values
	//take dot product of vector
	def update(transition: T, reward: U, maxQForDest: U): U = {

		//familiarity, optional. less likely to commit initially. affects α
		val times = 1//getTimesUtilized(transition) + 1

		val α = (1.0 / times) * α0
		val q0 = qValue(transition)
		val q1 = maxQForDest * γ

		//println(q0, q1, reward, α, q0 * α + q1 * (1.0 - α))

		//TODO: should we align the reward and 'ignore' unfocused rewards?
		//or do we just add the whole thing

		val alignedQ0 = desire.normal * (q0 * desire.normal)
		//portion of the vector not aligned with desires
		val remainingQ0 = q0 - alignedQ0
		//val alignedAdjMaxQ = desire.normal * (adjustedMaxQ * desire.normal)

		//new q value for transition
		reward + q0 * α + q1 * (1.0 - α)

		//remainingQ0 + alignedQ0 * α + (alignedAdjMaxQ + reward) * (1.0 - α)
		//remainingQ0 + alignedQ0 * α + (q1 + reward) * (1.0 - α)
	}
}*/

trait QLearningValues {
	def γ: Double //gamma, how much the max q of associated state is blended in
	def α0: Double //alpha, familiarity

	def gamma = γ
	def alpha0 = α0
}