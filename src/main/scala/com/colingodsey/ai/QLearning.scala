package com.colingodsey.ai

import scala.annotation.tailrec
import com.colingodsey.logos.collections.{Vec1D, VecCompanion, Vec}
import com.colingodsey.collections.VecN


trait Selector {
	def selectFrom[T](policy: Map[T, Double]): T
}

object BoltzmannSelector {
	def apply(temp: Double = 0.1): BoltzmannSelector = new BoltzmannSelector {
		val temperature = temp
	}

	def weightedSegmentSelect[K, V](chances: Map[K, V])
			(implicit num: Numeric[V]): K = {
		val (keys, values) = chances.unzip
		val sel = math.random * num.toDouble(values.sum)

		@tailrec def iter(acc: Double, keyList: Iterable[K]): K = {
			val key = keyList.head
			val entryValue = chances(key)
			val max = acc + num.toDouble(entryValue)

			//inside range
			if (sel <= max) key
			else iter(max, keyList.tail)
		}

		iter(0.0, keys)
	}

	val default = apply()
}

trait BoltzmannSelector extends Selector {
	def temperature: Double

	def selectFrom[T](policy: Map[T, Double]): T = {
		require(temperature > 0.0)

		val pl = policy//.toSeq.sortBy(_ => math.random)
		val distribution = pl map { case (key, x) =>
				val v = math.exp((x - pl.head._2) / temperature) + 0.01
				(key, v)
			}

		BoltzmannSelector weightedSegmentSelect distribution
	}

	override def toString = "BoltzmannSelector (" + temperature + ")"
}

/*object QLPolicyMaker {
	def apply[S](transFrom: S => Set[S],
			selector: Selector = BoltzmannSelector.default,
			qlearning: QLearning[S] = QLearning[S]()) = {
		val a = transFrom
		val b = selector
		val c = qlearning

		new QLPolicyMaker[S] {
			def transFrom(trans: S): Set[S] = a(trans)
			val selector = b
			val qLearning = c
		}
	}
}*/

trait QLPolicyMaker[T, U <: Vec] {
	def transFrom(trans: T): Set[T]
	def selector: Selector
	def qLearning: QLearning[T, U]

	def desireVector: U

	def companion: VecCompanion[U]

	//compare the end state
	def maxQ(justTransitioned: T): U = {
		val values = transFrom(justTransitioned).iterator.map(qLearning.qValue)

		//values.map(_.length)
		values.toStream.sortBy(-desireVector.normal * _).headOption.getOrElse(companion.zero)
	}

	//new q value for trans
	def update(trans: T, reward: U) =
		qLearning.update(trans, reward, maxQ(trans))

	def policy(transitions: Set[T]): T = {
		val possibleToSs = transitions.map { x =>
			(x, qLearning.qValue(x) * desireVector.normal)
		}.toMap

		selector selectFrom possibleToSs
	}
}



object QLearning {
	def apply[T](gamma: Double = 0.8, alphaScale: Double = 1.0,
			initialValue: Double = 0.0)(f: T => Double) = {
		val (a, b, c) = (gamma, alphaScale, initialValue)

		new QLearning[T, Vec1D] {
			val gamma: Double = a
			val alphaScale: Double = b
			val initialValue: Double = c

			def qValue(transition: T): Double = f(transition)
		}
	}
}

//cue, behavior, reward
trait QLearning[T, U <: Vec] {
	def gamma: Double //how much the max q of associated state is blended in
	def alphaScale: Double //familiarity
	def initialValue: U //best at 0 usually

	def qValue(transition: T): U

	/*val qTable: ConcurrentMap[T, Double] =
		new java.util.concurrent.ConcurrentHashMap[STransition, Double]
	private val timesUtilized: ConcurrentMap[STransition, Int] =
		new java.util.concurrent.ConcurrentHashMap[STransition, Int]()*/

	/*def init(initial: Map[(S, S), Double] = Map[(S, S), Double]()) =
		initial foreach {
			case (tran, v) => setQValueFor(tran, v)
		}*/

	//maxQForToS - max Q of the neighbors of the dest state
	//TODO: needs to return a vector of Q values
	//take dot product of vector
	def update(transition: T, reward: U, maxQForToS: U) = {

		val newValue = reward + maxQForToS * gamma
		val times = 1//getTimesUtilized(transition) + 1

		//familiarity, optional. less likely to commit in beginning
		val alpha = (1.0 / times) * alphaScale
		val oldValue = qValue(transition)

		newValue * (1.0 - alpha) + oldValue * alpha

		//setQValueFor(transition, currentValue)
	}
}