package com.colingodsey.ai

import scala.annotation.tailrec


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

object QLPolicyMaker {
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
}

trait QLPolicyMaker[T] {
	def transFrom(trans: T): Set[T]
	def selector: Selector
	def qLearning: QLearning[T]

	//compare the end state
	def maxQ(justTransitioned: T): Double = {
		val values = transFrom(justTransitioned).iterator.map(qLearning.qValue)

		values.foldLeft(qLearning.initialValue)(math.max)
	}

	//new q value for trans
	def update(trans: T, reward: Double) =
		qLearning.update(trans, reward, maxQ(trans))

	def policy(transitions: Set[T]): T = {
		val possibleToSs = transitions.map { x =>
			(x, qLearning.qValue(x))
		}.toMap

		selector selectFrom possibleToSs
	}
}

case class RewardVector[T](weights: Map[T, Double]) extends QLearning.TReward {
	def apply(x: T) = weights.getOrElse(x, 0.0)

	def *(x: RewardVector): Double = {
		val keys = weights.keySet ++ x.weights.keySet

		keys.iterator.map { k =>
			apply(k) * x(k)
		}.sum
	}
	def *(x: Double): this.type
	def -(x: this.type): this.type
	def +(x: this.type): this.type
}

object QLearning {
	def apply[T](gamma: Double = 0.8, alphaScale: Double = 1.0,
			initialValue: Double = 0.0)(f: T => Double) = {
		val (a, b, c) = (gamma, alphaScale, initialValue)

		new QLearning[T] {
			val gamma: Double = a
			val alphaScale: Double = b
			val initialValue: Double = c

			def qValue(transition: T): Double = f(transition)
		}
	}

	type Reward = {
		def *(x: Reward.this.type): Double
		def *(x: Double): Reward.this.type
		def -(x: Reward.this.type): Reward.this.type
		def +(x: Reward.this.type): Reward.this.type
	}

	trait TReward extends Reward {
		//TODO: I have no idea if this is right....
		lazy val length = math.sqrt(this * this)
	}
}

trait QLearning[T] {
	import QLearning.Reward

	def gamma: Double //how much the max q of associated state is blended in
	def alphaScale: Double //familiarity
	def initialValue: Double //best at 0 usually

	def qValue(transition: T): Double

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
	def update(transition: T, reward: Reward, maxQForToS: Double) = {

		val newValue = reward + gamma * maxQForToS
		val times = 1//getTimesUtilized(transition) + 1

		//familiarity, optional. less likely to commit in beginning
		val alpha = (1.0 / times) * alphaScale
		val oldValue = qValue(transition)

		val currentValue = (1.0 - alpha) * newValue + alpha * oldValue

		//setQValueFor(transition, currentValue)
		currentValue
	}
}