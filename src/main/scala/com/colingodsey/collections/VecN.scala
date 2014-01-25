package com.colingodsey.collections

import com.colingodsey.logos.collections.{VecCompanion, VecLike, Vec}

object MapVector extends VecCompanion[MapVector] {
	val origin: MapVector = MapVector()
	def one: MapVector = sys.error("No VecNOne vec... yet if ever!")

	def apply(x: Vec): MapVector = MapVector(x.toVecN.weights.toSeq: _*)

	def apply(weightSeq: (String, Double)*): MapVector = apply(weightSeq.toMap)
}

case class MapVector(override val weights: Map[String, Double]) extends VecN {
	def keySet = weights.keySet
	def fromWeights(nweights: Map[String, Double]) =
		MapVector(nweights.toSeq: _*).asInstanceOf[this.type]
	def apply(x: String): Double = weights.getOrElse(x, 0.0)

	def companion = MapVector
}

//TODO: this should store mutations internally and resolve values when retrieved
case class VecNOne(scale: Double, offset: VecN) {
	def length = ??? //fail
	def normal = ??? //fail
}

trait VecN extends Vec with VecLike[VecN] {
	def keySet: Set[String]
	def fromWeights(weights: Map[String, Double]): this.type
	def apply(x: String): Double //weights.getOrElse(x, 0.0)

	def weights = keySet.iterator.map(x => x -> apply(x)).toMap

	def *(x: VecN): Double = {
		val keys = keySet ++ x.keySet

		keys.iterator.map { k =>
			apply(k) * x(k)
		}.sum
	}

	def unary_-(): this.type = fromWeights(weights map {
		case (k, v) => k -> -v
	})
	def + (other: VecN): this.type = {
		val keys = keySet ++ other.keySet

		fromWeights(keys.iterator.map { k =>
			k -> (apply(k) + other(k))
		}.toMap)
	}
	def - (other: VecN): this.type = {
		val keys = keySet ++ other.keySet

		fromWeights(keys.iterator.map { k =>
			k -> (apply(k) - other(k))
		}.toMap)
	}
	def * (scale: Double): this.type = {
		fromWeights(weights.iterator.map { case (k, v) =>
			k -> (apply(k) * scale)
		}.toMap)
	}

	def toVec: this.type = this
	def toVecN: this.type = this

	def isAxisAligned: Boolean = weights.count(_._2 != 0) == 1
}