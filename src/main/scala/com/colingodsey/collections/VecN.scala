package com.colingodsey.collections

import com.colingodsey.logos.collections.{VecNumeric, VecCompanion, VecLike, Vec}

trait MapVectorCompanion[+T >: MapVector <: Vec] extends VecCompanion[T] {
	val origin: T = MapVector()

	def apply(x: Vec): T = MapVector(x.toVecN.weights.toSeq: _*)

	def apply(weightSeq: (String, Double)*): T = MapVector(weightSeq.toMap)

	def apply(weights: Map[String, Double]): T
}

object MapVector extends MapVectorCompanion[MapVector] with VecNumeric[MapVector] {
	def unit: MapVector = sys.error("No unit MapVector!")
}

case class MapVector(override val weights: Map[String, Double]) extends VecN {
	def keySet = weights.keySet
	def fromWeights(nweights: Map[String, Double]): MapVector =
		MapVector(nweights.toSeq: _*).asInstanceOf[this.type]
	def apply(x: String): Double = weights.getOrElse(x, 0.0)

	def companion: MapVectorCompanion[MapVector] = MapVector
}

//TODO: this should store mutations internally and resolve values when retrieved
case class VecNUnit(scale: Double = 1, offset: Double = 0) extends VecN {
	override def length: Double = Double.PositiveInfinity
	override def normal: VecN = sys.error("No normal for VecNOne") //technically VeN.origin

	override def companion = VecN

	def apply(x: String): Double = scale + offset

	override def keySet: Set[String] = sys.error("Infinite keys for unit VecN")
}

object VecN extends MapVectorCompanion[VecN] with VecNumeric[VecN] {
	def unit: VecN = VecNUnit()//sys.error("No VecNOne vec... yet if ever!")

	override def apply(weights: Map[String, Double]): VecN = MapVector(weights)
}

trait VecN extends Vec with VecLike[VecN] {
	def keySet: Set[String]

	def companion: MapVectorCompanion[VecN]

	def apply(x: String): Double //weights.getOrElse(x, 0.0)

	def weights = keySet.iterator.map(x => x -> apply(x)).toMap

	def *(x: VecN): Double = {
		val keys = keySet ++ x.keySet

		keys.iterator.map { k =>
			apply(k) * x(k)
		}.sum
	}

	def unary_-(): VecN = companion(weights map {
		case (k, v) => k -> -v
	})
	def + (other: VecN): VecN = {
		val keys = keySet ++ other.keySet

		companion(keys.iterator.map { k =>
			k -> (apply(k) + other(k))
		}.toMap)
	}
	def - (other: VecN): VecN = {
		val keys = keySet ++ other.keySet

		companion(keys.iterator.map { k =>
			k -> (apply(k) - other(k))
		}.toMap)
	}
	def * (scale: Double): VecN = {
		companion(weights.iterator.map { case (k, v) =>
			k -> (apply(k) * scale)
		}.toMap)
	}

	def toVec: VecN = this
	def toVecN: VecN = this

	def isAxisAligned: Boolean = weights.count(_._2 != 0) == 1
}