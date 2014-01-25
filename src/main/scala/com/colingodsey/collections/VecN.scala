package com.colingodsey.collections

import com.colingodsey.logos.collections.{VecLike, Vec}

case class VecN[T <: Equals](weights: Map[T, Double] = Map()) extends Vec with VecLike[VecN[T]] {
	def apply(x: T) = weights.getOrElse(x, 0.0)

	def *(x: VecN[T]): Double = {
		val keys = weights.keySet ++ x.weights.keySet

		keys.iterator.map { k =>
			apply(k) * x(k)
		}.sum
	}

	def unary_-(): VecN[T] = VecN(weights map {
		case (k, v) => k -> -v
	})
	def + (other: VecN[T]): VecN[T] = {
		val keys = weights.keySet ++ other.weights.keySet

		VecN apply keys.iterator.map { k =>
			k -> (apply(k) + other(k))
		}.toMap
	}
	def - (other: VecN[T]): VecN[T] = {
		val keys = weights.keySet ++ other.weights.keySet

		VecN apply keys.iterator.map { k =>
			k -> (apply(k) - other(k))
		}.toMap
	}
	def * (scale: Double): VecN[T] = {
		VecN apply weights.iterator.map { case (k, v) =>
			k -> (apply(k) * scale)
		}.toMap
	}

	def toVec: VecN[T] = this

	def isAxisAligned: Boolean = weights.count(_._2 != 0) == 1
}