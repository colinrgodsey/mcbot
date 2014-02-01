package com.colingodsey.logos.collections

import com.colingodsey.collections.{MapVector, VecN}
import scala.collection.generic.CanBuildFrom

trait Vec extends VecOps[Vec] {
	/*def unary_-(): Vec
	def + (other: Vec): Vec
	def - (other: Vec): Vec
	def * (scale: Vec): Double

	def normal: Vec

	def toVecN: VecN*/

	def toVecN: VecN
}

trait VecOps[+Repr <: Vec] {
	def unary_-(): Vec
	def * (scale: Double): Vec
	def + (other: Vec): Vec
	def - (other: Vec): Vec
	def * (other: Vec): Double

	def normal: Vec
	def length: Double
}

trait VecBuilder[+To, -From]

object AnyVecBuilder extends VecBuilder[Vec, Vec] {

}

trait VecLike[Coll <: VecLike[Coll] with Vec] extends VecOps[Coll] with Vec {
	type This = Coll

	def unary_-(): Coll
	def * (scale: Double): Coll

	//need generics that convert
    /*def + [U <: Vec](that: U)(implicit builder: CanBuildFrom[This, Double, U]): U =
		this.to[U] + that

	def to[U <: Vec](implicit builder: CanBuildFrom[This, Double, U]): U*/

	def + (other: Coll): Coll
	def - (other: Coll): Coll
	def * (other: Coll): Double

	def + (other: Vec): Coll = companion(other) + toVec
	def - (other: Vec): Coll = companion(other) - toVec
	def * (other: Vec): Double = companion(other) * toVec

	def isNormal = length == 1
	def / (scale: Double): Coll = this * (1.0 / scale)

	def isOrigin: Boolean = (toVec * toVec) == 0

    def toVec: Coll

	def companion: VecCompanion[This]

	//TODO: add isNormal, effecient normals

    //def map(f: Double => Double): PointType

    //def dimensions: Dimensions

	def isAxisAligned: Boolean

    def length: Double = {
	    val r = math.sqrt(toVec * toVec)
	    require(!r.isNaN, toString + " made a NaN")
	    r
    }

    def normal: Coll = {
        val l = length
	    require(l != 0, "cant take a normal of a zero length vector!")
        if(l == 1) toVec else toVec / l
    }

	def clamp(l: Double): This = if(length > l) normal * l else toVec

    def ~~(other: This)(implicit epsilon: Epsilon): Boolean = {
        val sub = this - other

        sub.isOrigin || math.abs(sub.length) < epsilon.e
    }

	def !~~(other: This)(implicit epsilon: Epsilon): Boolean =
		!this.~~(other)
}

trait VecCompanion[+VecType <: Vec] {
    def origin: VecType
	def unit: VecType

	def apply(x: Vec): VecType

    //def dimensions: Dimensions

	def one = unit
	def zero = origin
}

case class Epsilon(e: Double)

object Epsilon {
    implicit val default = Epsilon(1e-13)
}

object Vec1 extends VecCompanion[Vec1] {
	val origin = Vec1(0)
	val unit = Vec1(1)

	def apply(x: Vec): Vec1 = x match {
		case x: Vec1 => x
		case x => Vec1(x.toVecN("x"))
	}

	def dimensions: Dimensions = Dimensions.Two

	implicit def dTov1d(d: Double): Vec1 = Vec1(d)
	implicit def v1dToD(x: Vec1): Double = x.x
}

final case class Vec1(x: Double) extends Vec with VecLike[Vec1] {
	//only able to force this.type here because of final class
	def unary_-() = Vec1(-x).asInstanceOf[this.type]
	def + (other: Vec1) = Vec1(x + other.x).asInstanceOf[this.type]
	def - (other: Vec1) = Vec1(x - other.x).asInstanceOf[this.type]
	def * (scale: Double) = Vec1(x * scale).asInstanceOf[this.type]
	def * (other: Vec1): Double = x * other.x

	override def isOrigin = this == Vec1.origin
	def toVec = this

	def companion = Vec1

	lazy val isAxisAligned: Boolean = true

	override def length = x
	override lazy val normal: this.type = super.normal.asInstanceOf[this.type]

	def toVecN = MapVector("x" -> x)
}

object Vec2 extends VecCompanion[Vec2] {
    val origin = Vec2(0, 0)
    val unit = Vec2(1, 1)

    def dimensions: Dimensions = Dimensions.Two

	def apply(x: Vec): Vec2 = x match {
		case x: Vec2 => x
		case x =>
			val vn = x.toVecN
			Vec2(vn("x"), vn("y"))
	}
}

final case class Vec2(x: Double, y: Double) extends Vec with VecLike[Vec2] {
    final def unary_-() = Vec2(-x, -y)
    def + (other: Vec2) = Vec2(x + other.x, y + other.y).asInstanceOf[this.type]
    def - (other: Vec2) = Vec2(x - other.x, y - other.y).asInstanceOf[this.type]
    def * (scale: Double) = Vec2(x * scale, y * scale).asInstanceOf[this.type]
    def * (other: Vec2): Double = x * other.x + y * other.y

	override def isOrigin = this == Vec2.origin
    def toVec = this

	def companion = Vec2

	lazy val isAxisAligned: Boolean = Seq(x, y).count(_ != 0) == 1

    override lazy val length = super.length
	override lazy val normal = super.normal

	def toVecN = MapVector("x" -> x, "y" -> y)
}

object Vec3 extends VecCompanion[Vec3] {
	val origin = Vec3(0, 0, 0)
	val unit = Vec3(1, 1, 1)

	def random = (Vec3(math.random, math.random,
		math.random) * 2 - Vec3.one).normal

	def dimensions: Dimensions = Dimensions.Two

	def apply(x: Vec): Vec3 = x match {
		case x: Vec3 => x
		case x =>
			val vn = x.toVecN
			Vec3(vn("x"), vn("y"), vn("z"))
	}
}

final case class Vec3(x: Double, y: Double, z: Double) extends Vec with VecLike[Vec3] {
	def unary_-() = Vec3(-x, -y, -z).asInstanceOf[this.type]
	def + (other: Vec3) = Vec3(x + other.x, y + other.y, z + other.z).asInstanceOf[this.type]
	def - (other: Vec3) = Vec3(x - other.x, y - other.y, z - other.z).asInstanceOf[this.type]
	def * (scale: Double) = Vec3(x * scale, y * scale, z * scale).asInstanceOf[this.type]
	def * (other: Vec3): Double = x * other.x + y * other.y + z * other.z

	require(!x.isNaN && !y.isNaN && !z.isNaN, "no NaNs for points/vectors!")

	override def isOrigin = this == Vec3.origin

	def toVec = this
	def companion = Vec3

	lazy val isAxisAligned: Boolean = Seq(x, y, z).count(_ != 0) == 1

	override lazy val length = super.length
	override lazy val normal = super.normal

	def toVecN = MapVector("x" -> x, "y" -> y, "z" -> z)

	//def apply(other: Point3D) = Point3D(x * other.x, y * other.y, z * other.z)
}