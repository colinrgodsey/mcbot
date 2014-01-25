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
	def unary_-(): Repr
	def * (scale: Double): Repr
	def + (other: Vec): Repr
	def - (other: Vec): Repr
	def * (other: Vec): Double

	def normal: Repr
	def length: Double
}

trait VecBuilder[+To, -From]

object AnyVecBuilder extends VecBuilder[Vec, Vec] {

}

trait VecLike[Coll <: VecLike[Coll] with Vec] extends VecOps[Coll] with Vec {
	type This = Coll

	def unary_-(): This
	def * (scale: Double): This

	//need generics that convert
    /*def + [U <: Vec](that: U)(implicit builder: CanBuildFrom[This, Double, U]): U =
		this.to[U] + that

	def to[U <: Vec](implicit builder: CanBuildFrom[This, Double, U]): U*/

	def + (other: Coll): This
	def - (other: Coll): This
	def * (other: Coll): Double

	def + (other: Vec): This = companion(other) + toVec
	def - (other: Vec): This = companion(other) - toVec
	def * (other: Vec): Double = companion(other) * toVec

	def isNormal = length == 1
	def / (scale: Double): This = this * (1.0 / scale)

	def isOrigin: Boolean = (toVec * toVec) == 0

    def toVec: This

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

    def normal: This = {
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
    def one: VecType

	def apply(x: Vec): VecType

    //def dimensions: Dimensions

	def zero = origin
}

case class Epsilon(e: Double)

object Epsilon {
    implicit val default = Epsilon(1e-13)
}

object Vec1D extends VecCompanion[Vec1D] {
	val origin = Vec1D(0)
	val one = Vec1D(1)

	def apply(x: Vec): Vec1D = x match {
		case x: Vec1D => x
		case x => Vec1D(x.toVecN("x"))
	}

	def dimensions: Dimensions = Dimensions.Two

	implicit def dTov1d(d: Double): Vec1D = Vec1D(d)
	implicit def v1dToD(x: Vec1D): Double = x.x
}

final case class Vec1D(x: Double) extends Vec with VecLike[Vec1D] {
	//only able to force this.type here because of final class
	def unary_-() = Vec1D(-x).asInstanceOf[this.type]
	def + (other: Vec1D) = Vec1D(x + other.x).asInstanceOf[this.type]
	def - (other: Vec1D) = Vec1D(x - other.x).asInstanceOf[this.type]
	def * (scale: Double) = Vec1D(x * scale).asInstanceOf[this.type]
	def * (other: Vec1D): Double = x * other.x

	override def isOrigin = this == Vec1D.origin
	def toVec = this

	def companion = Vec1D

	lazy val isAxisAligned: Boolean = true

	override def length = x
	override lazy val normal: this.type = super.normal.asInstanceOf[this.type]

	def toVecN = MapVector("x" -> x)
}

object Vec2D extends VecCompanion[Vec2D] {
    val origin = Vec2D(0, 0)
    val one = Vec2D(1, 1)

    def dimensions: Dimensions = Dimensions.Two

	def apply(x: Vec): Vec2D = x match {
		case x: Vec2D => x
		case x =>
			val vn = x.toVecN
			Vec2D(vn("x"), vn("y"))
	}
}

final case class Vec2D(x: Double, y: Double) extends Vec with VecLike[Vec2D] {
    final def unary_-() = Vec2D(-x, -y)
    def + (other: Vec2D) = Vec2D(x + other.x, y + other.y).asInstanceOf[this.type]
    def - (other: Vec2D) = Vec2D(x - other.x, y - other.y).asInstanceOf[this.type]
    def * (scale: Double) = Vec2D(x * scale, y * scale).asInstanceOf[this.type]
    def * (other: Vec2D): Double = x * other.x + y * other.y

	override def isOrigin = this == Vec2D.origin
    def toVec = this

	def companion = Vec2D

	lazy val isAxisAligned: Boolean = Seq(x, y).count(_ != 0) == 1

    override lazy val length = super.length
	override lazy val normal = super.normal

	def toVecN = MapVector("x" -> x, "y" -> y)
}

object Vec3D extends VecCompanion[Vec3D] {
	val origin = Vec3D(0, 0, 0)
	val one = Vec3D(1, 1, 1)

	def random = (Vec3D(math.random, math.random,
		math.random) * 2 - Vec3D.one).normal

	def dimensions: Dimensions = Dimensions.Two

	def apply(x: Vec): Vec3D = x match {
		case x: Vec3D => x
		case x =>
			val vn = x.toVecN
			Vec3D(vn("x"), vn("y"), vn("z"))
	}
}

final case class Vec3D(x: Double, y: Double, z: Double) extends Vec with VecLike[Vec3D] {
	def unary_-() = Vec3D(-x, -y, -z).asInstanceOf[this.type]
	def + (other: Vec3D) = Vec3D(x + other.x, y + other.y, z + other.z).asInstanceOf[this.type]
	def - (other: Vec3D) = Vec3D(x - other.x, y - other.y, z - other.z).asInstanceOf[this.type]
	def * (scale: Double) = Vec3D(x * scale, y * scale, z * scale).asInstanceOf[this.type]
	def * (other: Vec3D): Double = x * other.x + y * other.y + z * other.z

	require(!x.isNaN && !y.isNaN && !z.isNaN, "no NaNs for points/vectors!")

	override def isOrigin = this == Vec3D.origin

	def toVec = this
	def companion = Vec3D

	lazy val isAxisAligned: Boolean = Seq(x, y, z).count(_ != 0) == 1

	override lazy val length = super.length
	override lazy val normal = super.normal

	def toVecN = MapVector("x" -> x, "y" -> y, "z" -> z)

	//def apply(other: Point3D) = Point3D(x * other.x, y * other.y, z * other.z)
}