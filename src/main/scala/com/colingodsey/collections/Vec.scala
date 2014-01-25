package com.colingodsey.logos.collections

trait Vec extends VecLike[Vec] {
	/*def unary_-(): this.type

	def + (other: Vec): this.type
	def - (other: Vec): this.type
	def * (scale: Double): this.type
	def * (scale: Vec): Double*/
}
//trait Vec { _: VecLike[_ <: Vec] =>}

trait VecLike[+VecType] {
    def unary_-(): this.type
    def + (other: VecType): this.type
    def - (other: VecType): this.type
    def * (scale: Double): this.type
    def * (vec: VecType): Double

	def clamp(l: Double): this.type = if(length > l) normal * l else this

    def toVec: VecType

	def companion: VecCompanion[VecType]

	//TODO: add isNormal, effecient normals

    //def map(f: Double => Double): PointType

    //def dimensions: Dimensions

	def isAxisAligned: Boolean

    def length: Double = {
	    val r = math.sqrt(this * toVec)
	    require(!r.isNaN, this.toString + " made a NaN")
	    r
    }
    def isOrigin: Boolean = (this * toVec) == 0

    def / (scale: Double): this.type = this * (1.0 / scale)

    def normal: this.type = {
        val l = length
	    require(l != 0, "cant take a normal of a zero length vector!")
        if(l == 1) this else this / l
    }

    def isNormal = length == 1

    def ~~(other: VecType)(implicit epsilon: Epsilon): Boolean = {
        val sub = this - other

        sub.isOrigin || math.abs(sub.length) < epsilon.e
    }

	def !~~(other: VecType)(implicit epsilon: Epsilon): Boolean =
		!this.~~(other)
}

trait VecCompanion[+VecType <: Vec] {
    def origin: VecType
    def one: VecType

	def zero = origin

    def dimensions: Dimensions
}

case class Epsilon(e: Double)

object Epsilon {
    implicit val default = Epsilon(1e-13)
}

object Vec1D extends VecCompanion[Vec1D] {
	val origin = Vec1D(0)
	val one = Vec1D(1)

	def dimensions: Dimensions = Dimensions.Two
}

case class Vec1D(val x: Double) extends Vec with VecLike[Vec1D] {
	def unary_-(): Vec1D = Vec1D(-x)
	def + (other: Vec1D): Vec1D = Vec1D(x + other.x)
	def - (other: Vec1D): Vec1D = Vec1D(x - other.x)
	def * (scale: Double): Vec1D = Vec1D(x * scale)
	def * (other: Vec1D): Double = x * other.x

	override def isOrigin = this == Vec1D.origin
	def toVec = this

	lazy val isAxisAligned: Boolean = true

	override def length = x
	override def normal: Vec1D = Vec1D.one
}

object Vec2D extends VecCompanion[Vec2D] {
    val origin = Vec2D(0, 0)
    val one = Vec2D(1, 1)

    def dimensions: Dimensions = Dimensions.Two
}

final case class Vec2D(x: Double, y: Double) extends Vec with VecLike[Vec2D] {
    def unary_-(): Vec2D = Vec2D(-x, -y)
    def + (other: Vec2D): Vec2D = Vec2D(x + other.x, y + other.y)
    def - (other: Vec2D): Vec2D = Vec2D(x - other.x, y - other.y)
    def * (scale: Double): Vec2D = Vec2D(x * scale, y * scale)
    def * (other: Vec2D): Double = x * other.x + y * other.y

	override def isOrigin = this == Vec2D.origin
    def toVec = this

	lazy val isAxisAligned: Boolean = Seq(x, y).count(_ != 0) == 1

    override lazy val length = super.length
    override lazy val normal: Vec2D = super.normal.asInstanceOf[Vec2D]
}

object Vec3D extends VecCompanion[Vec3D] {
	val origin = Vec3D(0, 0, 0)
	val one = Vec3D(1, 1, 1)

	def random = (Vec3D(math.random, math.random,
		math.random) * 2 - Vec3D.one).normal

	def dimensions: Dimensions = Dimensions.Two
}

final case class Vec3D(x: Double, y: Double, z: Double) extends Vec with VecLike[Vec3D] {
	def unary_-(): Vec3D = Vec3D(-x, -y, -z)
	def + (other: Vec3D): Vec3D = Vec3D(x + other.x, y + other.y, z + other.z)
	def - (other: Vec3D): Vec3D = Vec3D(x - other.x, y - other.y, z - other.z)
	def * (scale: Double): Vec3D = Vec3D(x * scale, y * scale, z * scale)
	def * (other: Vec3D): Double = x * other.x + y * other.y + z * other.z

	require(!x.isNaN && !y.isNaN && !z.isNaN, "no NaNs for points/vectors!")

	override def isOrigin = this == Vec3D.origin
	def toVec = this

	lazy val isAxisAligned: Boolean = Seq(x, y, z).count(_ != 0) == 1

	override lazy val length = super.length
	override lazy val normal: Vec3D = super.normal.asInstanceOf[Vec3D]

	//def apply(other: Point3D) = Point3D(x * other.x, y * other.y, z * other.z)
}