package com.colingodsey.logos.collections

trait Point// extends PointLike[Point]

trait PointLike[PointType <: PointLike[_]] {
    def unary_-(): PointType
    def + (other: PointType): PointType
    def - (other: PointType): PointType
    def * (scale: Double): PointType
    //def / (scale: Double): PointType
    def * (scale: PointType): Double

	def clamp(l: Double): PointType = (if(length > l) normal * l else this).asInstanceOf[PointType]

    def toPoint: PointType


	//TODO: add isNormal, effecient normals

    //def map(f: Double => Double): PointType

    //def dimensions: Dimensions

	def isAxisAligned: Boolean

    def length: Double = {
	    val r = math.sqrt(this * toPoint)
	    require(!r.isNaN, this.toString + " made a NaN")
	    r
    }
    def isOrigin: Boolean = (this * toPoint) == 0

    def / (scale: Double): PointType = this * (1.0 / scale)

    def normal: PointType = {
        val l = length
	    require(l != 0, "cant take a normal of a zero length vector!")
        if(l == 1) toPoint else this / l
    }

    def isNormal = length == 1

    def ~~(other: PointType)(implicit epsilon: Epsilon): Boolean = {
        val sub = this - other

        sub.isOrigin || math.abs(sub.length) < epsilon.e
    }

	def !~~(other: PointType)(implicit epsilon: Epsilon): Boolean =
		!this.~~(other)
}

trait PointCompanion[PointType <: Point] {
    def origin: PointType
    def one: PointType

    def dimensions: Dimensions
}

case class Epsilon(e: Double)

object Epsilon {
    implicit val default = Epsilon(1e-7)
}

object Point2D extends PointCompanion[Point2D] {
    val origin = Point2D(0, 0)
    val one = Point2D(1, 1)

    def dimensions: Dimensions = Dimensions.Two
}

final case class Point2D(x: Double, y: Double) extends Point with PointLike[Point2D] {
    def unary_-(): Point2D = Point2D(-x, -y)
    def + (other: Point2D): Point2D = Point2D(x + other.x, y + other.y)
    def - (other: Point2D): Point2D = Point2D(x - other.x, y - other.y)
    def * (scale: Double): Point2D = Point2D(x * scale, y * scale)
    def * (other: Point2D): Double = x * other.x + y * other.y

	override def isOrigin = this == Point2D.origin
    def toPoint = this

	lazy val isAxisAligned: Boolean = Seq(x, y).count(_ != 0) == 1

    override lazy val length = super.length
    override lazy val normal: Point2D = super.normal.asInstanceOf[Point2D]
}

object Point3D extends PointCompanion[Point3D] {
	val origin = Point3D(0, 0, 0)
	val one = Point3D(1, 1, 1)

	def zero = origin

	def dimensions: Dimensions = Dimensions.Two
}

final case class Point3D(x: Double, y: Double, z: Double) extends Point with PointLike[Point3D] {
	def unary_-(): Point3D = Point3D(-x, -y, -z)
	def + (other: Point3D): Point3D = Point3D(x + other.x, y + other.y, z + other.z)
	def - (other: Point3D): Point3D = Point3D(x - other.x, y - other.y, z - other.z)
	def * (scale: Double): Point3D = Point3D(x * scale, y * scale, z * scale)
	def * (other: Point3D): Double = x * other.x + y * other.y + z * other.z

	require(!x.isNaN && !y.isNaN && !z.isNaN, "no NaNs for points/vectors!")

	override def isOrigin = this == Point3D.origin
	def toPoint = this

	lazy val isAxisAligned: Boolean = Seq(x, y, z).count(_ != 0) == 1

	override lazy val length = super.length
	override lazy val normal: Point3D = super.normal.asInstanceOf[Point3D]

	//def apply(other: Point3D) = Point3D(x * other.x, y * other.y, z * other.z)
}