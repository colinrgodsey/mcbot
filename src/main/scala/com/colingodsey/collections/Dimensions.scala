package com.colingodsey.logos.collections

import com.colingodsey.logos.collections

abstract class Dimensions {
    type Point <: collections.Point
    //type Matrix <: collections.Matrix

    val Point: PointCompanion[Point]
    //val Matrix: MatrixCompanion[Matrix]

    def n: Int
}

object Dimensions {
    /*case object Three extends Dimensions {
        type Point = Point3D
        type Matrix = Matrix4x4
    }*/

    case object Two extends Dimensions {
        type Point = Point2D
        //type Matrix = Matrix3x2

        val Point = Point2D
        //val Matrix = ImmutableMatrix3x2

        def n = 2
    }

	case object Three extends Dimensions {
		type Point = Point3D
		//type Matrix = Matrix3x2

		val Point = Point3D
		//val Matrix = ImmutableMatrix3x2

		def n = 2
	}
}
