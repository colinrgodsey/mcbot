package com.colingodsey.logos.collections

import com.colingodsey.logos.collections

abstract class Dimensions {
    type Point <: collections.Vec
    //type Matrix <: collections.Matrix

    val Point: VecCompanion[Point]
    //val Matrix: MatrixCompanion[Matrix]

    def n: Int
}

object Dimensions {
    /*case object Three extends Dimensions {
        type Point = Point3D
        type Matrix = Matrix4x4
    }*/

    case object Two extends Dimensions {
        type Point = Vec2
        //type Matrix = Matrix3x2

        val Point = Vec2
        //val Matrix = ImmutableMatrix3x2

        def n = 2
    }

	case object Three extends Dimensions {
		type Point = Vec3
		//type Matrix = Matrix3x2

		val Point = Vec3
		//val Matrix = ImmutableMatrix3x2

		def n = 2
	}
}
