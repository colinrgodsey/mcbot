package com.colingodsey.mcbot.world

import com.colingodsey.logos.collections.Point3D

object CollisionDetection {
	sealed trait TraceResult {
		def dist: Double
	}
	sealed trait SurfaceHit extends TraceResult {
		def normal: Point3D
	}
	object SurfaceHit {
		def unapply(x: TraceResult) = x match {
			case x: SurfaceHit => Some(x.dist, x.normal)
			case _ => None
		}
	}

	case class TraceHit(dist: Double, normal: Point3D) extends SurfaceHit
	case class StartHit(normal: Point3D) extends SurfaceHit {
		def dist: Double = 0
	}
	case object StartSolid extends TraceResult {
		def dist: Double = 0
	}
	case class NoHit(dist: Double) extends TraceResult

	sealed trait Body {
		//def points: Seq[Point3D]
	}

	case class SphereBody(spheres: Seq[(Point3D, Double)], offset: Point3D = Point3D.zero) extends Body

	case class BoxBody(min: Point3D, max: Point3D) extends Body {
		val points = Seq(
			Point3D(min.x, min.y, min.z),
			Point3D(max.x, min.y, min.z),
			Point3D(max.x, min.y, max.z),
			Point3D(min.x, min.y, max.z),
			Point3D(min.x, max.y, min.z),
			Point3D(max.x, max.y, min.z),
			Point3D(max.x, max.y, max.z),
			Point3D(min.x, max.y, max.z))
	}

	//val eyeHeight = 1.62
	//val PlayerBody = BoxBody(Point3D(-0.5, -eyeHight, -0.5), Point3D(0.5, 2 - eyeHight, 0.5)) //eye origin
	//val PlayerBody = BoxBody(Point3D(-0.5, -1, -0.5), Point3D(0.5, 1, 0.5)) //center origin
	//val PlayerBody = BoxBody(Point3D(-0.5, 0, -0.5), Point3D(0.5, 2, 0.5)) //bottom center

	/*def playerBody(eyeHeight: Double) = SphereBody(Seq(
		(Point3D(0, -eyeHeight + 0.5, 0), 0.5),
		(Point3D(0, -eyeHeight + 1.5, 0), 0.5)
	))*/

	def playerBody(eyeHeight: Double) =
		BoxBody(Point3D(-0.5, -eyeHeight, -0.5), Point3D(0.5, 2 - eyeHeight, 0.5)) //eye origin
}

trait CollisionDetection { world: World =>
	import CollisionDetection._

	val colIncr = 0.15
	val colIncrMax = 0.2
	val sphereColIncr = 0.9
	val traceEp = 0 //0.00000000001
	val startSolidSecEp = 0.0//0.000000001


	val bbScaleBack = 0.00001

	//from a point along a vector, what distance along the vector can we move
	//returns < 0 if we started solid
	def traceBody(body: BoxBody, from: Point3D, vec: Point3D): Seq[TraceResult] = {
		val startPoints = body.points.map(_ + from)
		val endPoints = body.points.map(_ + from + vec)
		val vl = vec.length

		body.points.map { point =>
			/*val adjustedPoint = point * (1 - bbScaleBack)
			val deltaL = point.length - adjustedPoint.length
			traceRay(from + adjustedPoint, vec, from) match {
				case StartSolid => StartSolid
				case NoHit(l) => NoHit(l + deltaL)
				case TraceHit(dist, n) => TraceHit(dist + deltaL, n)
			}*/
			traceRay(from + point, vec, from)
		}.sortBy(_.dist)
	}

	def traceBody(body: SphereBody, from: Point3D, vec: Point3D): Seq[TraceResult] =
		body.spheres.map { case (center, radius) =>
			val start = center + from + body.offset

			traceSphere(start, vec, radius)
		}.sortBy(_.dist)

	val axisNorms = Set(
		Point3D(1, 0, 0),
		Point3D(0, 1, 0),
		Point3D(0, 0, 1)
	)

	val blockNormals =
		axisNorms.map(-_) ++ axisNorms

	private def normalFromBlockDelta(start: Block, end: Block) = {
		val dx = end.globalPos.x - start.globalPos.x
		val dy = end.globalPos.y - start.globalPos.y
		val dz = end.globalPos.z - start.globalPos.z

		require((math.abs(dx) + math.abs(dy) + math.abs(dz)) == 1,
			"bad block delta normal? " + (dx, dy, dz))

		Point3D(-dx, -dy, -dz)
	}

	private def normalsFromBlockDelta(start: Block, end: Block) = {
		val dx = end.globalPos.x - start.globalPos.x
		val dy = end.globalPos.y - start.globalPos.y
		val dz = end.globalPos.z - start.globalPos.z

		//require(math.abs(dx + dy + dz) == 1, "bad block normal? " + (dx, dy, dz))
//blockceter also not valid for this, replace this thing!!! all
//asfasfasfasf mak negatives too!!!
		//if((math.abs(dx) + math.abs(dy) + math.abs(dz)) != 1) {
			//just pick one normal...
			var outSet = Set[Point3D]()
			/*if(dy > 0) outSet += Point3D(0, -1, 0)
			if(dz > 0) outSet += Point3D(0, 0, -1)
			if(dx > 0) outSet += Point3D(-1, 0, 0)
			if(dy < 0) outSet += Point3D(0, 1, 0)
			if(dz < 0) outSet += Point3D(0, 0, 1)
			if(dx < 0) outSet += Point3D(1, 0, 0)*/

		if(dy > 0) outSet += Point3D(0, -1, 0)
		if(dz > 0) outSet += Point3D(0, 0, -1)
		if(dx > 0) outSet += Point3D(-1, 0, 0)
		if(dy < 0) outSet += Point3D(0, 1, 0)
		if(dz < 0) outSet += Point3D(0, 0, 1)
		if(dx < 0) outSet += Point3D(1, 0, 0)

			outSet
		//} else Set(Point3D(-dx, -dy, -dz).normal)
	}

	//consider replacing with 'block' based ray
	//center vec (from - center) can be 0
	//TODO: for the most part, this is broken because of the 0-boundry test
	//which might not even be a thing? but its still awkward to do this
	//with bounding boxes that specify mins/maxes, and maxes dont really have
	//a finite position on the maxes, just a comparator
	def traceRay(from: Point3D, vec: Point3D,
			centerPoint: Point3D): TraceResult = {
		//always reference the block from the point pulled a bit closer to us
		val centerVec = (from - centerPoint).normal //vector from center to point
		val startSolidVec = centerVec * startSolidSecEp
		val closerStart = from - startSolidVec
		val startBlock =
			if(closerStart.y < 0 || closerStart.y > 255)
				NoBlock(closerStart.x.toInt, closerStart.y.toInt, closerStart.z.toInt)
			else getBlock(closerStart)
		val isSeg = vec.length <= colIncrMax

		require(vec.length > 0)

		/*if(!startBlock.btyp.isPassable) {
			StartSolid
		} else */if(isSeg) {
			def checkSubComponent(surfaceNormal: Point3D): TraceResult = {
				val directionNormal = -surfaceNormal
				val directionLength = directionNormal * vec

				if(directionLength <= 0) return NoHit(vec.length)

				//val directionLenPerVecLength = directionLength / vec.length
				val directionVec = directionNormal * directionLength

				require(directionVec != Point3D.zero)

				val innerStartBlock = startBlock
				val adjustedEnd = from + directionVec - startSolidVec
				val innerEndBlock = getBlock(adjustedEnd)

				val blockCenter = innerStartBlock.globalPos.toPoint3D + Block.halfBlockVec
				val faceCenter = blockCenter + directionNormal / 2

				val surfaceVector = faceCenter - from
				val hitDist = (surfaceVector * surfaceNormal) / (vec * surfaceNormal)

				require(!hitDist.isNaN)

				//only start solid if we're not on the 0 boundry
				if(/*!startBlock.btyp.isPassable && */!innerStartBlock.btyp.isPassable/* && dist == 0*/)
					StartHit(surfaceNormal)
				else if(!innerEndBlock.btyp.isPassable && hitDist <= vec.length) {
					require(hitDist >= 0, s"hitDist $hitDist ")
					TraceHit(hitDist, surfaceNormal)
				} else NoHit(vec.length)
			}

			blockNormals.toSeq.map(checkSubComponent).sortBy(_.dist).head
		} else { //break into segments
			var i = 0

			while(i * colIncr < vec.length) {
				val startD = i * colIncr
				val a = from + vec.normal * startD

				require(startD < vec.length)

				val segLen = if((vec.length - startD) < colIncr)
					vec.length - startD
				else colIncr

				require(segLen > 0 && segLen <= colIncr)

				//println(a, vec.normal, segLen)

				traceRay(a, vec.normal * segLen, centerPoint + vec.normal * startD) match {
					case StartSolid if i == 0 => return StartSolid
					case StartSolid =>
						sys.error("Not fucking right")
					case TraceHit(d, norm) =>
						require((startD + d) < vec.length)
						return TraceHit(startD + d, norm)
					case StartHit(norm) =>
						return TraceHit(startD, norm)
					case NoHit(_) =>
				}

				i += 1
			}

			return NoHit(vec.length)
		}
	}

	//actually tracing a sphere of radius 1 (block)
	def traceSphere(from: Point3D, vec: Point3D, radius: Double = 1): TraceResult = {
		???
/*		require(radius > startSolidSecEp, "radius must be bigger than " + startSolidSecEp)
//println(from, vec, radius)
		/*
		for x,y,z - find sphere adjusted point, check start/end block, go from there
		always 3 checks.
		start solid checked by center -> sphere adjusted point
		 */

		require(vec.length > 0)

		//check axis aligned normals, return length of full vector (not aligned)
		//this function is cool because you'll only ever cross one boundary!
		def checkSubComponent(surfaceNormal: Point3D): TraceResult = {
			//val blockCenter = startBlock.globalPos.toPoint3D + Block.halfBlockVec
			//require(directionNormal.isAxisAligned)

			//val sDist = 0.5
			val directionNormal = -surfaceNormal
			val directionLength = directionNormal * vec

			if(directionLength <= 0) return NoHit(vec.length)

			//val directionLenPerVecLength = directionLength / vec.length
			val directionVec = directionNormal * directionLength

			require(directionVec != Point3D.zero)

			val sphereAdjustedStart = from + directionNormal * radius
			val innerSphereAdjustedStart = from + directionNormal * (radius - startSolidSecEp)

			val innerStartBlock = getBlock(innerSphereAdjustedStart)
			val innerEndBlock = getBlock(innerSphereAdjustedStart + directionVec)

			val blockCenter = innerStartBlock.globalPos.toPoint3D + Block.halfBlockVec
			val faceCenter = blockCenter + directionNormal / 2

			val sDist = faceCenter * surfaceNormal
			val pDist = (sphereAdjustedStart * surfaceNormal) - sDist
			val vDot = surfaceNormal * vec.normal
			val hitDist = -(pDist / vDot)

			require(!hitDist.isNaN)

			//println(hitDist, vec.length, from, vec)

			//only start solid if we're not on the 0 boundry
			if(/*!startBlock.btyp.isPassable && */!innerStartBlock.btyp.isPassable/* && dist == 0*/)
				StartSolid
			else if(!innerEndBlock.btyp.isPassable && hitDist <= vec.length) {
				TraceHit(math.min(hitDist, vec.length), surfaceNormal)
			} else NoHit(vec.length)
		}

		if(vec.length <= sphereColIncr) {
			require(vec.length < 1) //must be less than a block length

			//generate the movements along the 3 axis. Will at most positively intersect with 3 planes
			blockNormals.toSeq.map(checkSubComponent).sortBy(_.dist).head
		} else { //break into segments
			var i = 0

			while(i * sphereColIncr < vec.length) {
				val startD = i * sphereColIncr
				val a = from + vec.normal * startD

				require(startD < vec.length)

				val segLen = if((vec.length - startD) < sphereColIncr)
					vec.length - startD
				else sphereColIncr

				require(segLen > 0 && segLen <= sphereColIncr)

				traceSphere(a, vec.normal * segLen, radius) match {
					case StartSolid if i == 0 => return StartSolid
					case StartSolid => return StartSolid//sys.error("Not fucking right")
					case TraceHit(d, norm) =>
						return TraceHit(startD + d, norm)
					case NoHit(_) =>
				}

				i += 1
			}

			return NoHit(vec.length)
		}*/
	}
}
