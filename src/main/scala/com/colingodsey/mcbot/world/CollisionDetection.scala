package com.colingodsey.mcbot.world

import com.colingodsey.logos.collections.{IPoint3D, Epsilon, Point3D}

object CollisionDetection {
	sealed trait TraceResult {
		def dist: Double
	}
	object TraceResult {
		def unapply(x: TraceResult) = Some(x.dist)
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
		BoxBody(Point3D(-0.5, -eyeHeight, -0.5), Point3D(0.5, 1.9 - eyeHeight, 0.5)) //eye origin

	val UnitBox = BoxBody(Point3D.one * -0.5, Point3D.one * 0.5)
}

trait CollisionDetection { world: World =>
	import CollisionDetection._

	val colIncr = 0.9
	val colIncrMax = 0.9
	val sphereColIncr = 0.9

	val bbScaleBack = 0.00001

	def epsilon = Epsilon.default.e

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
		Point3D(0, 1, 0),
		Point3D(1, 0, 0),
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

		var outSet = Set[Point3D]()

		if(dy > 0) outSet += Point3D(0, -1, 0)
		if(dz > 0) outSet += Point3D(0, 0, -1)
		if(dx > 0) outSet += Point3D(-1, 0, 0)
		if(dy < 0) outSet += Point3D(0, 1, 0)
		if(dz < 0) outSet += Point3D(0, 0, 1)
		if(dx < 0) outSet += Point3D(1, 0, 0)

		outSet
	}


	//fixes whole-number boundries
	def blockSelect(point: Point3D, centerPoint: Point3D) = {
		val centerVec = point - centerPoint

		var centerCorVec = Point3D.zero
		if((math.floor(point.x) == point.x) && centerVec.x > 0) centerCorVec += Point3D(-1, 0, 0)
		if((math.floor(point.y) == point.y) && centerVec.y > 0) centerCorVec += Point3D(0, -1, 0)
		if((math.floor(point.z) == point.z) && centerVec.z > 0) centerCorVec += Point3D(0, 0, -1)

		//below makes no sense
		/*if((point.x == 1) && centerVec.x < 0) centerCorVec += Point3D(1, 0, 0)
		if((point.y == 1) && centerVec.y < 0) centerCorVec += Point3D(0, 1, 0)
		if((point.z == 1) && centerVec.z < 0) centerCorVec += Point3D(0, 0, 1)*/

		val bl = centerCorVec + point

		(if(bl.y < 0 || bl.y > 255)
			NoBlock(bl.x.toInt, bl.y.toInt, bl.z.toInt)
		else getBlock(bl), centerCorVec)
	}

	def traceRay(from: Point3D, vec: Point3D, center: Point3D): TraceResult = {
		val (startBlock, _) = blockSelect(from, center)
		traceRay(from, vec, startBlock)
	}

	//always clip to the closest surface, move from there
	def traceRay(from: Point3D, vec: Point3D,
			startBlock: Block, distAcc: Double = 0): TraceResult = {
		//val (startBlock, _) = blockSelect(from, centerPoint)

		require(vec.length > epsilon)

		if(!startBlock.btyp.isPassable) {
			require(distAcc == 0)
			return StartSolid
		}

		//find all planar hits, transition from block to block that way
		//all hits in sub-units
		val hits = for {
			norm <- blockNormals.toSeq
			if (vec * norm) < 0
			//cornerVec = (from - startBlock.globalPos)
			//faceDot = cornerVec * norm
			blockCenter = Block.halfBlockVec + startBlock.globalPos
			blockFace = blockCenter - norm * 0.5
			//faceDist = if(faceDot < 0) 1 + faceDot else faceDot
			//hitDist = -faceDist / (vec * norm)
			hitDist = ((blockFace - from) * norm) / (vec.normal * norm)
			if hitDist <= vec.length
			//vecPart = vec.normal * hitDist
			endBlock = getBlock(startBlock.globalPos - norm)
			_ = {
				val blockDelta = endBlock.globalPos - startBlock.globalPos
				require(blockDelta.length == 1, (blockDelta, hitDist))
				require(hitDist >= 0)
				//require(faceDist >= 0 && faceDist <= 1, faceDist)
			}
		} yield if(hitDist == 0) (StartHit(norm), endBlock)
		else (TraceHit(hitDist, norm), endBlock)

		if(hits.isEmpty) return NoHit(distAcc + vec.length)

		hits.sortBy(_._1.dist).head match {
			case (SurfaceHit(d, norm), endBlock) if !endBlock.btyp.isPassable =>
				if(d == 0 && distAcc == 0) StartHit(norm)
				else TraceHit(d + distAcc, norm)
			case (SurfaceHit(d, _), endBlock) =>
				require(endBlock.btyp.isPassable)
				traceRay(from + vec.normal * d,
					vec.normal * (vec.length - d), endBlock, d + distAcc)
		}

	}


	//TODO: alternative... get hit points along all 3 possible planes, find ones in same block
	//select all blocks that intersect rectangle created by ray
	//or take ray and origin, and see wat xis will hit boundry first.... minimum


	//ORRRRR do the exact same thing as below, but perform axis based movement checks sorted by axis intensity

	def traceRayOld(from: Point3D, vec: Point3D,
			centerPoint: Point3D): TraceResult = {

		//val startSolidVec = centerVec * epsilon
		//val centerBlock = getBlock(centerPoint)
		val isSeg = vec.length <= colIncrMax

		require(vec.length > 0)

		//if(!startBlock.btyp.isPassable) return StartSolid

		/*if(!startBlock.btyp.isPassable) {
			StartSolid
		} else */if(isSeg) {
			var startPoint = from

			//TODO: corners are now giving issues. the taxi-cabbing around is letting us clip corners
			def checkSubComponent(surfaceNormal: Point3D): TraceResult = {
				//val svl = -(surfaceNormal * vec)
				val svl = vec * -surfaceNormal

				if(svl <= epsilon) return NoHit(vec.length)

				val subVec = -surfaceNormal * svl

				require(subVec.length > 0)
				require(vec.length > 0)

				val subStart = startPoint

				
				require(subVec != Point3D.zero)

				val (innerStartBlock, startBlockCorrection) = blockSelect(subStart, centerPoint)

				val adjustedEnd = subStart + subVec// - startSolidVec
				val (innerEndBlock, _) = blockSelect(adjustedEnd, centerPoint)
//println(innerStartBlock, innerEndBlock)
				//val blockCenter = innerStartBlock.globalPos.toPoint3D + Block.halfBlockVec
				//val faceCenter = blockCenter - surfaceNormal * 0.5

				//val centerOffset = (subStart - blockCenter) * surfaceNormal
				//val isNegative = Point3D.one * surfaceNormal < 0

				//i feel like this isnt right....
				val blCornerVec = (subStart - innerStartBlock.globalPos.toPoint3D + startBlockCorrection)
				val wholeBlockOffset = math.abs(blCornerVec * surfaceNormal)
				val realCornerOffset = blCornerVec * surfaceNormal

				require(wholeBlockOffset <= 1 && wholeBlockOffset >= 0,
					(wholeBlockOffset, subStart, innerStartBlock.globalPos.toPoint3D, startBlockCorrection, blCornerVec))

				val faceOffset = if(realCornerOffset < 0) 1 + realCornerOffset else realCornerOffset

				//if(faceOffset > subVec.length) return NoHit(vec.length)

				//println(faceCenter - subStart, surfaceNormal)

				//if((innerStart - subStart).length != 0) println((innerStart - subStart).length)

				//TODO: why does the normal stuff below not work?
				//val faceVec = subStart - faceCenter
				//val hitDist = -(faceVec * surfaceNormal) / (vec * surfaceNormal)

				//val subVecToVecRatio = vec.normal * directionNormal
				//val hitDist = faceOffset / (vec.normal * subVec.normal)
				val hitDist = faceOffset * vec.length / subVec.length

				require(!hitDist.isNaN)
				//require(subVecToVecRatio > 0)

				require(hitDist >= 0, (faceOffset, svl, subVec, surfaceNormal))

				//if(hitDist == 0) println(directionVec, surfaceNormal)

				require((innerStartBlock.globalPos - innerEndBlock.globalPos).length <= 1,
					(subVec, innerStartBlock.globalPos, innerEndBlock.globalPos, startBlockCorrection, subStart, adjustedEnd))

				//only start solid if we're not on the 0 boundry
				if(!innerStartBlock.btyp.isPassable) StartSolid
				else if(innerEndBlock.btyp.isPassable) {
					startPoint += subVec
					NoHit(vec.length)
				} else if(hitDist < epsilon) {
					//println(innerStartBlock.globalPos, innerEndBlock.globalPos,  subStart, subVec, hitDist)
					StartHit(surfaceNormal)
				} else if(!innerEndBlock.btyp.isPassable && hitDist < vec.length) {
					require(hitDist >= 0, s"hitDist $hitDist ")
					val blockDelta = innerEndBlock.globalPos.toPoint3D - innerStartBlock.globalPos.toPoint3D
					//println(innerStartBlock.globalPos, blockDelta, subStart, hitDist, vec.length, subStart + vec.normal * hitDist, subVec)
					startPoint += subVec.normal * faceOffset
					TraceHit(hitDist, surfaceNormal)
				} else ??? //NoHit(vec.length))
			}

			//TODO: do this in order of intensity
			val norms = blockNormals.toSeq.sortBy(-_ * vec.normal)

			norms.toSeq.map(checkSubComponent).sortBy(_.dist).head
		} else { //break into segments
			var i = 0.0
			var lastHit: TraceResult = StartSolid
			var lastI = i

			while(i < vec.length) {
				val a = from + vec.normal * i

				require(i < vec.length)

				val segLen = if((vec.length - i) < colIncr)
					vec.length - i
				else colIncr

				require(segLen > 0 && segLen <= colIncrMax)

				//println(a, vec.normal, segLen)

				val hit = traceRay(a, vec.normal * segLen, centerPoint + vec.normal * i)

//println(lastHit)
				hit match {
					case StartSolid if i == 0 => return StartSolid
					case StartSolid =>
						//sys.error("Not fucking right")
						//return StartSolid//lastHit

						return lastHit match {
							case StartSolid => StartSolid
							case SurfaceHit(d, norm) => TraceHit(d + lastI, norm)
							case NoHit(d) => NoHit(d + lastI)
						}
					case TraceHit(d, norm) =>
						require(d <= segLen)
						require((i + d) <= vec.length)
						return TraceHit(i + d, norm)
					case StartHit(norm) if i == 0 =>
						return StartHit(norm)
					case StartHit(norm) =>
						return TraceHit(i, norm)
					case NoHit(_) =>
				}

				lastHit = hit
				lastI = i
				i += segLen
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
