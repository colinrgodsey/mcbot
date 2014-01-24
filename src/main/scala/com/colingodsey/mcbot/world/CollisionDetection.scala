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

	val playerHalfWidth = 0.4
	def playerBody(eyeHeight: Double) =
		BoxBody(Point3D(-playerHalfWidth, -eyeHeight, -playerHalfWidth),
			Point3D(playerHalfWidth, 1.8 - eyeHeight, playerHalfWidth)) //eye origin

	val UnitBox = BoxBody(Point3D.one * -playerHalfWidth, Point3D.one * playerHalfWidth)
	val SmallBox = BoxBody(Point3D.one * -playerHalfWidth / 4, Point3D.one * playerHalfWidth / 4)
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

		require(vec.length > 0)

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
			case (SurfaceHit(d, _), endBlock) if d > 0 =>
				require(endBlock.btyp.isPassable)
				traceRay(from + vec.normal * d,
					vec.normal * (vec.length - d), endBlock, d + distAcc)
			case (SurfaceHit(d, norm), endBlock) if d <= 0 =>
				StartHit(norm)
		}

	}
}
