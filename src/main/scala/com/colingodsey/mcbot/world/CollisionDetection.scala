package com.colingodsey.mcbot.world

import com.colingodsey.logos.collections.{IVec3, Epsilon, Vec3}
import com.colingodsey.collections.{MapVector, VecN}

/**
 *
 * TODO: even newer col detection
 *
 * for each step, find the dimensional movement that will first intersect integer bounds.
 * move cursor to their, should be at least 1 whole number dim. check bounds at that exact point
 */

object CollisionDetection {
	sealed trait TraceResult {
		def dist: Double
	}
	object TraceResult {
		def unapply(x: TraceResult) = Some(x.dist)
	}
	sealed trait SurfaceHit extends TraceResult {
		def normal: Vec3
	}
	object SurfaceHit {
		def unapply(x: TraceResult) = x match {
			case x: SurfaceHit => Some(x.dist, x.normal)
			case _ => None
		}
	}

	case class TraceHit(dist: Double, normal: Vec3) extends SurfaceHit
	case class StartHit(normal: Vec3) extends SurfaceHit {
		def dist: Double = 0
	}
	case object StartSolid extends TraceResult {
		def dist: Double = 0
	}
	case class NoHit(dist: Double) extends TraceResult

	sealed trait Body {
		//def points: Seq[Point3D]
	}

	case class SphereBody(spheres: Seq[(Vec3, Double)]) extends Body

	//TODO: add mid-points, must be less than 1 block
	case class BoxBody(min: Vec3, max: Vec3) extends Body {
		val points = Seq(
			Vec3(min.x, min.y, min.z),
			Vec3(max.x, min.y, min.z),
			Vec3(max.x, min.y, max.z),
			Vec3(min.x, min.y, max.z),
			Vec3(min.x, max.y, min.z),
			Vec3(max.x, max.y, min.z),
			Vec3(max.x, max.y, max.z),
			Vec3(min.x, max.y, max.z))
	}

	//val eyeHeight = 1.62
	//val PlayerBody = BoxBody(Point3D(-0.5, -eyeHight, -0.5), Point3D(0.5, 2 - eyeHight, 0.5)) //eye origin
	//val PlayerBody = BoxBody(Point3D(-0.5, -1, -0.5), Point3D(0.5, 1, 0.5)) //center origin
	//val PlayerBody = BoxBody(Point3D(-0.5, 0, -0.5), Point3D(0.5, 2, 0.5)) //bottom center

	/*def playerBody(eyeHeight: Double) = SphereBody(Seq(
		(Point3D(0, -eyeHeight + 0.5, 0), 0.5),
		(Point3D(0, -eyeHeight + 1.5, 0), 0.5)
	))*/

	val playerHalfWidth = 0.25
	def playerBody(eyeHeight: Double) =
		BoxBody(Vec3(-playerHalfWidth, -eyeHeight, -playerHalfWidth),
			Vec3(playerHalfWidth, 1.8 - eyeHeight, playerHalfWidth)) //eye origin

	val UnitBox = BoxBody(Vec3.one * -playerHalfWidth, Vec3.one * playerHalfWidth)
	val SmallBox = BoxBody(Vec3.one * -playerHalfWidth / 4, Vec3.one * playerHalfWidth / 4)
}


trait CollisionDetection {
	import CollisionDetection._

	implicit val worldView: WorldView
	import worldView._

	val colIncr = 0.9
	val colIncrMax = 0.9
	val sphereColIncr = 0.9

	val bbScaleBack = 0.00001

	def epsilon = Epsilon.default.e

	//from a point along a vector, what distance along the vector can we move
	//returns < 0 if we started solid
	def traceBody(body: BoxBody, from: Vec3, vec: Vec3): Seq[TraceResult] = {
		val startPoints = body.points.map(_ + from)
		val endPoints = body.points.map(_ + from + vec)
		val vl = vec.length

		//TODO: this should only trace points that are at the head of the vector
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

	//def planeAABBIntersection()

	//def traceBody(body: SphereBody, from: Vec3, vec: Vec3)


	val axisNorms = Set(
		Vec3(0, 1, 0),
		Vec3(1, 0, 0),
		Vec3(0, 0, 1)
	)

	val blockNormals =
		axisNorms.map(-_) ++ axisNorms

	private def normalFromBlockDelta(start: Block, end: Block) = {
		val dx = end.pos.x - start.pos.x
		val dy = end.pos.y - start.pos.y
		val dz = end.pos.z - start.pos.z

		require((math.abs(dx) + math.abs(dy) + math.abs(dz)) == 1,
			"bad block delta normal? " + (dx, dy, dz))

		Vec3(-dx, -dy, -dz)
	}

	private def normalsFromBlockDelta(start: Block, end: Block) = {
		val dx = end.pos.x - start.pos.x
		val dy = end.pos.y - start.pos.y
		val dz = end.pos.z - start.pos.z

		var outSet = Set[Vec3]()

		if(dy > 0) outSet += Vec3(0, -1, 0)
		if(dz > 0) outSet += Vec3(0, 0, -1)
		if(dx > 0) outSet += Vec3(-1, 0, 0)
		if(dy < 0) outSet += Vec3(0, 1, 0)
		if(dz < 0) outSet += Vec3(0, 0, 1)
		if(dx < 0) outSet += Vec3(1, 0, 0)

		outSet
	}


	//fixes whole-number boundries
	def blockSelect(point: Vec3, centerPoint: Vec3) = {
		val centerVec = point - centerPoint

		var centerCorVec = Vec3.zero
		if((point.x.toInt == point.x) && centerVec.x > 0) centerCorVec += Vec3(-0.1, 0, 0)
		if((point.y.toInt == point.y) && centerVec.y > 0) centerCorVec += Vec3(0, -0.1, 0)
		if((point.z.toInt == point.z) && centerVec.z > 0) centerCorVec += Vec3(0, 0, -0.1)

		//if(centerCorVec != Vec3.zero) println("center vec " + centerCorVec)

		//below makes no sense
		/*if((point.x == 1) && centerVec.x < 0) centerCorVec += Point3D(1, 0, 0)
		if((point.y == 1) && centerVec.y < 0) centerCorVec += Point3D(0, 1, 0)
		if((point.z == 1) && centerVec.z < 0) centerCorVec += Point3D(0, 0, 1)*/

		val bl = centerCorVec + point

		(getBlock(bl), centerCorVec)
	}

	/*def traceRay(from: Vec3, vec: Vec3, center: Vec3): TraceResult = {
		val (startBlock, _) = blockSelect(from, center)
		traceRay(from, vec, startBlock)
	}*/

	def getBlockStream(pos0: Vec3, vecNormal: Vec3, startBlock: Block): Stream[(Block, Double, Vec3)] = {
		val pos = pos0

		val nextCross = (for {
			dim <- Stream("x", "y", "z")
			scale <- Stream(-1.0, 1.0)
			sNorm = VecN(dim -> scale).to[Vec3]
			normDot = sNorm * vecNormal
			if normDot < 0 //are facing
			surfCenter = startBlock.center - sNorm * 0.5
			/*surfD = (pos - surfCenter) * sNorm
			hitLen = surfD / normDot*/
			surfD = (surfCenter - pos)
			hitLen = (surfD * sNorm) / normDot
			if hitLen >= 0
		} yield (dim, sNorm, hitLen)).sortBy(_._3).headOption

		if(nextCross == None) Stream.empty
		else {
			val (dim, sNorm, hitLen) = nextCross.get
			val dNorm = -sNorm

			//val newPos0 = (pos + vecNormal * hitLen).toVecN
			//lock to lbock boundry
			/*val newEntry: (String, Double) = dim -> math.round(newPos0(dim))
			val newDims = newPos0.weights + newEntry
			val newPos = MapVector(newDims).to[Vec3]
			//val newPos = newPos0.to[Vec3]*/
			val newBlock = getBlock(startBlock.center + dNorm)
			//val (newBlock, _) = blockSelect(from, center)
			//val newLen = hitLen

			(newBlock, hitLen, sNorm) #:: getBlockStream(pos0, vecNormal, newBlock)
		}
	}

	def traceRay(from: Vec3, vec: Vec3, center: Vec3): TraceResult = {
		val (startBlock, _) = blockSelect(from, center)

		val blockStream = getBlockStream(from, vec.normal, startBlock)

		val filtered = blockStream.takeWhile {
			case (_, len, _)  => len < vec.length
		}.filter {
			case (block, len, _) => !block.isPassable
		}

		val r = filtered.headOption match {
			case _ if !startBlock.isPassable =>
				println(s"start solid block ${startBlock.pos} ${startBlock.btyp} from $from")
				StartSolid
			case a @ Some((block, len, norm))
					if !getBlock(from + vec.normal * len).isPassable &&
							block.isPassable =>
				println(s"weird col $a for vec $vec from pos $from")
				StartSolid //should nver happen....
			case Some((block, len, norm)) if !block.isPassable =>
				TraceHit(len, norm)
			case _ => NoHit(vec.length)
		}

		//println(from, vec.normal, r, from + vec.normal * r.dist)

		r
	}

	//always clip to the closest surface, move from there
	/*def traceRay(from: Vec3, vec: Vec3,
			startBlock: Block, distAcc: Double = 0): TraceResult = {
		//val (startBlock, _) = blockSelect(from, centerPoint)

		if(vec.length <= 0) return NoHit(0)

		//require(vec.length > 0)

		if(!isPassable(startBlock)) {
			require(distAcc == 0)
			return StartSolid
			//return StartHit(Vec3(1, 0, 0))
		}

		//find all planar hits, transition from block to block that way
		//all hits in sub-units
		val hits = for {
			norm <- blockNormals.toSeq
			if (vec * norm) < 0
			//cornerVec = (from - startBlock.globalPos)
			//faceDot = cornerVec * norm
			blockCenter = Block.halfBlockVec + startBlock.pos
			blockFace = blockCenter - norm * 0.5
			//faceDist = if(faceDot < 0) 1 + faceDot else faceDot
			//hitDist = -faceDist / (vec * norm)
			hitDist = math.max(((blockFace - from) * norm) / (vec.normal * norm), 0)
			if hitDist <= vec.length
			//vecPart = vec.normal * hitDist
			endBlock = getBlock(startBlock.pos - norm)
			_ = {
				val blockDelta = endBlock.pos - startBlock.pos
				require(blockDelta.length == 1, (blockDelta, hitDist))
				require(hitDist >= 0)
				//require(faceDist >= 0 && faceDist <= 1, faceDist)
			}
		} yield if(hitDist == 0) (StartHit(norm), endBlock)
		else (TraceHit(hitDist, norm), endBlock)

		if(hits.isEmpty) return NoHit(distAcc + vec.length)

		hits.sortBy(_._1.dist).head match {
			//actual hit
			case (SurfaceHit(d, norm), endBlock) if !isPassable(endBlock) =>
				if(d == 0 && distAcc == 0) StartHit(norm)
				else TraceHit(d + distAcc, norm)
			//invis hit
			case (SurfaceHit(d, _), endBlock) =>
				require(endBlock.isPassable)
				traceRay(from + vec.normal * d,
					vec.normal * (vec.length - d), endBlock, d + distAcc)
		}

	}*/
}
