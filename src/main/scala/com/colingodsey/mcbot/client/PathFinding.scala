package com.colingodsey.mcbot.client

import scala.collection.immutable.VectorBuilder

trait PathFinding[State <: Equals, Move <: Equals] {
	type Paths = Stream[(State, List[Move])]

	def maxMoves = 40
	
	def legalNeighbors(state: State): Stream[(State, Move)]

	def neighborsWithHistory(b: State, history: List[Move]): Paths = {
		if(history.length > maxMoves) Stream()
		else legalNeighbors(b).toStream map {
			case (block, move) =>
				(block, move :: history)
		}
	}

	def newNeighborsOnly(neighbors: Paths,
			explored: Set[State]): Paths = neighbors filter {
		case (block, move) => !explored(block)
	}

	def pathsFrom(initial: Paths, explored: Set[State]): Paths = {
		val more = for {
			(state, moves) <- initial
			withHistory = neighborsWithHistory(state, moves)
			next <- newNeighborsOnly(withHistory, explored)
		} yield next

		if(more.isEmpty) initial
		else initial #::: pathsFrom(more, explored ++ more.iterator.map(_._1))
	}

	def pathsFrom(start: State): Paths =
		pathsFrom(Stream((start, Nil)), Set(start))

	/*def pathsFrom(start: State, to: State): Paths = {
		val fromStart = pathsFrom(start)

		fromStart.filter {
			case (state, _) => state == to
		}
	}*/

	def pathFrom(start: State, to: State, of: Int = 1000): Option[Seq[Move]] = {
		val paths = pathsFrom(start)

		val iter = paths.iterator
		var n = 0
		val res = new VectorBuilder[Seq[Move]]

		while(iter.hasNext && n < of) {
			val (state, moves) = iter.next

			if(state == to) {
				res += moves
				n = of //hacky
			} //return Some(moves)

			n += 1
		}

		val totalRes = res.result

		if(totalRes.isEmpty) None
		else Some(totalRes.head.reverse)
		//else Some(totalRes.sortBy(_.length).head)
	}
}
