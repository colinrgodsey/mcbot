package com.colingodsey.collections

import scala.collection.immutable.VectorBuilder
import scala.concurrent.blocking

trait PathFinding[State <: Equals, Move <: Equals] {
	type Paths = Stream[(State, List[Move])]

	def maxPathLength: Int

	private def maxMoves = maxPathLength
	
	def legalNeighbors(state: State): Stream[(State, Move)]

	def neighborsWithHistory(b: State, history: List[Move]): Paths = {
		if(history.length > maxMoves) Stream()
		else legalNeighbors(b).toStream map {
			case (block, move) =>
				(block, move :: history)
		}
	}

	def newNeighborsOnly(neighbors: Paths,
			explored: Set[State]): Paths = neighbors.headOption match {
		case None => Stream()
		case Some((state, moves)) if !explored(state) =>
			Stream(state -> moves) #::: newNeighborsOnly(neighbors.tail, explored + state)
		case _ => newNeighborsOnly(neighbors.tail, explored)
	}


		/*neighbors filter {
		case (block, move) => !explored(block)
	}*/

	def pathsFrom(initial0: Paths, explored0asdsad: Set[State]): Paths = {
		var explored = Set[State](initial0.head._1)

		def moreFrom(state: State, moves: List[Move]): Paths = for {
			nextPath @ (nextState, nextMoves) <- {
				val ns = neighborsWithHistory(state, moves).filter(x => !explored(x._1))

				explored ++= ns.map(_._1)

				ns
			}
			next <- Stream(nextPath) append moreFrom(nextState, nextMoves)
			//if !explored(next._1)
		} yield {
			//explored += next._1

			next
		}

		initial0 #::: moreFrom(initial0.head._1, initial0.head._2)
	}

	def pathsFrom(start: State): Paths =
		pathsFrom(Stream((start, Nil)), Set(start))

	def infinitePathsFrom(start: State, to: State): Paths = {
		val fromStart = pathsFrom(start)

		fromStart.filter {
			case (state, _) => state == to
		}
	}

	def pathFrom(start: State, to: State, of: Int = 1000): Option[Seq[Move]] = blocking {
		val paths = pathsFrom(start)

		val iter = paths.iterator
		var n = 0
		//val res = new VectorBuilder[Seq[Move]]

		if(start == to) return None

		while(iter.hasNext && n < of) {
			val (state, moves) = iter.next

			if(state == to) {
				//res += moves
				//println(n, moves.length)
				return Some(moves.reverse)
			}

			n += 1
		}

		None
	}
}
