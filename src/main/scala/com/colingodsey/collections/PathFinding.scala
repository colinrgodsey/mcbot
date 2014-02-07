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

	def pathsFrom(initial: Paths, explored: Set[State]): Paths = {
		/*val more = for {
			(state, moves) <- initial
			if !explored(state)
			withHistory = neighborsWithHistory(state, moves)
			ns = newNeighborsOnly(withHistory, explored)
			next <- ns
		} yield next

		if(more.isEmpty) initial
		else initial #::: pathsFrom(more, explored ++ more.iterator.map(_._1))*/

		val more0 = for {
			(state, moves) <- initial
			withHistory = neighborsWithHistory(state, moves)
			ns = newNeighborsOnly(withHistory, explored)
			next <- ns
		} yield next

		def uniqueOnly(paths: Paths, expl: Set[State]): Paths = paths.headOption match {
			case None => Stream()
			case Some((state, moves)) if !expl(state) =>
				val nExp = expl + state
				Stream(state -> moves) #::: uniqueOnly(paths.tail, nExp)
			case _ => uniqueOnly(paths.tail, expl)
		}

		val more = uniqueOnly(more0, explored)

		if(more.isEmpty) initial
		else initial #::: pathsFrom(more, explored ++ more.iterator.map(_._1))

		//TODO: this break. explored set isnt updated correctly
		/*val tailStream = for {
			(state, moves) <- initial
			withHistory = neighborsWithHistory(state, moves)
			more = newNeighborsOnly(withHistory, explored)
			newExplored = explored ++ more.iterator.map(_._1)
			next @ (nextState, nextMoves) <- more
			path <- pathsFrom(more, newExplored)
			//add some kind of secondary sorting herew
		} yield path

		initial #::: tailStream*/
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
