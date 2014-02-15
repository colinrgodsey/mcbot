package com.colingodsey.mcbot.client

import com.colingodsey.collections._
import com.colingodsey.logos.collections.{Vec3, IVec3}

/*

TODO:
*tiled should produce a larger selection list, weighted appropriately
*a movement into a tiled area produces a tiled transition with appr weights
*qValue should be weighted average of discrete transition
* motion tiled transition is weighted of discrete TileTransition
 */

object BotLearn {
	val tileR = 3.0
	val tileWidth = tileR * 2.0
	val tileD = tileR //full overlap

	sealed trait State extends Equals

	sealed trait Action extends Equals


	trait StateAction {
		//states and weights
		//def fromStates: Map[State, Double]
		//def action: Action

		def stateActionPairs: Map[DiscreteStateAction, Double]
			//fromStates.map(pair => StateActionPair(pair._1, action) -> pair._2)
	}

	trait DiscreteStateAction extends StateAction with Equals {
		def fromState: State
		def action: Action
		def stateActionPairs: Map[DiscreteStateAction, Double] = Map(this -> 1.0)
	}

	trait Transition extends StateAction {
		def transitions: Map[DiscreteTransition, Double]

		def stateActionPairs = transitions.map(pair =>
			(pair._1: DiscreteStateAction) -> pair._2)
	}

	trait DiscreteTransition extends Transition with DiscreteStateAction {
		def toState: State
		def transitions: Map[DiscreteTransition, Double] = Map(this -> 1.0)
	}

	case class StateActionPair(fromState: State, action: Action) extends DiscreteStateAction

	case class TileMoveAction(tile: TileState) extends Action

	case class TileMove(fromState: TileState, action: TileMoveAction)
			extends DiscreteTransition {
		def toState = action.tile
	}

	case class TiledMove(moves: Map[TileMove, Double]) extends Transition {
		def transitions = moves.map(pair => (pair._1: DiscreteTransition) -> pair._2)
	}

	//for world tiles, pick a path that ends anywhere in the tile
	case class TileState(tilePos: IVec3) extends State {
		lazy val pos = tilePos.toVec3 * tileD

		//val left = TileState(tilePos + Vec3(0))
		def neighborD0 = Seq(
			Vec3(1, 0, 0),
			Vec3(0, 1, 0),
			Vec3(0, 0, 1)
		)
		def neighborDs = neighborD0 ++ neighborD0.map(-_)

		lazy val neighbors = neighborDs.toSeq.map { x =>
			val pos = tilePos.toVec3 + x
			TileState(IVec3(pos.x.toInt, pos.y.toInt, pos.z.toInt))
		}

		lazy val tileMoves = neighbors.map { tile =>
			TileMove(this, TileMoveAction(tile))
		}

		def tiledMove(point: Vec3) = {
			val moves = tileMoves.flatMap { move =>
				val tile = move.action.tile
				val cont = tile contains point getOrElse 0.0

				if(cont == 0) None
				else Some(move -> cont)
			}.toMap

			val tot = moves.map(_._2).sum
			val avgdMoves = moves.map(x => x._1 -> (x._2 / tot))

			TiledMove(avgdMoves)
		}

		//def tiledMoves(point: Vec3)

		//returns percent contained
		def contains(point: Vec3): Option[Double] = {
			val max = (point - pos).values.map(math.abs).max

			//inside box
			if(max <= tileR) Some(1.0 - max / tileR)
			else None
		}
	}
}



trait BotLearn {
	import BotLearn._

	def qValue(trans: DiscreteStateAction): VecN

	def qValue(trans: StateAction): VecN = trans.stateActionPairs.map {
		case (st, w) => qValue(st: DiscreteStateAction) * w
	}.sum

/*
	//q value for q-learning
	def qValue(trans: DiscreteTransition): VecN

	//control values, all transitions that could result from said transition, ignore reverse
	def controls(trans: DiscreteTransition): Set[DiscreteTransition]

	def qValue(trans: Transition): VecN = trans.subStates.map {
		case (st, w) => qValue(st: DiscreteTransition) * w
	}.sum

	/*def controls(trans: Transition): Set[Transition] = trans.subStates.map {
		case (st, w) =>
	}*/

	//per dimension max - the max corner of the q-value space box
	//
	def maxQ(controlSet: Set[Transition]): VecN = {
		val values = controlSet.map(qValue)
		val keySet = values.flatMap(_.weights.keySet)
		val maxQPart = keySet.map { d =>
			d -> values.toStream.map(_ apply d).max
		}.filter(_._2 != 0)

		MapVector(maxQPart.toMap)
	}*/


}
