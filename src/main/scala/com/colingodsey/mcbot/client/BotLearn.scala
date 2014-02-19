package com.colingodsey.mcbot.client

import com.colingodsey.collections._
import com.colingodsey.logos.collections.{Vec3, IVec3}
import com.colingodsey.ai.{QLearningValues, Selector}

/*

TODO:
*tiled should produce a larger selection list, weighted appropriately
*a movement into a tiled area produces a tiled transition with appr weights
*qValue should be weighted average of discrete transition
* motion tiled transition is weighted of discrete TileTransition
*
*
*
* AHHHHHH
* There should be only one action and toState per Transition
* toState exists only for reference to control
* a StateAction has multiple states with weights
* tiles can end up having StateAction relationships with tiles not directly connected
* the above will result in lots of meshing
 */

object BotLearn {
	val tileR = 3.0
	val tileWidth = tileR * 2.0
	val tileD = tileR //full overlap

	sealed trait State extends Equals
	sealed trait Action extends Equals

	sealed trait Transition extends Action {
		def toState: State
	}

	type States = Map[State, Double]
	type StateActions = Map[StateAction, Double]

	case class StateAction(fromState: State, action: Action) {
		lazy val id = (toString, fromState.hashCode, action.hashCode).hashCode
	}

	//case class Transition(toState: State) extends Action

	case class TileMove(toState: TileState) extends Transition

	object TileState {
		def tileFor(pos: Vec3) =
			TileState(IVec3(math.round(pos.x / tileD)))
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

		/*lazy val tileMoves = neighbors.map { tile =>
			TileMove(TileState.this, TileMoveAction(tile))
		}*/

		lazy val neighborMoves = neighbors.map(TileMove.apply).toSet

		def tiledStates(point: Vec3) = {
			val tiles = neighbors.flatMap { tile =>
				val cont = tile contains point

				if(cont == 0) None
				else Some(tile -> cont)
			}.toMap

			val tot = tiles.map(_._2).sum

			require(tot != 0.0)

			tiles.map(x => x._1 -> (x._2 / tot))
		}

		//returns percent contained, if contained
		def contains(point: Vec3): Double = {
			val max = (point - pos).values.map(math.abs).max

			//inside box
			if(max <= tileR) 1.0 - max / tileR
			else 0.0
		}
	}
}

/*


---AAAA--------
-----BBBB------
-------CCCC----

1. States(a -> 0.5, b -> 0.5)
2. controls(states) = Map(a,b -> 0.33, b,a -> 0.33, b,c -> 0.33)

 */

trait BotLearn extends QLearningValues {
	import BotLearn._

	def qValue(sa: StateAction): VecN
	def controls(state: State): Set[StateAction]

	def selector: Selector

	def qValue(sas: StateActions): VecN = {
		require(!sas.isEmpty)

		val sum = sas.map(_._2).sum

		require(sum != 0.0)

		(sas map { case (sa, w) =>
			qValue(sa) * (w / sum)
		}).sum
	}

	def maxQ(set: Set[StateAction]) = {
		val values = set.toSeq.map(qValue)

		val keySet = values.flatMap(_.weights.keySet)
		val maxQPart = keySet.map { d =>
			d -> values.toStream.map(_ apply d).max
		}.filter(_._2 != 0)

		MapVector(maxQPart.toMap)
	}

	//initial value, reward, maxQ of dest, weight
	def update(q0: VecN, reward: VecN, maxQ: VecN, w: Double = 1): VecN = {
		val α = α0 * (1.0 - w)
		val q1 = maxQ * γ

		reward + q0 * α + q1 * (1.0 - α)
	}

	def update(sa: StateAction, reward: VecN, reverse: StateAction, w: Double = 1): VecN = {
		//val StateAction(fromState, action) = sa
		val StateAction(toState, raction) = reverse
		//val inv = StateAction
		val max = maxQ(controls(toState) - reverse)

		update(qValue(sa), reward, max, w)
	}

	def policy(states: States, desire: VecN): Action = {
		val actionPairs = for {
			(fromState, w0) <- states.toSeq
			ctr @ StateAction(_, action) <- controls(fromState)
			q = qValue(ctr)
			w1 = (desire * q) * w0
		} yield action -> w1

		val actions = actionPairs.groupBy(_._1) map { case (a, seq) =>
			val ws = seq.map(_._2).sum

			a -> ws
		}

		selector.selectFrom(actions)
	}

	//use maxQ of all trans that ignore fromStates
	def update(tran: StateAction, reward: VecN = VecN.zero, maxQForDest: VecN): VecN


}



trait WorldTile extends BotLearn {



}