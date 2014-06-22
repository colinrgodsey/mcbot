package com.colingodsey.mcbot.client

import com.colingodsey.collections._
import com.colingodsey.logos.collections.{Vec3, IVec3}
import com.colingodsey.ai.{BoltzmannSelector, QLearningValues, Selector}
import com.colingodsey.mcbot.world.{Block, BlockPathFinder, WorldView}
import akka.actor.Actor

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
	val tileD = 3.0
	val tileWidth = tileD * 2.0

	sealed trait State extends Equals {
		lazy val id = (toString, this).hashCode
	}
	sealed trait Action extends Equals {
		lazy val id = (toString, this).hashCode

		def toStateOpt: Option[State]
	}

	sealed trait Transition extends Action {
		def toState: State

		def toStateOpt: Option[State] = Some(toState)
	}

	type States = Map[State, Double]
	type StateActions = Map[StateAction, Double]

	case class StateAction(fromState: State, action: Action) {
		//lazy val id = (toString, fromState.hashCode, action.hashCode).hashCode
	}

	//case class Transition(toState: State) extends Action


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

	def maxQ(set: Set[StateAction]): VecN = {
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

	def update(sa: StateAction, reward: VecN, controls: Set[StateAction], w: Double = 1): VecN =
		update(qValue(sa), reward, maxQ(controls), w)

	def update(sa: StateAction, reward: VecN, reverse: StateAction, w: Double = 1): VecN = {
		//val StateAction(fromState, action) = sa
		val StateAction(toState, rAction) = reverse

		update(sa, reward, controls(toState) - reverse, w)
	}

	def update(sa: StateAction, reward: VecN, dest: State, w: Double = 1): VecN =
		update(sa, reward, controls(dest), w)

	def policy(states: States, desire: VecN): Option[Action] = {
		val actionPairs = for {
			(fromState, w0) <- states.toSeq
			ctr @ StateAction(_, action) <- controls(fromState)
			q = qValue(ctr)
			w1 = (desire * q) * w0
			w1c = math.max(w1, 0)
		} yield action -> w1c

		val actions = actionPairs.groupBy(_._1) map { case (a, seq) =>
			val ws = seq.map(_._2).sum

			a -> ws
		}

		if(actions.isEmpty) None
		else Some(selector selectFrom actions)
	}

	//use maxQ of all trans that ignore fromStates
	def update(tran: StateAction, reward: VecN = VecN.zero, maxQForDest: VecN): VecN


}

object WorldTile {
	import BotLearn._

	case class TileTransition(toState: TileState) extends Transition

	val tileDs = for {
		x <- -1 to 1
		y <- -1 to 1
		z <- -1 to 1
		if x != 0 || y != 0 || z != 0
	} yield IVec3(x, y, z)

	object TileState {
		def apply(pos: Vec3): TileState =
			TileState(IVec3(pos / tileD))

		def statesFor(pos: Vec3): States = {
			val center = TileState(pos)
			val tiles = center.neighbors + center

			tiles.map(x => (x: State) -> x.contains(pos)).toMap.filter(_._2 != 0)
		}
	}

	//for world tiles, pick a path that ends anywhere in the tile
	case class TileState(tilePos: IVec3) extends State {
		lazy val pos = tilePos.toVec3 * tileD

		lazy val neighbors = tileDs.map { x =>
			val pos = tilePos.toVec3 + x.toVec3
			TileState(IVec3(pos))
		}.toSet

		/*lazy val tileMoves = neighbors.map { tile =>
			TileMove(TileState.this, TileMoveAction(tile))
		}*/

		lazy val neighborMoves = neighbors.map(TileTransition.apply).toSet

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
			if(max <= tileD) 1.0 - max / tileD
			else 0.0
		}
	}
}

trait WorldTile extends BotLearn {
	import BotLearn._
	import WorldTile._

	implicit val clientView: BotClientView
	import clientView._

	def tileStatesFor = TileState statesFor _
	
	def currentTileStates = tileStatesFor(selfEnt.pos)
	def currentTileState = currentTileStates.toSeq.sortBy(-_._2).head._1

}

/**
 * This is the main state machine that selects behaviors,
 * follows through with them, can go back on them,
 * and then select other behavior.
 */
class BotActor extends Actor with BotThink with WorldTile {
	def baseReceive: Receive

	def followPath(path: Seq[Block]) {

	}
}

object BotThink {
	case class BlockPath(start: Block, moves: Seq[Vec3])
}

trait BotThink extends WorldTile with BotLearn {
	import BotLearn._
	import WorldTile._

	def isSane(sa: StateAction): Boolean

	implicit val worldView: WorldView
	import worldView._

	var lastStates: States = Map.empty
	var stateActions: Map[State, Map[Action, VecN]] = Map.empty
	var currentAction: Option[Action] = None
	var rewardAcc = VecN.zero
	var desire = VecN.zero

	def selector: Selector = BoltzmannSelector.default

	def qValue(sa: StateAction): VecN = {
		val StateAction(state, action) = sa

		val map = stateActions.getOrElse(state, Map.empty)

		map.getOrElse(action, VecN.zero)
	}

	def pathTo(from: Vec3, to: TileState): List[Block] = {
		val startBlock = takeBlockDownWater(Block(from))

		if(!startBlock.isPassable) Nil
		else {
			val finder = new BlockPathFinder(worldView, Block(to.pos), (tileD.toInt + 1) * 3)
			val paths = finder pathsFrom startBlock

			paths.take(200).dropWhile { case (end, moves) =>
				(to contains end.pos.toVec3) > 0.0
			}.headOption.map(_._2).toList.flatten
		}
	}

	def possibleControls(state: State): Set[StateAction] = state match {
		case x: TileState => x.neighborMoves.map(StateAction(state, _))
	}

	def controls(state: State): Set[StateAction] = (for {
		(action, q) <- stateActions.getOrElse(state, Map.empty)
	} yield StateAction(state, action)).toSet ++ possibleControls(state)

	def setQValue(sa: StateAction, q: VecN) {
		val StateAction(state, action) = sa

		val map = stateActions.getOrElse(state, Map.empty)

		stateActions += state -> (map + (action -> q))
	}

	def updateStates(oldStates: States, newStates: States,
			action: Action, reward: VecN, dt: Double) {

		//avoid reverse for max
		val oldStatesOptionSet: Set[Option[State]] = oldStates.keySet.map(Some.apply)
		val ctrs = newStates.keySet flatMap controls filter {
			case StateAction(state, a) => !oldStatesOptionSet(a.toStateOpt)
		}
		//TODO: do something with dt

		for {
			(oldState, w0) <- oldStates
			sa = StateAction(oldState, action)
			if isSane(sa)
			newQ = update(sa, reward, ctrs, w0)
		} setQValue(sa, newQ)
	}

	//finished on action complete
	def stateUpdate(dt: Double) {
		val curStates = currentTileStates

		if(currentAction.isDefined)
			updateStates(lastStates, curStates, currentAction.get, rewardAcc, dt)

		currentAction = policy(curStates, desire)
		rewardAcc = VecN.zero
		lastStates = curStates
	}
}