package com.colingodsey.mcbot.client

import com.colingodsey.collections._
import com.colingodsey.logos.collections.{Vec3, IVec3}
import com.colingodsey.ai.{BoltzmannSelector, QLearningValues, Selector}
import com.colingodsey.mcbot.world.{Block, BlockPathFinder, WorldView}
import com.colingodsey.mcbot.client.BotClient.View
import akka.actor._
import com.colingodsey.mcbot.client.BotLearn.{StateAction, Action, States}
import com.colingodsey.mcbot.client.BotThink.ActionFinished
import java.io._
import scala.util.Try
import scala.concurrent.blocking
import scala.concurrent.duration._
import com.colingodsey.mcbot.client.WorldTile.TileState
import scala.Some
import com.colingodsey.mcbot.client.BotThink.ActionFinished
import com.colingodsey.mcbot.client.BotLearn.StateAction
import scala.collection.immutable.{Queue, VectorBuilder}
import org.hsqldb.persist.HsqlProperties
import java.sql.DriverManager


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

	def normalizeWeights[T](map: Map[T, Double]) = {
		val tot = map.values.sum

		map.map(p => p._1 -> p._2 / tot)
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

	def qValueForPolicy(sa: StateAction): VecN = qValue(sa)

	def maxQ(set: Set[StateAction]): VecN = {
		val values = set.toSeq.map(qValue)

		val keySet = values.flatMap(_.weights.keySet)
		val maxQPart = keySet.map { d =>
			d -> values.toStream.map(_ apply d).max
		}.filter(_._2 != 0)

		MapVector(maxQPart.toMap)
	}

	//initial value, reward, maxQ of dest, weight
	def calcQ(q0: VecN, reward: VecN, maxQ: VecN, w: Double): VecN = {
		val α = α0 * (1.0 - w)
		val q1 = maxQ * γ

		reward + q0 * α + q1 * (1.0 - α)
	}

	def calcQ(sa: StateAction, reward: VecN, controls: Set[StateAction], w: Double): VecN =
		calcQ(qValue(sa), reward, maxQ(controls), w)

	def calcQ(sa: StateAction, reward: VecN, reverse: StateAction, w: Double): VecN = {
		//val StateAction(fromState, action) = sa
		val StateAction(toState, rAction) = reverse

		calcQ(sa, reward, controls(toState) - reverse, w)
	}

	def calcQ(sa: StateAction, reward: VecN, dest: State, w: Double): VecN =
		calcQ(sa, reward, controls(dest), w)

	def policy(states: States, desire: VecN, ignoreStates0: Set[State]): Option[Action] = {
		val stateSet = states.keySet
		val ignoreStates: Set[Option[State]] =
			ignoreStates0.map(Some.apply) ++ stateSet.map(Some.apply)

		val actionPairs = for {
			(fromState, w0) <- states.toSeq
			ctr @ StateAction(_, action) <- controls(fromState)
			if !ignoreStates(action.toStateOpt)
			q = qValueForPolicy(ctr)
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
	//def update(tran: StateAction, reward: VecN = VecN.zero, maxQForDest: VecN): VecN


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
		@transient lazy val pos = tilePos.toVec3 * tileD

		@transient lazy val neighbors = tileDs.map { x =>
			val pos = tilePos.toVec3 + x.toVec3
			TileState(IVec3(pos))
		}.toSet

		/*lazy val tileMoves = neighbors.map { tile =>
			TileMove(TileState.this, TileMoveAction(tile))
		}*/

		@transient lazy val neighborMoves = neighbors.map(TileTransition.apply).toSet

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

	implicit val clientView: BotClient.View
	import clientView._

	def tileStatesFor = TileState statesFor _

	def currentTileStates = tileStatesFor(selfEnt.pos)
	def currentTileState = currentTileStates.toSeq.sortBy(-_._2).head._1

}

object BotThink {
	case class BlockPath(start: Block, moves: Seq[Vec3])

	case class ActionFinished(dt: Double)
	case object MaybeSelectGoal
	case object SelectGoal
	case object ClearGoal
	case class AccumReward(r: VecN)
}

trait BotThink extends WorldTile with BotLearn with Actor with ActorLogging {
	import BotLearn._
	import BotThink._
	import WorldTile._

	def isSane(sa: StateAction): Boolean
	def isStale(sa: StateAction): Boolean
	def actionSelected(states: States, action: Action)
	def desire: VecN
	def desire_=(x: VecN)

	implicit val worldView: WorldView
	import worldView._

	var lastStates: States = Map.empty
	var stateActions: Map[State, Map[Action, VecN]] = Map.empty
	var currentAction: Option[Action] = None
	var rewardAcc = VecN.zero

	def selector: Selector = BoltzmannSelector.default

	def stateActionsFor(state: State) =
		stateActions.getOrElse(state, Map.empty)

	def qValue(sa: StateAction): VecN = {
		val StateAction(state, action) = sa

		//allow defaults for discovery mechanics
		val map = stateActionsFor(state)

		map.getOrElse(action, VecN.zero)
	}

	//perceptual need weighting
	override def qValueForPolicy(sa: StateAction): VecN = {
		if(sa.action.toStateOpt.isDefined) {
			val to = sa.action.toStateOpt.get


			if(isStale(sa)) VecN("stale" -> 100.0)
			else if(stateActions.get(to) == None) VecN("discover" -> 1000.0)
			else qValue(sa)
		} else qValue(sa)
	}

	//should be exhaustive
	def possibleControls(state: State): Set[StateAction] = state match {
		case x: TileState => x.neighborMoves.map(StateAction(state, _))
	}

	//only sane controls
	def controls(state: State): Set[StateAction] = ((for {
		(action, q) <- stateActionsFor(state)
	} yield StateAction(state, action)).toSet ++ possibleControls(state)).filter(isSane)

	def setQValue(sa: StateAction, q: VecN) {
		val StateAction(state, action) = sa

		val map = stateActionsFor(state)

		val (isNew, bonus) = action match {
			case TileTransition(to: TileState) =>
				//if target is not recognized
				(stateActions.get(state) == None, VecN("discover" -> 10.0))
			case TileTransition(to: TileState) if state.isInstanceOf[TileState] =>
				val dy = to.pos.y - state.asInstanceOf[TileState].pos.y
				val up = if(dy > 0) 1 else 0
				val down = if(dy < 0) 1 else 0
				//if target is not recognized
				(stateActions.get(state) == None, VecN("discover" -> 10.0,
					"up" -> up, "down" -> down))
			case _ => (false, VecN.zero)
		}

		val q2 = if(isNew) {
			//rewardAcc += VecN("discover" -> 1.0)
			val dy =

			desire -= VecN("discover" -> 1.0)

			log.info("new state action " + sa)

			q + bonus
		} else {
			q - VecN("discover" -> (q("discover") * 0.08))
		}

		val oldQ = map.getOrElse(action, VecN.zero)

		val dq = q2 - oldQ

		log.info(s"Q updated for $sa delta $dq qval $q2")

		stateActions += state -> (map + (action -> q2))
	}

	def updateStates(oldStates: States, newStates: States,
			action: Action, reward: VecN, w: Double) {

		//avoid reverse for max
		val oldStatesOptionSet: Set[Option[State]] = oldStates.keySet.map(Some.apply)
		val ctrs = newStates.keySet flatMap controls filter {
			case StateAction(state, a) => !oldStatesOptionSet(a.toStateOpt)
		}
		//TODO: do something with dt

		val sas = for {
			(oldState, w0) <- oldStates
			sa = StateAction(oldState, action)
			if isSane(sa)
		} yield sa -> w0

		for {
			(sa, w0) <- normalizeWeights(sas)
			newQ = calcQ(sa, reward, ctrs, w0 * w)
		} setQValue(sa, newQ)
	}

	//finished on action complete
	//TODO: make only  actions to a toState if we're more in the state than we were before.
	//a weight based on how much movement towards the state were in ?
	def actionFinished(dt: Double = 1) {
		log.debug("action finished")
		val curStates = currentTileStates

		if(currentAction.isDefined) {
			//TODO: this should allow transfers to non-sane states...
			//can still be used in selection to further weight other tiles actions
			updateStates(lastStates, curStates, currentAction.get, rewardAcc, dt)

			lastStates.toSeq.sortBy(-_._2).headOption match {
				case Some((s: TileState, w)) =>
					updateStates(curStates, lastStates, TileTransition(s), VecN.zero, dt * 0.25)
				case _ =>
			}
		}

		val prevOld = lastStates
		lastStates = curStates
		currentAction = None
		maybeSelectGoal(prevOld)
	}

	def maybeSelectGoal(prevOldStates: States = Map.empty) = if(currentAction == None) blocking {
		currentAction = policy(currentTileStates, desire, prevOldStates.map(_._1).toSet)
		//log.info("Selecting new action " + currentAction)
		rewardAcc = VecN.zero

		if(currentAction != None)
			actionSelected(currentTileStates, currentAction.get)
	}
}

class BotThinkActor(bot: ActorRef, wv: WorldView) extends BotThink with Actor
		with ActorLogging with BotClient.ViewReceiver with BotPathing with WorldTile {
	import BotThink._

	implicit val clientView: View = this
	implicit val worldView: WorldView = wv

	val stateSaveActor = context.actorOf(
		Props(classOf[StateSaveActor], self).withDispatcher("mcbot.db-pinned-dispatcher"),
		name = "state-save")

	var subscribers = Set[ActorRef]()

	//only redo StateActions if we get stuck
	var lastStateActions = Queue[StateAction]()

	val maxSAQueueLength = 15

	def γ: Double = 0.8
	def α0: Double = 0.7

	context watch stateSaveActor

	def isStale(sa: StateAction) = lastStateActions contains sa

	override def isSane(sa: StateAction): Boolean = ((sa.fromState, sa.action) match {
		case (s: WorldTile.TileState, t @ WorldTile.TileTransition(dest)) =>
			//use our position if were in this state, otherwise use center
			val path = if(s.contains(footBlock.center) > 0) pathTo(selfEnt.pos, dest)
			else pathTo(s.pos, dest)
			/*s.neighborMoves(t) && */path.length > 1 && s != dest
		case _ => false
	}) //&& !isStale(sa)

	override def setQValue(sa: StateAction, q: VecN) {
		super.setQValue(sa, q)

		val save = StateSaveActor.SaveState(sa.fromState, stateActions(sa.fromState))

		stateSaveActor ! save

		subscribers.foreach(_ ! StateSaveActor.LoadState(sa.fromState, stateActions(sa.fromState)))
	}

	def actionSelected(states0: States, action: Action) = {
		val states = BotLearn.normalizeWeights(states0)
		bot ! BotClient.ActionSelected(states, action)

		val selQ = qValue(states0.map(x => StateAction(x._1, action) -> x._2))

		log.info(s"Select action $action with q $selQ states $states ")

		//not finding some good actions
		if((selQ * desire) <= 0) lastStateActions = Queue.empty

		if(lastStateActions.length > maxSAQueueLength)
			lastStateActions = lastStateActions.drop(lastStateActions.length - maxSAQueueLength)

		lastStateActions ++= states.map(pair => StateAction(pair._1, action))
	}

	def receive: Receive = viewReceive orElse {
		case BotClient.Subscribe =>
			context watch sender
			subscribers += sender
			stateActions.foreach(pair => sender ! StateSaveActor.LoadState(pair._1, pair._2))
		case Terminated(ref) if subscribers(ref) =>
			subscribers -= ref
			
		case BotClient.Desire(d) => desire = d
		case MaybeSelectGoal => maybeSelectGoal()
		case ClearGoal =>
			currentAction = None
			rewardAcc = VecN.zero
		case SelectGoal =>
			self ! ClearGoal
			self ! MaybeSelectGoal
		case ActionFinished(dt) => actionFinished(dt)
		case AccumReward(r) => rewardAcc += r
		case load @ StateSaveActor.LoadState(state, actions) =>
			stateActions += state -> actions
			subscribers.foreach(_ ! load)
	}

	override def postStop() {
		context stop self
	}
}



