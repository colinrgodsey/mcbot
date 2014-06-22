package com.colingodsey.mcbot.world

import akka.actor._

/**
 * Async view of the world
 */
trait WorldViewSubscriber {
	var entities = Map[Int, Entity]()

	def wvSubReceive: Actor.Receive = {
		case e: Entity => entities += e.id -> e
	}
}
