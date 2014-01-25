package com.colingodsey.mcbot.world

import com.colingodsey.logos.collections.Vec3D
import com.colingodsey.mcbot.protocol._

//object, mob, player
trait Entity extends Product {
	import Entity._

	def id: Int
	def typ: Int
	def pos: Vec3D
	def vel: Vec3D
	def yaw: Double
	def pitch: Double
	def onGround: Boolean
	def status: Int
	def props: Map[String, PropertyData]
	
	//def update(id: Int = id, pos: Point3D = pos, vel: Point3D = vel, yaw: Double = yaw, )
	
	def entityCopy(pos: Vec3D = pos,
		vel: Vec3D = vel,
		yaw: Double = yaw,
		pitch: Double = pitch,
		onGround: Boolean = onGround,
		status: Int = status,
		props: Map[String, PropertyData] = props): Entity

	def prop(prop: String) =
		props.get(prop).map(_.value).getOrElse(defaultProp(prop))
}

trait LivingEntity extends Entity {
	def headAngle: Double
	def health: Double
	def typ: Int

	def livingCopy(headAngle: Double = headAngle,
			health: Double = health): LivingEntity

	def look = (yaw, pitch, headAngle)
}

case class Modifier(uuid: BigInt128, amount: Double, operation: Byte)
case class PropertyData(key: String, value: Double, modifiers: Seq[Modifier])

case class Mob(id: Int, typ: Int,
		pos: Vec3D = Vec3D.origin,
		vel: Vec3D = Vec3D.origin,
		yaw: Double = 0,
		pitch: Double = 0,
		props: Map[String, PropertyData] = Map(),
		headAngle: Double = 0,
		health: Double = 0,
		onGround: Boolean = false,
		status: Int = 0
		) extends LivingEntity {

	def livingCopy(headAngle: Double,
			health: Double): LivingEntity =
		copy(headAngle = headAngle, health = health)

	def entityCopy(pos: Vec3D,
			vel: Vec3D,
			yaw: Double,
			pitch: Double,
			onGround: Boolean,
			status: Int,
			props: Map[String, PropertyData]): Entity =
		copy(pos = pos, vel = vel, yaw = yaw, pitch = pitch, props = props, status = status)
}

case class Player(id: Int,
		pos: Vec3D = Vec3D.origin,
		vel: Vec3D = Vec3D.origin,
		yaw: Double = 90,
		pitch: Double = 90,
		props: Map[String, PropertyData] = Map(),
		headAngle: Double = 0,
		health: Double = 0,
		onGround: Boolean = false,
		status: Int = 0
		) extends LivingEntity {

	def typ: Int = 0 //default??

	def livingCopy(headAngle: Double,
			health: Double): LivingEntity =
		copy(headAngle = headAngle, health = health)

	def entityCopy(pos: Vec3D,
			vel: Vec3D,
			yaw: Double,
			pitch: Double,
			onGround: Boolean,
			status: Int = status,
			props: Map[String, PropertyData]): Entity =
		copy(pos = pos, vel = vel, yaw = yaw, pitch = pitch, props = props,
			onGround = onGround, status = status)
}

object Entity {
	val defaultProp: String => Double = Map(
		"generic.maxHealth" ->              20.0,
		"generic.followRange" -> 	        32.0,
		"generic.knockbackResistance" ->    0.0,
		"generic.movementSpeed" -> 	        0.699999988079071,
		"generic.attackDamage" -> 	        2.0,
		"horse.jumpStrength" -> 	        0.7,
		"zombie.spawnReinforcements" ->     0.0
	)
}

case class ObjectEntity(id: Int, typ: Int,
			pos: Vec3D = Vec3D.origin,
			vel: Vec3D = Vec3D.origin,
			yaw: Double = 0,
			pitch: Double = 0,
			props: Map[String, PropertyData] = Map(),
			onGround: Boolean = false,
			status: Int = 0
		) extends Entity {

	def entityCopy(pos: Vec3D,
			vel: Vec3D,
			yaw: Double,
			pitch: Double,
			onGround: Boolean,
			status: Int = status,
			props: Map[String, PropertyData]): Entity =
		copy(pos = pos, vel = vel, yaw = yaw, pitch = pitch, onGround = onGround,
			props = props, status = status)
}