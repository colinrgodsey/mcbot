package com.colingodsey.mcbot.protocol.server

import com.colingodsey.mcbot.protocol._
import com.colingodsey.mcbot.protocol

trait ServerProtocol {
	implicit object KeepAlive extends LocalPacketCompanion[KeepAlive](0) {
		val codec = codecFrom1(KeepAlive.apply)
	}
	case class KeepAlive(keepAliveID: Int) extends KeepAlive.Packet

	implicit object ChatMessage extends LocalPacketCompanion[ChatMessage](1) {
		val codec = codecFrom1(ChatMessage.apply)
	}
	case class ChatMessage(message: String) extends ChatMessage.Packet

	implicit object UseEntity extends LocalPacketCompanion[UseEntity](2) {
		val codec = codecFrom2(UseEntity.apply)
	}
	case class UseEntity(target: Int, mouse: Byte) extends UseEntity.Packet

	implicit object Player extends LocalPacketCompanion[Player](3) {
		val codec = codecFrom1(Player.apply)
	}
	case class Player(onGround: Boolean) extends Player.Packet

	implicit object PlayerPosition extends LocalPacketCompanion[PlayerPosition](4) {
		val codec = codecFrom5(PlayerPosition.apply)
	}
	case class PlayerPosition(x: Double, stance: Double, y: Double, z: Double,
			onGround: Boolean) extends PlayerPosition.Packet

	implicit object PlayerLook extends LocalPacketCompanion[PlayerLook](5) {
		val codec = codecFrom3(PlayerLook.apply)
	}
	case class PlayerLook(yaw: Float, pitch: Float, onGround: Boolean) extends PlayerLook.Packet

	implicit object PlayerPositionAndLook extends LocalPacketCompanion[PlayerPositionAndLook](6) {
		val codec = codecFrom7(PlayerPositionAndLook.apply)
	}
	case class PlayerPositionAndLook(x: Double, stance: Double, y: Double, z: Double,
			yaw: Float, pitch: Float, onGround: Boolean) extends PlayerPositionAndLook.Packet

	implicit object PlayerDigging extends LocalPacketCompanion[PlayerDigging](7) {
		val codec = codecFrom5(PlayerDigging.apply)
	}
	case class PlayerDigging(status: Byte, x: Int, y: Byte, z: Int, face: Byte) extends PlayerDigging.Packet

	implicit object PlayerBlockPlacement extends LocalPacketCompanion[PlayerBlockPlacement](8) {
		implicit def slotCodec = protocol.ShortSlotCodec
		val codec = codecFrom8(PlayerBlockPlacement.apply)
	}
	case class PlayerBlockPlacement(x: Int, y: UByte, z: Int, direction: Byte, helditem: Slot,
			cursorpositionX: Byte, cursorpositionY: Byte, cursorpositionZ: Byte) extends PlayerBlockPlacement.Packet

	implicit object HeldItemChange extends LocalPacketCompanion[HeldItemChange](9) {
		val codec = codecFrom1(HeldItemChange.apply)
	}
	case class HeldItemChange(slot: Short) extends HeldItemChange.Packet

	implicit object Animation extends LocalPacketCompanion[Animation](10) {
		val codec = codecFrom2(Animation.apply)
	}
	case class Animation(entityID: Int, animation: Byte) extends Animation.Packet

	implicit object EntityAction extends LocalPacketCompanion[EntityAction](11) {
		val codec = codecFrom3(EntityAction.apply)
	}
	case class EntityAction(entityID: Int, actionID: Byte,
			jumpBoost: Int) extends EntityAction.Packet

	implicit object SteerVehicle extends LocalPacketCompanion[SteerVehicle](12) {
		val codec = codecFrom4(SteerVehicle.apply)
	}
	case class SteerVehicle(sideways: Float, forward: Float, jump: Boolean,
			unmount: Boolean) extends SteerVehicle.Packet

	implicit object CloseWindow extends LocalPacketCompanion[CloseWindow](13) {
		val codec = codecFrom1(CloseWindow.apply)
	}
	case class CloseWindow(windowid: Byte) extends CloseWindow.Packet

	implicit object ClickWindow extends LocalPacketCompanion[ClickWindow](14) {
		implicit def slotCodec = protocol.ShortSlotCodec
		val codec = codecFrom6(ClickWindow.apply)
	}
	case class ClickWindow(windowID: Byte, slot: Short, button: Byte, actionnumber: Short,
			mode: Byte, clickeditem: Slot) extends ClickWindow.Packet

	implicit object ConfirmTransaction extends LocalPacketCompanion[ConfirmTransaction](15) {
		val codec = codecFrom3(ConfirmTransaction.apply)
	}
	case class ConfirmTransaction(windowID: Byte, actionnumber: Short,
			accepted: Boolean) extends ConfirmTransaction.Packet

	implicit object EnchantItem extends LocalPacketCompanion[EnchantItem](17) {
		val codec = codecFrom2(EnchantItem.apply)
	}
	case class EnchantItem(windowID: Byte, enchantment: Byte) extends EnchantItem.Packet

	implicit object UpdateSign extends LocalPacketCompanion[UpdateSign](18) {
		val codec = codecFrom7(UpdateSign.apply)
	}
	case class UpdateSign(x: Int, y: Short, z: Int, line1: String, line2: String,
			line3: String, line4: String) extends UpdateSign.Packet

	implicit object PlayerAbilities extends LocalPacketCompanion[PlayerAbilities](19) {
		val codec = codecFrom3(PlayerAbilities.apply)
	}
	case class PlayerAbilities(flags: Byte, flyingspeed: Float,
			walkingspeed: Float) extends PlayerAbilities.Packet

	implicit object TabComplete extends LocalPacketCompanion[TabComplete](20) {
		val codec = codecFrom1(TabComplete.apply)
	}
	case class TabComplete(text: String) extends TabComplete.Packet

	implicit object ClientSettings extends LocalPacketCompanion[ClientSettings](21) {
		val codec = codecFrom6(ClientSettings.apply)
	}
	case class ClientSettings(locale: String, viewdistance: Byte, chatflags: Byte,
			chatcolours: Boolean, difficulty: Byte, showCape: Boolean) extends ClientSettings.Packet

	implicit object ClientStatus extends LocalPacketCompanion[ClientStatus](22) {
		val codec = codecFrom1(ClientStatus.apply)

		lazy val Respawn = new ClientStatus(0)
	}
	case class ClientStatus(actionID: Byte) extends ClientStatus.Packet

	implicit object PluginMessage extends LocalPacketCompanion[PluginMessage](23) {
		import LengthCodec.ShortLengthCodec

		val codec = codecFrom2(PluginMessage.apply)
	}
	case class PluginMessage(channel: String, data: Seq[Byte]) extends PluginMessage.Packet

	val packets = Set[PacketCompanion[_]](PluginMessage, ClientStatus, ClientSettings, TabComplete, PlayerAbilities,
		UpdateSign, EnchantItem, ConfirmTransaction, ClickWindow, CloseWindow, SteerVehicle,
		EntityAction, Animation, HeldItemChange, PlayerBlockPlacement, PlayerDigging,
		PlayerPositionAndLook, PlayerLook, PlayerPosition, Player, UseEntity, ChatMessage, KeepAlive)
}
