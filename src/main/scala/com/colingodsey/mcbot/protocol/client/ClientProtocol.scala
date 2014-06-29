package com.colingodsey.mcbot.protocol.client

import com.colingodsey.mcbot.protocol._
import com.colingodsey.mcbot.protocol
import com.colingodsey.mcbot.protocol.Slot
import com.colingodsey.mcbot.protocol.VarInt
import com.colingodsey.mcbot.protocol.BigInt128
import com.colingodsey.mcbot.world

trait ClientProtocol extends Protocol {
	private val bitMasks = for(i <- 0.until(64).toIndexedSeq) yield if(i == 0) 0 else (1 << i) - 1

	sealed trait Command
	sealed trait WorldCommand extends Command
	sealed trait ClientViewCommand extends Command

	sealed abstract class ClientPacketCompanion[T <: Packet](val _packetID: Byte)
			extends LocalPacketCompanion[T](_packetID) with ClientProtocol.Command

	case class BlockRecord(metaData: Byte, blockId: Int, y: Int, z: Int, x: Int) {
		require(y >= 0 && x >= 0 && z >= 0, "negative BlockRecord changes ")
	}
	implicit object BlockRecord extends FieldCodec[BlockRecord] {
		def read(src: DataSource): BlockRecord = {
			val dat = src.read[Int]

			var bitIdx = 0

			def readBits(n: Int) = {
				require(n < 16)

				val oldIdx = bitIdx
				val mask = bitMasks(n)

				bitIdx += n

				(dat >>> (oldIdx - 1) & mask)
			}

			BlockRecord(readBits(4).toByte, readBits(12) & 0xFFF,
				readBits(8) & 0xFF, readBits(4) & 0xF, readBits(4) & 0xF)
		}

		def write(obj: BlockRecord, dest: DataDest): Unit = {
			var out: Int = 0
			var bitIdx = 0

			def writeBits(x: Int, bits: Int) {
				val masked = x & bitMasks(bits)

				out |= masked << bitIdx

				bitIdx += bits
			}

			dest.write(out)
		}
	}

	implicit object Modifier extends FieldCodec[world.Modifier] {
		val codec = codecFrom3(world.Modifier.apply)

		def read(src: DataSource): world.Modifier = codec.read(src)
		def write(obj: world.Modifier, dest: DataDest) = codec.write(obj, dest)
	}

	implicit object PropertyData extends FieldCodec[world.PropertyData] {
		import LengthCodec.ShortLengthCodec
		val codec = codecFrom3(world.PropertyData.apply)

		def read(src: DataSource): world.PropertyData = codec.read(src)
		def write(obj: world.PropertyData, dest: DataDest) = codec.write(obj, dest)
	}

	case class Metadata(key: Byte, typ: Byte, data: Any)
	implicit object MetaDataCodec extends FieldCodec[Seq[Metadata]] {
		implicit def slotCodec = protocol.ShortSlotCodec

		def read(src: DataSource): Seq[Metadata] = {
			var byte = src.readUnsignedByte
			var outSeq = List[Metadata]()

			while(byte != 127) {
				val index = byte & 0x1F
				val t = byte >> 5

				val data = t match {
					case 0 => src.read[Byte]
					case 1 => src.read[Short]
					case 2 => src.read[Int]
					case 3 => src.read[Float]
					case 4 => src.read[String]
					case 5 => src.read[Slot]
					case 6 => src.read(12) //int int int
					case _ => //not sure about this....
				}

				outSeq ::= Metadata(index.toByte, t.toByte, data)

				byte = src.readUnsignedByte
			}

			outSeq
		}

		def write(obj: Seq[Metadata], dest: DataDest) {
			import CodecFromLocal.AnyExt

			for(dat <- obj.reverseIterator) {
				val byte = (dat.key.toInt & 0x1F) | (dat.typ.toInt << 5)
				dest.writeUnsignedByte(byte)
				dat.typ match {
					case 0 => dest.write[Byte](dat.data.aiof)
					case 1 => dest.write[Short](dat.data.aiof)
					case 2 => dest.write[Int](dat.data.aiof)
					case 3 => dest.write[Float](dat.data.aiof)
					case 4 => dest.write[String](dat.data.aiof)
					case 5 => dest.write[Slot](dat.data.aiof)
					case 6 => dest.write(dat.data.asInstanceOf[Seq[Byte]])
					case _ => // again
				}
			}

			dest.writeUnsignedByte(127)
		}
	}


	/*
	Generated code below
	 */
	implicit object KeepAlive extends ClientPacketCompanion[KeepAlive](0) {
		val codec = codecFrom1(KeepAlive.apply)
	}
	case class KeepAlive(keepAliveID: Int) extends KeepAlive.Packet with ClientViewCommand

	implicit object JoinGame extends ClientPacketCompanion[JoinGame](1) {
		val codec = codecFrom6(JoinGame.apply)
	}
	case class JoinGame(entityID: Int, gamemode: UByte, dimension: Byte, difficulty: UByte,
			maxPlayers: UByte, leveltyp: String) extends JoinGame.Packet with ClientViewCommand

	implicit object ChatMessage extends ClientPacketCompanion[ChatMessage](2) {
		val codec = codecFrom1(ChatMessage.apply)
	}
	case class ChatMessage(jSONData: String) extends ChatMessage.Packet with ClientViewCommand

	implicit object TimeUpdate extends ClientPacketCompanion[TimeUpdate](3) {
		val codec = codecFrom2(TimeUpdate.apply)
	}
	case class TimeUpdate(ageoftheworld: Long, timeofday: Long
			) extends TimeUpdate.Packet with ClientViewCommand

	implicit object EntityEquipment extends ClientPacketCompanion[EntityEquipment](4) {
		implicit def slotCodec = protocol.ShortSlotCodec
		val codec = codecFrom3(EntityEquipment.apply)
	}
	case class EntityEquipment(entityID: Int, slot: Short, item: Slot) extends EntityEquipment.Packet with WorldCommand

	implicit object SpawnPosition extends ClientPacketCompanion[SpawnPosition](5) {
		val codec = codecFrom3(SpawnPosition.apply)
	}
	case class SpawnPosition(x: Int, y: Int, z: Int) extends SpawnPosition.Packet

	implicit object UpdateHealth extends ClientPacketCompanion[UpdateHealth](6) {
		val codec = codecFrom3(UpdateHealth.apply)
	}
	case class UpdateHealth(health: Float, food: Short, foodSaturation: Float) extends UpdateHealth.Packet with ClientViewCommand

	implicit object Respawn extends ClientPacketCompanion[Respawn](7) {
		val codec = codecFrom4(Respawn.apply)
	}
	case class Respawn(dimension: Int, difficulty: UByte, gamemode: UByte, leveltyp: String) extends Respawn.Packet

	implicit object PositionAndLook extends ClientPacketCompanion[PositionAndLook](8) {
		val codec = codecFrom6(PositionAndLook.apply)
	}
	case class PositionAndLook(x: Double, y: Double, z: Double, yaw: Float,
			pitch: Float, onGround: Boolean) extends PositionAndLook.Packet with ClientViewCommand

	implicit object HeldItemChange extends ClientPacketCompanion[HeldItemChange](9) {
		val codec = codecFrom1(HeldItemChange.apply)
	}
	case class HeldItemChange(slot: Byte) extends HeldItemChange.Packet with ClientViewCommand

	implicit object UseBed extends ClientPacketCompanion[UseBed](10) {
		val codec = codecFrom4(UseBed.apply)
	}
	case class UseBed(entityID: Int, x: Int, y: UByte, z: Int) extends UseBed.Packet

	implicit object Animation extends ClientPacketCompanion[Animation](11) {
		val codec = codecFrom2(Animation.apply)
	}
	case class Animation(entityID: VarInt, animation: UByte) extends Animation.Packet

	implicit object SpawnPlayer extends ClientPacketCompanion[SpawnPlayer](12) {
		val codec = codecFrom10(SpawnPlayer.apply)
	}
	case class SpawnPlayer(entityID: VarInt, playerUUID: String, playerName: String,
			x: Int, y: Int, z: Int, yaw: Byte, pitch: Byte, currentItem: Short,
			metadata: Seq[Metadata]) extends SpawnPlayer.Packet with WorldCommand

	implicit object CollectItem extends ClientPacketCompanion[CollectItem](13) {
		val codec = codecFrom2(CollectItem.apply)
	}
	case class CollectItem(collectedEntityID: Int, collectorEntityID: Int) extends CollectItem.Packet

	implicit object SpawnObject extends ClientPacketCompanion[SpawnObject](14) {
		val codec = codecFrom11(SpawnObject.apply)
	}
	//TODO: deltas only there for metaData != 1
	case class SpawnObject(entityID: VarInt, typ: Byte, x: Int, y: Int,
			z: Int, pitch: Byte, yaw: Byte,
			//data: ObjectData) extends SpawnObject.Protocol(14)
			metaData: Int, dx: Short, dy: Short, dz: Short) extends SpawnObject.Packet with WorldCommand

	implicit object SpawnMob extends ClientPacketCompanion[SpawnMob](15) {
		val codec = codecFrom12(SpawnMob.apply)
	}
	case class SpawnMob(entityID: VarInt, typ: UByte, x: Int, y: Int, z: Int,
			pitch: Byte, headPitch: Byte, yaw: Byte, velocityX: Short,
			velocityY: Short, velocityZ: Short, metadata: Seq[Metadata]) extends SpawnMob.Packet with WorldCommand

	implicit object SpawnPainting extends ClientPacketCompanion[SpawnPainting](16) {
		val codec = codecFrom6(SpawnPainting.apply)
	}
	case class SpawnPainting(entityID: VarInt, title: String, x: Int, y: Int, z: Int,
			direction: Int) extends SpawnPainting.Packet

	implicit object SpawnExperienceOrb extends ClientPacketCompanion[SpawnExperienceOrb](17) {
		val codec = codecFrom5(SpawnExperienceOrb.apply)
	}
	case class SpawnExperienceOrb(entityID: VarInt, x: Int, y: Int, z: Int,
			count: Short) extends SpawnExperienceOrb.Packet

	implicit object EntityVelocity extends ClientPacketCompanion[EntityVelocity](18) {
		val codec = codecFrom4(EntityVelocity.apply)
	}
	case class EntityVelocity(entityID: Int, velocityX: Short, velocityY: Short,
			velocityZ: Short) extends EntityVelocity.Packet with WorldCommand

	implicit object DestroyEntities extends ClientPacketCompanion[DestroyEntities](19) {
		import LengthCodec.ByteLengthCodec

		val codec = codecFrom1(DestroyEntities.apply)
	}
	case class DestroyEntities(entityIDs: Seq[Int]) extends DestroyEntities.Packet with WorldCommand

	implicit object Entity extends ClientPacketCompanion[Entity](20) {
		val codec = codecFrom1(Entity.apply)
	}
	case class Entity(entityID: Int) extends Entity.Packet

	implicit object EntityRelativeMove extends ClientPacketCompanion[EntityRelativeMove](21) {
		val codec = codecFrom4(EntityRelativeMove.apply)
	}
	case class EntityRelativeMove(entityID: Int, dX: Byte, dY: Byte,
			dZ: Byte) extends EntityRelativeMove.Packet with WorldCommand

	implicit object EntityLook extends ClientPacketCompanion[EntityLook](22) {
		val codec = codecFrom3(EntityLook.apply)
	}
	case class EntityLook(entityID: Int,
			yaw: Byte, pitch: Byte) extends EntityLook.Packet with WorldCommand

	implicit object EntityLookandRelativeMove extends ClientPacketCompanion[EntityLookandRelativeMove](23) {
		val codec = codecFrom6(EntityLookandRelativeMove.apply)
	}
	case class EntityLookandRelativeMove(entityID: Int, dX: Byte, dY: Byte, dZ: Byte,
			yaw: Byte, pitch: Byte) extends EntityLookandRelativeMove.Packet with WorldCommand

	implicit object EntityTeleport extends ClientPacketCompanion[EntityTeleport](24) {
		val codec = codecFrom6(EntityTeleport.apply)
	}
	case class EntityTeleport(entityID: Int, x: Int, y: Int, z: Int,
			yaw: Byte, pitch: Byte) extends EntityTeleport.Packet with WorldCommand

	implicit object EntityHeadLook extends ClientPacketCompanion[EntityHeadLook](25) {
		val codec = codecFrom2(EntityHeadLook.apply)
	}
	case class EntityHeadLook(entityID: Int, headYaw: Byte
			) extends EntityHeadLook.Packet with WorldCommand

	implicit object EntityStatus extends ClientPacketCompanion[EntityStatus](26) {
		val codec = codecFrom2(EntityStatus.apply)
	}
	case class EntityStatus(entityID: Int, entityStatus: Byte
			) extends EntityStatus.Packet with WorldCommand

	implicit object AttachEntity extends ClientPacketCompanion[AttachEntity](27) {
		val codec = codecFrom3(AttachEntity.apply)
	}
	case class AttachEntity(entityID: Int, vehicleID: Int, leash: Boolean) extends AttachEntity.Packet

	implicit object EntityMetadata extends ClientPacketCompanion[EntityMetadata](28) {
		val codec = codecFrom2(EntityMetadata.apply)
	}
	case class EntityMetadata(entityID: Int, metadata: Seq[Metadata]) extends EntityMetadata.Packet with WorldCommand

	implicit object EntityEffect extends ClientPacketCompanion[EntityEffect](29) {
		val codec = codecFrom4(EntityEffect.apply)
	}
	case class EntityEffect(entityID: Int, effectID: Byte, amplifier: Byte, duration: Short) extends EntityEffect.Packet

	implicit object RemoveEntityEffect extends ClientPacketCompanion[RemoveEntityEffect](30) {
		val codec = codecFrom2(RemoveEntityEffect.apply)
	}
	case class RemoveEntityEffect(entityID: Int, effectID: Byte) extends RemoveEntityEffect.Packet

	implicit object SetExperience extends ClientPacketCompanion[SetExperience](31) {
		val codec = codecFrom3(SetExperience.apply)
	}
	case class SetExperience(experiencebar: Float, level: Short, totalExperience: Short) extends SetExperience.Packet

	implicit object EntityProperties extends ClientPacketCompanion[EntityProperties](32) {
		import LengthCodec.IntLengthCodec

		val codec = codecFrom2(EntityProperties.apply)
	}
	case class EntityProperties(entityID: Int,
			properties: Seq[world.PropertyData]) extends EntityProperties.Packet with WorldCommand

	implicit object ChunkData extends ClientPacketCompanion[ChunkData](33) {
		import LengthCodec.IntLengthCodec
		val codec = codecFrom6(ChunkData.apply)
	}
	case class ChunkData(chunkX: Int, chunkZ: Int, groundUpcontinuous: Boolean,
			primaryBitmask: Short, addBitmask: Short, data: Seq[Byte]) extends ChunkData.Packet with WorldCommand

	implicit object MultiBlockChange extends ClientPacketCompanion[MultiBlockChange](34) {
		import LengthCodec.IntLengthCodec

		val codec = codecFrom4(MultiBlockChange.apply)
	}
	case class MultiBlockChange(chunkX: Int, chunkZ: Int, recordCount: Short,
			recordData: Seq[Byte]/*records: Seq[BlockRecord]*/) extends MultiBlockChange.Packet {
		val records = readFields[BlockRecord](DataSource(recordData), recordCount)
	}

	implicit object BlockChange extends ClientPacketCompanion[BlockChange](35) {
		val codec = codecFrom5(BlockChange.apply)
	}
	case class BlockChange(x: Int, y: UByte, z: Int, blockID: VarInt,
			blockMetadata: UByte) extends BlockChange.Packet

	implicit object BlockAction extends ClientPacketCompanion[BlockAction](36) {
		val codec = codecFrom6(BlockAction.apply)
	}
	case class BlockAction(x: Int, y: Short, z: Int, byte1: UByte, byte2: UByte, blockType: VarInt) extends BlockChange.Packet

	implicit object BlockBreakAnimation extends ClientPacketCompanion[BlockBreakAnimation](37) {
		val codec = codecFrom5(BlockBreakAnimation.apply)
	}
	case class BlockBreakAnimation(entityID: VarInt, x: Int, y: Int, z: Int, destroyStage: Byte
			) extends BlockBreakAnimation.Packet

	implicit object MapChunkBulk extends ClientPacketCompanion[MapChunkBulk](38) {
		//val codec = codecFrom5(MapChunkBulk.apply)
		val codec = new FieldCodec[MapChunkBulk] {
			def read(src: DataSource): MapChunkBulk = {
				//MapChunkBulk(src.read, src.read])
				val colCount: Short = src.read[Short]
				val datLength: Int = src.read[Int]
				val skyLightSent: Boolean = src.read[Boolean]
				val data: Seq[Byte] = readFields[Byte](src, datLength)

				val metas = 0 until colCount map { _ =>
					MapChunkMeta(src.read[Int], src.read[Int], src.read[Short], src.read[Short])
				}

				MapChunkBulk(colCount, skyLightSent, data, metas)
			}
			def write(obj: MapChunkBulk, dest: DataDest): Unit = {
				dest write obj.chunkColumnCount
				dest write obj.data.length.toInt
				dest write obj.skyLightSent
				obj.data.foreach(dest.write(_))
				obj.metas foreach { meta =>
					dest write meta.chunkX
					dest write meta.chunkZ
					dest write meta.primaryBitmask
					dest write meta.addBitmask
				}
			}
		}
	}
	case class MapChunkBulk(chunkColumnCount: Short, skyLightSent: Boolean,
			data: Seq[Byte], metas: Seq[MapChunkMeta]) extends MapChunkBulk.Packet with WorldCommand
	case class MapChunkMeta(chunkX: Int, chunkZ: Int, primaryBitmask: Short,
			addBitmask: Short)

	implicit object Explosion extends ClientPacketCompanion[Explosion](39) {
		implicit val tupCodec = codecFrom3({ (a: Byte, b: Byte, c: Byte) =>
			(a, b, c)
		})
		import LengthCodec.IntLengthCodec

		val codec = codecFrom8(Explosion.apply)
	}
	case class Explosion(x: Float, y: Float, z: Float, radius: Float,
			records: Seq[(Byte, Byte, Byte)], playerMotionX: Float, playerMotionY: Float,
			playerMotionZ: Float) extends Explosion.Packet

	implicit object Effect extends ClientPacketCompanion[Effect](40) {
		val codec = codecFrom6(Effect.apply)
	}
	case class Effect(effectID: Int, x: Int, y: Byte, z: Int, data: Int,
			disablerelativevolume: Boolean) extends Effect.Packet

	implicit object SoundEffect extends ClientPacketCompanion[SoundEffect](41) {
		val codec = codecFrom6(SoundEffect.apply)
	}
	case class SoundEffect(soundname: String, effectpositionX: Int, effectpositionY: Int,
			effectpositionZ: Int, volume: Float, pitch: UByte) extends SoundEffect.Packet with ClientViewCommand

	implicit object Particle extends ClientPacketCompanion[Particle](42) {
		val codec = codecFrom9(Particle.apply)
	}
	case class Particle(particlename: String, x: Float, y: Float, z: Float, offsetX: Float, offsetY: Float,
			offsetZ: Float, particledata: Float, numberofparticles: Int) extends Particle.Packet

	implicit object ChangeGameState extends ClientPacketCompanion[ChangeGameState](43) {
		val codec = codecFrom2(ChangeGameState.apply)
	}
	case class ChangeGameState(reason: UByte, value: Float) extends ChangeGameState.Packet with WorldCommand

	implicit object SpawnGlobalEntity extends ClientPacketCompanion[SpawnGlobalEntity](44) {
		val codec = codecFrom5(SpawnGlobalEntity.apply)
	}
	case class SpawnGlobalEntity(entityID: VarInt, typ: Byte, x: Int, y: Int, z: Int
			) extends SpawnGlobalEntity.Packet

	implicit object OpenWindow extends ClientPacketCompanion[OpenWindow](45) {
		val codec = codecFrom6(OpenWindow.apply)
	}
	case class OpenWindow(windowid: UByte, inventorytyp: UByte, windowtitle: String, numberofSlots: UByte,
			useprovidedwindowtitle: Boolean, entityID: Int) extends OpenWindow.Packet

	implicit object CloseWindow extends ClientPacketCompanion[CloseWindow](46) {
		val codec = codecFrom1(CloseWindow.apply)
	}
	case class CloseWindow(windowID: UByte) extends CloseWindow.Packet

	implicit object SetSlot extends ClientPacketCompanion[SetSlot](47) {
		implicit def slotCodec = protocol.ShortSlotCodec
		val codec = codecFrom3(SetSlot.apply)
	}
	case class SetSlot(windowID: Byte, slot: Short, slotdata: Slot) extends SetSlot.Packet

	implicit object WindowItems extends ClientPacketCompanion[WindowItems](48) {
		import LengthCodec.ShortLengthCodec
		implicit def slotCodec = protocol.ShortSlotCodec
		val codec = codecFrom2(WindowItems.apply)
	}
	case class WindowItems(windowID: UByte, slots: Seq[Slot]) extends WindowItems.Packet

	implicit object WindowProperty extends ClientPacketCompanion[WindowProperty](49) {
		val codec = codecFrom3(WindowProperty.apply)
	}
	case class WindowProperty(windowID: UByte, property: Short, value: Short) extends WindowProperty.Packet

	implicit object ConfirmTransaction extends ClientPacketCompanion[ConfirmTransaction](50) {
		val codec = codecFrom3(ConfirmTransaction.apply)
	}
	case class ConfirmTransaction(windowID: UByte, actionnumber: Short,
			accepted: Boolean) extends ConfirmTransaction.Packet

	implicit object UpdateSign extends ClientPacketCompanion[UpdateSign](51) {
		val codec = codecFrom7(UpdateSign.apply)
	}
	case class UpdateSign(x: Int, y: Short, z: Int, line1: String, line2: String,
			line3: String, line4: String) extends UpdateSign.Packet

	implicit object Maps extends ClientPacketCompanion[Maps](52) {
		import LengthCodec.ShortLengthCodec
		val codec = codecFrom2(Maps.apply)
	}
	case class Maps(itemDamage: VarInt, data: Seq[Byte]) extends Maps.Packet

	implicit object UpdateBlockEntity extends ClientPacketCompanion[UpdateBlockEntity](53) {
		import LengthCodec.ShortLengthCodec
		val codec = codecFrom5(UpdateBlockEntity.apply)
	}
	case class UpdateBlockEntity(x: Int, y: Short, z: Int, action: UByte,
			nBTData: Seq[Byte]) extends UpdateBlockEntity.Packet

	implicit object SignEditorOpen extends ClientPacketCompanion[SignEditorOpen](54) {
		val codec = codecFrom3(SignEditorOpen.apply)
	}
	case class SignEditorOpen(x: Int, y: Int, z: Int) extends SignEditorOpen.Packet

	implicit object Statistics extends ClientPacketCompanion[Statistics](55) {
		import LengthCodec.VarIntLengthCodec

		implicit val tupCodec = codecFrom2({ (a: String, b: VarInt) =>
			(a, b)
		})

		val codec = codecFrom1(Statistics.apply)
	}
	case class Statistics(entries: Seq[(String, VarInt)]) extends Statistics.Packet

	implicit object PlayerListItem extends ClientPacketCompanion[PlayerListItem](56) {
		val codec = codecFrom3(PlayerListItem.apply)
	}
	case class PlayerListItem(playername: String, online: Boolean, ping: Short) extends PlayerListItem.Packet with ClientViewCommand

	implicit object PlayerAbilities extends ClientPacketCompanion[PlayerAbilities](57) {
		val codec = codecFrom3(PlayerAbilities.apply)
	}
	case class PlayerAbilities(flags: Byte, flyingspeed: Float,
			walkingspeed: Float) extends PlayerAbilities.Packet

	implicit object TabComplete extends ClientPacketCompanion[TabComplete](58) {
		val codec = codecFrom2(TabComplete.apply)
	}
	case class TabComplete(count: VarInt, theMatch: String) extends TabComplete.Packet

	implicit object ScoreboardObjective extends ClientPacketCompanion[ScoreboardObjective](59) {
		val codec = codecFrom3(ScoreboardObjective.apply)
	}
	case class ScoreboardObjective(objectivename: String, objectivevalue: String,
			createRemove: Byte) extends ScoreboardObjective.Packet

	implicit object UpdateScore extends ClientPacketCompanion[UpdateScore](60) {
		val codec = codecFrom4(UpdateScore.apply)
	}
	case class UpdateScore(itemName: String, updateRemove: Byte, scoreName: String,
			value: Int) extends UpdateScore.Packet

	implicit object DisplayScoreboard extends ClientPacketCompanion[DisplayScoreboard](61) {
		val codec = codecFrom2(DisplayScoreboard.apply)
	}
	case class DisplayScoreboard(position: Byte, scoreName: String) extends DisplayScoreboard.Packet

	implicit object Teams extends ClientPacketCompanion[Teams](62) {
		import LengthCodec.ShortLengthCodec
		val codec = codecFrom7(Teams.apply)
	}
	case class Teams(teamName: String, mode: Byte, teamDisplayName: String,
			teamPrefix: String, teamSuffix: String, friendlyfire: Byte,
			players: Seq[String]) extends Teams.Packet

	implicit object PluginMessage extends ClientPacketCompanion[PluginMessage](63) {
		import LengthCodec.ShortLengthCodec
		val codec = codecFrom2(PluginMessage.apply)
	}
	case class PluginMessage(channel: String, data: Seq[Byte]) extends PluginMessage.Packet with ClientViewCommand

	implicit object Disconnect extends ClientPacketCompanion[Disconnect](64) {
		val codec = codecFrom1(Disconnect.apply)
	}
	case class Disconnect(reason: String) extends Disconnect.Packet

	//EncryptionResponse, LoginStart, LoginSuccess, EncryptionRequest, Disconnect, Ping, Request, Ping, Response, PluginMessage, ClientStatus, ClientSettings, Tab-Complete, PlayerAbilities, UpdateSign, EnchantItem, ConfirmTransaction, ClickWindow, CloseWindow, SteerVehicle, EntityAction, Animation, HeldItemChange, PlayerBlockPlacement, PlayerDigging, PlayerPositionAndLook, PlayerPosition, Player, UseEntity, ChatMessage, KeepAlive, Disconnect, PluginMessage, Teams, DisplayScoreboard, UpdateScore, ScoreboardObjective, Tab-Complete, PlayerAbilities, PlayerListItem, Statistics, SignEditorOpen, UpdateBlockEntity, Maps, UpdateSign, ConfirmTransaction, WindowProperty, SetSlot, CloseWindow, OpenWindow, SpawnGlobalEntity, ChangeGameState, Particle, SoundEffect, Effect, Explosion, MapChunkBulk, BlockBreakAnimation, BlockChange, MultiBlockChange, ChunkData, EntityProperties, SetExperience, RemoveEntityEffect, EntityEffect, EntityMetadata, AttachEntity, EntityStatus, EntityHeadLook, EntityTeleport, EntityLookandRelativeMove, EntityLook, EntityRelativeMove, Entity, DestroyEntities, EntityVelocity, SpawnExperienceOrb, SpawnPainting, SpawnMob, SpawnObject, CollectItem, SpawnPlayer, Animation, UseBed, HeldItemChange, Respawn, UpdateHealth, SpawnPosition, EntityEquipment, TimeUpdate, ChatMessage, JoinGame, KeepAlive, Handshake
	//
	val packets = Set[PacketCompanion[_]](Disconnect, PluginMessage, Teams, DisplayScoreboard,
		UpdateScore, ScoreboardObjective, TabComplete, PlayerAbilities, PlayerListItem, Statistics,
		SignEditorOpen, UpdateBlockEntity, Maps, UpdateSign, ConfirmTransaction, WindowProperty,
		WindowItems, SetSlot, CloseWindow, OpenWindow, SpawnGlobalEntity, ChangeGameState, Particle,
		SoundEffect, Effect, Explosion, MapChunkBulk, BlockBreakAnimation, BlockChange,
		MultiBlockChange, ChunkData, EntityProperties, SetExperience, RemoveEntityEffect,
		EntityEffect, EntityMetadata, AttachEntity, EntityStatus, EntityHeadLook, EntityTeleport,
		EntityLookandRelativeMove, EntityLook, EntityRelativeMove, Entity, DestroyEntities,
		EntityVelocity, SpawnExperienceOrb, SpawnPainting, SpawnMob, SpawnObject, CollectItem,
		SpawnPlayer, Animation, UseBed, HeldItemChange, PositionAndLook, Respawn, UpdateHealth, SpawnPosition,
		EntityEquipment, TimeUpdate, ChatMessage, JoinGame, KeepAlive)


}