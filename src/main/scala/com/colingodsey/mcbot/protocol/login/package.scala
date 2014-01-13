package com.colingodsey.mcbot.protocol

import com.colingodsey.mcbot.protocol._
package object login {
	object server extends Protocol  {
		implicit object LoginStart extends LocalPacketCompanion[LoginStart](0) {
			val codec = codecFrom1(LoginStart.apply)
		}
		case class LoginStart(name: String) extends LoginStart.Packet

		implicit object EncryptionResponse extends LocalPacketCompanion[EncryptionResponse](1) {
			import LengthCodec.ShortLengthCodec
			val codec = codecFrom2(EncryptionResponse.apply)
		}
		case class EncryptionResponse(/*length: Short, */sharedSecret: Seq[Byte],
			/*length: Short, */verifyToken: Seq[Byte]) extends EncryptionResponse.Packet

		val packets = Set[PacketCompanion[_]](LoginStart, EncryptionResponse)
	}

	object client extends Protocol  {
		implicit object Disconnect extends LocalPacketCompanion[Disconnect](0) {
			val codec = codecFrom1(Disconnect.apply)
		}
		case class Disconnect(jSONData: String) extends Disconnect.Packet

		implicit object EncryptionRequest extends LocalPacketCompanion[EncryptionRequest](1) {
			import LengthCodec.ShortLengthCodec
			val codec = codecFrom3(EncryptionRequest.apply)
		}
		case class EncryptionRequest(serverID: String, publicKey: Seq[Byte],
			verifyToken: Seq[Byte]) extends EncryptionRequest.Packet

		implicit object LoginSuccess extends LocalPacketCompanion[LoginSuccess](2) {
			val codec = codecFrom2(LoginSuccess.apply)
		}
		case class LoginSuccess(uuid: String, username: String) extends LoginSuccess.Packet

		val packets = Set[PacketCompanion[_]](Disconnect, EncryptionRequest, LoginSuccess)
	}

}