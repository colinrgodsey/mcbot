package com.colingodsey.mcbot.protocol

import com.colingodsey.mcbot.protocol._

package object client extends ClientProtocol {
	/*implicit object KeepAlive extends LocalProtocolCompanion[KeepAlive] {
		val codec = codecFrom1(KeepAlive.apply)
	}
	case class KeepAlive(keepAliveId: Int) extends KeepAlive.Protocol(0)

	implicit object JoinGame extends LocalProtocolCompanion[JoinGame] {
		val codec = codecFrom6(JoinGame.apply)
	}
	case class JoinGame(entId: Int, gameMode: Byte, dimension: Byte,
			difficulty: Byte, maxPlayers: Byte, leveltyp: String) extends JoinGame.Protocol(1)
*/

	val Protocol = this: ClientProtocol
}
