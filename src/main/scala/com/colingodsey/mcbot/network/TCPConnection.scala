package com.colingodsey.mcbot.network

object TCPConnection {
	case class Data(data: IndexedSeq[Byte])

	case object Connected
	case object Close

	//internal
	case object ConnectionClosed
	case class RecvData(data: IndexedSeq[Byte])
}