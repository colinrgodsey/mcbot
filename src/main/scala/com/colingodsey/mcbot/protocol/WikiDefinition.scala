package com.colingodsey.mcbot.protocol

import scala.io.Source
import java.io.File

object WikiDefinition {
	val is = getClass.getClassLoader.getResourceAsStream("protocol.html")
//println( scala.xml.XML.load(is))
	val xml = scala.xml.XML.load(is).head
}