package com.colingodsey.mcbot.protocol

import scala.xml
import scala.annotation.tailrec
import scala.util.Try

object Generate extends App {
	val theXml = scala.xml.Utility.trim(WikiDefinition.xml)

	implicit class XmlEx(val node: scala.xml.Node) extends AnyVal {
		def allNodes: Seq[xml.Node] = node match {
			case x: xml.Elem => x.child.flatMap(_.allNodes)
			case x => Seq(x)
		}

		def attr(key: String) = node.attribute(key).map(_.text)
	}

	//val allElems = theXml.allNodes

	val content = (theXml \ "div" \ "div" \ "div").filter {
		case a @ <div>{_*}</div> if a.attr("id") == Some("mw-content-text") => true
		case x => false
	}.head

	trait AnyNumberOf[T] { self =>
		def where: PartialFunction[T, Unit]
		def unapply(list: List[T]): Option[(List[T], List[T])] = list match {
			case head :: tail if where.isDefinedAt(head) =>
				val (newFound, newTail) = self.unapply(tail).getOrElse(Nil, Nil)
				Some(head :: newFound, newTail)
			case x => Some(Nil, x)
		}
	}

	object AnyNumberOf {
		def apply[T](_where: PartialFunction[T, Unit]): AnyNumberOf[T] = new AnyNumberOf[T] {
			def where: PartialFunction[T, Unit] = _where
		}
	}

	val AnyNumberOfPs = AnyNumberOf[xml.Node] {
		case (<p>{_*}</p>) =>
		case x if !x.isInstanceOf[xml.Elem] =>
	}

	def scanForPacketDefsClient(nodes: List[xml.Node],
			acc: List[(String, String, xml.Node)] = Nil): List[(String, String, xml.Node)] = nodes match {
		case <h4>{title}</h4> :: AnyNumberOfPs(ps, (table @ <table>{_*}</table>) :: tail) =>
			scanForPacketDefs(tail, (title.text.trim, ps.text, table) :: acc)
		case <h3>{a @ <span>{_*}</span>}</h3> :: _
			if a.attr("id") == Some("Serverbound_2") => acc
		case _ :: tail => scanForPacketDefs(tail, acc)
		case Nil => acc
	}

	var isServer = true
	def scanForPacketDefs(nodes: List[xml.Node],
			acc: List[(String, String, xml.Node)] = Nil): List[(String, String, xml.Node)] = nodes match {
		case <h3>{a @ <span>{_*}</span>}</h3> :: tail
			if a.attr("id") == Some("Serverbound_2") && !isServer =>
			isServer = true
			scanForPacketDefs(tail, acc)
		case _ :: tail if !isServer => scanForPacketDefs(tail, acc)
		case <h4>{title}</h4> :: AnyNumberOfPs(ps, (table @ <table>{_*}</table>) :: tail) =>
			scanForPacketDefs(tail, (title.text.trim, ps.text, table) :: acc)
		case _ :: tail => scanForPacketDefs(tail, acc)
		case Nil => acc
	}

//println(content.child.toList)
	val defNodes = scanForPacketDefs(content.child.toList)

	object ArrayOf {
		val tok = "Array of "

		def unapply(str: String) = {
			if(str.startsWith(tok)) Some(str.substring(tok.length))
			else None
		}
	}


	val typToTyp: PartialFunction[String, String] = {
		case "Unsigned Short" => "Short"
		case "Unsigned Byte" => "Short"
		case "Byte array" => "Seq[Byte]"
		case "Byte Array" => "Seq[Byte]"
		case "strings" => "String"
		case "Bool" => "Boolean"
		case ArrayOf(t) => s"Seq[${typToTyp(t)}]"
		case x => x
	}

	case class PacketField(name: String, typ: String, notes: String) {
		def sanitize = copy(typ = typToTyp(typ), name = saneName)

		def saneName = sanitizeName(name, true)
	}

	def sanitizeName(str: String, lower: Boolean): String = {
		val repd = str.replaceAll(" ", "")

		if(repd.length < 2) return ""

		val head = if(lower) repd.substring(0, 1).toLowerCase
		else repd.substring(0, 1).toUpperCase

		head + repd.substring(1)
	}

	case class PacketDesc(id: Int, name: String, fields: Seq[PacketField]) {
		def saneName = sanitizeName(name, false)

		def sanitize = copy(fields = fields.map(_.sanitize), name = saneName)

		def fieldsString = fields.map(x => x.name + ": " + x.typ).mkString(", ")

		def generate = {
			s"implicit object $name extends ProtocolCompanion[$name] {\n" +
			s"\tval codec = codecFrom${fields.length}($name.apply)\n" +
			s"}\n" +
			s"case class $name($fieldsString) " +
			s"extends $name.Protocol($id)"
		}
	}

	def trySeq[T](dat: Any, name: String)(f: => T): Seq[T] = Try(Seq(f)).recover {
		case x: Throwable =>
			//println(x + " -> " + dat.toString)
			badNames += name
			Nil
	}.get

	var badNames = Set[String]()

	object Td {
		def unapply(elem: xml.Elem) = elem match {
			case xml.Elem(_, "td", _, _, children @ _*) => Some(children.text.trim)
			case _ => None
		}
	}

	object HexTd {
		def unapply(elem: xml.Elem) = elem match {
			case xml.Elem(_, "td", _, _, children @ _*) =>
				val txt = children.text.trim
				if(txt.length > 2 && txt.substring(0, 2) == "0x")
					Some(Integer.parseInt(txt.substring(2), 16))
				else None
			case _ => None
		}
	}

	val defs = defNodes.map { case (name, desc, table) =>
		var pid = -1

		val fields = (table.child).flatMap {
			case a @ xml.Elem(_, "tr", _, _, HexTd(packetId),
					Td(fieldName), Td(fieldType), _*) =>
				pid = packetId
				trySeq(a, name)(PacketField(fieldName, fieldType, ""))
			case a @ xml.Elem(_, "tr", _, _, HexTd(packetId),
					Td(fieldName), Td(fieldType), Td(fieldNotes), _*) =>
				pid = packetId
				trySeq(a, name)(PacketField(fieldName, fieldType, fieldNotes))
			case a @ xml.Elem(_, "tr", _, _, Td(fieldName), Td(fieldType), _*) =>
				trySeq(a, name)(PacketField(fieldName, fieldType, ""))
			case a @ xml.Elem(_, "tr", _, _, Td(fieldName),
					Td(fieldType), Td(fieldNotes), _*) =>
				trySeq(a, name)(PacketField(fieldName, fieldType, fieldNotes))
			case <tr><th>{_*}</th>{_*}</tr> => Nil
			case <tr>{a @ _*}</tr> =>
				//println(a)
				badNames += name
				Nil
			case _ => Nil
		}

		PacketDesc(pid, name, fields).sanitize
	}//.sortBy(_.id)

	val typeSet = defs.flatMap(_.fields.map(_.typ)).toSet
	val keys = defs.map(_.name)

	println(defs.map(_.generate).mkString("\n\n"))
	println(defs.map(_.name).mkString(", "))

	println("bad names " + badNames)

	println("types " + typeSet.toString)
	println("keys " + keys)

	//println(codecFroms)
}
