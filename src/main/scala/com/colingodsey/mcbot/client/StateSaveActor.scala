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

class StateSaveActor(botThink: ActorRef) extends Actor with ActorLogging {
	import StateSaveActor._

	implicit def ec = context.system.dispatcher

	def saveDelay = 5.seconds

	val dbFileName = "./state.db"
	val createTable = "CREATE TABLE IF NOT EXISTS tile_states ( " +
			"x INTEGER, " +
			"y INTEGER, " +
			"z INTEGER, " +
			"state VARBINARY(10240) NOT NULL, " +
			"actions VARBINARY(40000) NOT NULL, " +
			"CONSTRAINT pk_pos PRIMARY KEY (x,y,z) )"
	//val createAttachmentsTable = "CREATE TABLE IF NOT EXISTS attachments ( " +
	val indexes = Seq(
		"CREATE INDEX IF NOT EXISTS x_index ON tile_states(x)",
		"CREATE INDEX IF NOT EXISTS y_index ON tile_states(y)",
		"CREATE INDEX IF NOT EXISTS z_index ON tile_states(z)"
	)

	var cachedSaves = Map[BotLearn.State, SaveState]()

	object SaveCached

	val saveTimer = context.system.scheduler.schedule(15.seconds, saveDelay, self, SaveCached)

	val hsqlProps = new HsqlProperties
	val server = new org.hsqldb.Server()
	server.setProperties(hsqlProps)
	server.setDatabaseName(0, "mcbot");
	server.setDatabasePath(0, "file:db/db");
	//server.start()

	classOf[org.hsqldb.jdbcDriver]
	//val con = DriverManager.getConnection("jdbc:hsqldb:hsql://localhost/mcbot", "sa", "")
	val con = DriverManager.getConnection("jdbc:hsqldb:file:db/db;shutdown=true")

	try {
		con.createStatement.executeUpdate(createTable)
		indexes foreach con.createStatement.executeUpdate
	} catch {
		case x: Throwable =>
			//log.error()
	}

	loadAll()

	def serialize(obj: Any) = {
		val bos = new ByteArrayOutputStream
		val out = new ObjectOutputStream(bos)

		out.writeObject(obj)

		val ba = bos.toByteArray

		bos.close
		out.close

		ba
	}

	def deserialize[T](dat: Array[Byte]) = {
		val bis = new ByteArrayInputStream(dat)
		val in = new ObjectInputStream(bis)

		val obj = in.readObject

		bis.close
		in.close

		obj.asInstanceOf[T]
	}

	def loadAll() = blocking {
		val rs = con.createStatement.executeQuery("SELECT * FROM tile_states")

		var numLoaded = 0
		while(rs.next) {
			val tileState = deserialize[TileState](rs.getBytes("state"))
			val actions = deserialize[Map[Action, VecN]](rs.getBytes("actions"))

			botThink ! LoadState(tileState, actions)

			numLoaded += 1
		}

		log.info(s"Loaded $numLoaded states from db")

		numLoaded
	}

	def receive = {
		case SaveCached if !cachedSaves.isEmpty => blocking {
			cachedSaves.values foreach {
				case save @ SaveState(state: TileState, map) =>
					val st2 = con.prepareStatement(
						"INSERT INTO tile_states VALUES(?, ?, ?, ?, ?)")

					st2.setInt(1, state.tilePos.x)
					st2.setInt(2, state.tilePos.y)
					st2.setInt(3, state.tilePos.z)
					st2.setBytes(4, serialize(state))
					st2.setBytes(5, serialize(map))

					try st2.executeUpdate() catch {
						case x: java.sql.SQLIntegrityConstraintViolationException =>
							//log.info(x.toString)

							val st = con.prepareStatement(
								"UPDATE tile_states SET actions = ? WHERE x = ? " +
										"AND y = ? AND z = ?")

							st.setBytes(1, serialize(map))
							st.setInt(2, state.tilePos.x)
							st.setInt(3, state.tilePos.y)
							st.setInt(4, state.tilePos.z)

							st.executeUpdate()
					}
				case save @ SaveState(state, map) =>
					log.warning("dont know how to save state " + state)
			}

			log.info("save states " + cachedSaves.size)

			cachedSaves = Map.empty
		}
		case save @ SaveState(state, _) =>
			cachedSaves += state -> save

	}

	override def postStop() {
		if(con != null) {
			val st = con.createStatement
			st.execute("SHUTDOWN")
			con.close()
		}

		//db.close()
		//server.stop()
		context stop self
	}
}

object StateSaveActor {
	case class SaveState(state: BotLearn.State, map: Map[Action, VecN])
	case class LoadState(state: BotLearn.State, map: Map[Action, VecN])
}