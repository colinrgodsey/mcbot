package com.colingodsey.util

import com.typesafe.config.Config
import akka.dispatch.{ExecutorServiceFactory, ExecutorServiceConfigurator, DispatcherPrerequisites}
import java.util.concurrent.{AbstractExecutorService, ExecutorService, ThreadFactory}
import akka.event.{Logging, BusLogging}
import java.util
import javafx.application.Platform
import scala.concurrent.duration._

class JfxExecutor(config: Config,
		prerequisites: DispatcherPrerequisites)
		extends ExecutorServiceConfigurator(config, prerequisites) {
	def createExecutorServiceFactory(id: String,
			threadFactory: ThreadFactory): ExecutorServiceFactory = new ExecutorServiceFactory {
		def createExecutorService: ExecutorService = new AbstractExecutorService {
			//Logging(LogSource.fromAnyClass[this.type])
			val clz = JfxExecutor.this.getClass
			val logging = new BusLogging(prerequisites.eventStream, id, clz)

			logging.info("JfxExecutor started")

			def shutdown(): Unit = {}

			def shutdownNow(): util.List[Runnable] = new util.Vector

			def isShutdown: Boolean = Platform.isImplicitExit // ??

			def isTerminated: Boolean = Platform.isImplicitExit // ??

			def awaitTermination(timeout: Long, unit: TimeUnit): Boolean = true

			def execute(command: Runnable): Unit = Platform.runLater(new Runnable {
				lazy val run = try command.run catch { case t: Throwable =>
					logging.error(t, "Uncaught exception!")
				}
			})
		}
	}
}