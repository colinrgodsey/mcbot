import sbt._

import Keys._

//import sbtassembly.Plugin.AssemblyKeys._
//import sbtassembly.Plugin.assemblySettings

import spray.revolver.RevolverPlugin.Revolver

import com.typesafe.sbt.SbtAtmos.{ Atmos, atmosSettings }

object General {
	val settings = Defaults.defaultSettings ++ Seq (
		name := "MCB",
		version := "0.2",
		scalaVersion := scalaV,
		organization  := "com.colingodsey",
		javacOptions ++= Seq("-encoding", "UTF-8", "-source", "1.6", "-target", "1.6"),
		//javaOptions += " -javaagent:lib/aspectj-1.7.4.jar ",

		libraryDependencies ++= Seq(
			"io.spray"              %   "spray-can"                 % sprayV,
			"io.spray"              %   "spray-client"              % sprayV,
			"io.spray"              %   "spray-routing"             % sprayV,
			"io.spray"              %   "spray-testkit"             % sprayV,

			"io.spray"              %%  "spray-json"                % "1.2.3",

			"com.typesafe.akka"     %%  "akka-actor"                % akkaV,
			"com.typesafe.akka"     %%  "akka-remote"               % akkaV,
			"com.typesafe.akka"     %%  "akka-testkit"              % akkaV,

			"com.couchbase.client"  %   "couchbase-client"          % "1.2.3",

			"org.bouncycastle"      %   "bcprov-jdk15on"            % "1.50",
			"org.bouncycastle"      %   "bcpkix-jdk15on"            % "1.50",

			"org.scalafx"           %   "scalafx_2.10"              % "8.0.0-M3",

			"net.sourceforge.jsi"   %   "jsi"                       % "1.0.0",

			//"com.typesafe.atmos"    %   "trace-akka-2.2.1_2.10"     % "1.3.0",

			"org.scalatest" 		%%  "scalatest"                 % "1.9"  % "test",
			"junit" 				%   "junit"					    % "4.10" % "test"
		),

		resolvers ++= Seq(
			"spray repo" at "http://repo.spray.io/",
			"jsi repo" at "http://sourceforge.net/projects/jsi/files/m2_repo"
		)
	) /*++ assemblySettings*/ ++ Revolver.settings ++ atmosSettings

	val akkaV = "2.2.3"
	val sprayV = "1.2.0"
	val scalaV = "2.10.3"
}

object MCBot extends Build {
	lazy val main = Project (
		"mcbot",
		file("."),
		settings = General.settings
	).configs(Atmos)
}
