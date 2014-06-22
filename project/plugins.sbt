//scalaVersion := "2.9.2"

//resolvers += Resolver.url("scalasbt releases", new URL("http://scalasbt.artifactoryonline.com/scalasbt/sbt-plugin-releases"))(Resolver.ivyStylePatterns)

resolvers += "spray repo" at "http://repo.spray.io/"

addSbtPlugin("io.spray" % "sbt-revolver" % "0.7.1")

//addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.8.5")

addSbtPlugin("com.typesafe.sbt" % "sbt-atmos" % "0.3.2")