scalaVersion := "2.12.4"

name := "ivo-core"

organization := "ch.usi.l3"

version := "0.1"

javaOptions in Test ++= Seq("-Xss1024M")

scalacOptions ++= Seq("-deprecation")

fork in Test := true

// Kiama
libraryDependencies += "org.bitbucket.inkytonik.kiama" %% "kiama" % "2.0.0"

// ScalaTest
libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.1"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"

// Logger
libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.1.7"
libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.5.0"

// JLine
libraryDependencies += "org.jline" % "jline" % "3.3.1"

// Scalaz
libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.15"

// Parboiled
libraryDependencies += "org.parboiled" %% "parboiled-scala" % "1.1.8"
