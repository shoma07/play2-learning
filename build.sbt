name := """play2-learning"""
organization := "com.example"

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.13.1"

libraryDependencies += guice
libraryDependencies += "org.scalatestplus.play" %% "scalatestplus-play" % "5.0.0" % Test
libraryDependencies ++= Seq(
  "com.h2database" % "h2" % "1.4.200",
  "org.scalikejdbc" %% "scalikejdbc" % "3.4.0",
  "org.scalikejdbc" %% "scalikejdbc-config" % "3.4.0",
  "org.scalikejdbc" %% "scalikejdbc-play-initializer" % "2.8.0-scalikejdbc-3.4"
)

enablePlugins(ScalikejdbcPlugin)

// Adds additional packages into Twirl
//TwirlKeys.templateImports += "com.example.controllers._"

// Adds additional packages into conf/routes
// play.sbt.routes.RoutesKeys.routesImport += "com.example.binders._"
