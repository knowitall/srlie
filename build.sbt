name := "openie-srl"

organization := "edu.washington.cs.knowitall.openiesrl"

version := "1.0.0-SNAPSHOT"

crossScalaVersions := Seq("2.10.1")

scalaVersion <<= crossScalaVersions { (vs: Seq[String]) => vs.head }

libraryDependencies ++= Seq("edu.washington.cs.knowitall.nlptools" %% "nlptools-srl-clear" % "2.4.1",
  "edu.washington.cs.knowitall.nlptools" %% "nlptools-conf-breeze" % "2.4.1",
  "com.github.scopt" %% "scopt" % "2.1.0",
  "com.github.wookietreiber" %% "scala-chart" % "latest.integration",
  "org.slf4j" % "slf4j-api" % "1.7.2",
  "ch.qos.logback" % "logback-classic" % "1.0.9",
  "ch.qos.logback" % "logback-core" % "1.0.9",
  "org.specs2" %% "specs2" % "1.12.3" % "test",
  "junit" % "junit" % "4.11" % "test")

resolvers += "sonatype-snapshot" at "https://oss.sonatype.org/content/repositories/snapshots/"

publishMavenStyle := true

publishTo <<= version { (v: String) =>
  val nexus = "https://oss.sonatype.org/"
  if (v.trim.endsWith("SNAPSHOT"))
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}
