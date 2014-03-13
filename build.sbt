import AssemblyKeys._

assemblySettings

ReleaseSettings.defaults

name := "srlie"

organization := "org.allenai.srlie"

crossScalaVersions := Seq("2.10.3")

scalaVersion <<= crossScalaVersions { (vs: Seq[String]) => vs.head }

resolvers += "Sonatype SNAPSHOTS" at "https://oss.sonatype.org/content/repositories/snapshots/"

libraryDependencies ++= Seq("org.allenai.nlptools" %% "nlptools-srl-clear" % "2.5.0-SNAPSHOT",
  "org.allenai.nlptools" %% "nlptools-conf-breeze" % "2.5.0-SNAPSHOT",
  "org.allenai.nlptools" %% "nlptools-stem-morpha" % "2.5.0-SNAPSHOT",
  "com.github.scopt" %% "scopt" % "3.2.0",
  "org.slf4j" % "slf4j-api" % "1.7.6",
  "ch.qos.logback" % "logback-classic" % "1.0.13" % "test",
  "ch.qos.logback" % "logback-core" % "1.0.13" % "test",
  "junit" % "junit" % "4.11" % "test",
  "org.scalatest" % "scalatest_2.10" % "2.0" % "test")

mainClass in assembly := Some("edu.knowitall.srlie.SrlExtractor")

scalacOptions ++= Seq("-unchecked", "-deprecation")

licenses := Seq("Ollie Software License Agreement" -> url("https://raw.github.com/knowitall/ollie/master/LICENSE"))

homepage := Some(url("https://github.com/knowitall/openie-srl"))

publishMavenStyle := true

publishTo <<= version { (v: String) =>
  val nexus = "https://oss.sonatype.org/"
  if (v.trim.endsWith("SNAPSHOT"))
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

pomExtra := (
  <scm>
    <url>https://github.com/knowitall/openie-srl</url>
    <connection>scm:git://github.com/knowitall/openie-srl.git</connection>
    <developerConnection>scm:git:git@github.com:knowitall/openie-srl.git</developerConnection>
    <tag>HEAD</tag>
  </scm>
  <developers>
   <developer>
      <name>Michael Schmitz</name>
    </developer>
  </developers>)
