name := "openie-srl"

organization := "edu.washington.cs.knowitall.openiesrl"

version := "1.0-SNAPSHOT"

crossScalaVersions := Seq("2.9.2", "2.10.1")

scalaVersion <<= crossScalaVersions { (vs: Seq[String]) => vs.head }

libraryDependencies ++= Seq("edu.washington.cs.knowitall.nlptools" %% "nlptools-srl-clear" % "2.4.1-SNAPSHOT",
  "edu.washington.cs.knowitall.nlptools" %% "nlptools-conf-breeze" % "2.4.1-SNAPSHOT",
  "com.github.scopt" %% "scopt" % "2.1.0",
  "org.specs2" %% "specs2" % "1.12.3",
  "junit" % "junit" % "4.11")

resolvers += "sonatype-snapshot" at "https://oss.sonatype.org/content/repositories/snapshots/"

publishMavenStyle := true

publishTo <<= version { (v: String) =>
  val nexus = "https://oss.sonatype.org/"
  if (v.trim.endsWith("SNAPSHOT"))
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

EclipseKeys.withSource := true
