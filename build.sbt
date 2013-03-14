name := "openie-srl"

organization := "edu.washington.cs.knowitall.openiesrl"

version := "1.0-SNAPSHOT"

scalaVersion := "2.9.2"

libraryDependencies ++= Seq("edu.washington.cs.knowitall.nlptools" %% "nlptools-srl-clear" % "2.4.0",
  "edu.washington.cs.knowitall.nlptools" %% "nlptools-conf-breeze" % "2.4.0",
  "com.github.scopt" %% "scopt" % "2.1.0",
  "org.specs2" % "specs2_2.9.2" % "1.12.3",
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
