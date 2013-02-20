name := "openie-srl"

organization := "edu.knowitall"

version := "1.0-SNAPSHOT"

scalaVersion := "2.9.2"

libraryDependencies ++= Seq("edu.washington.cs.knowitall.nlptools" %% "nlptools-srl-clear" % "2.3.1-SNAPSHOT",
  "com.github.scopt" % "scopt_2.9.1" % "2.1.0",
  "org.specs2" % "specs2_2.9.2" % "1.12.3",
  "junit" % "junit" % "4.11")

resolvers += "sonatype-snapshot" at "https://oss.sonatype.org/content/repositories/snapshots/"

publishTo := Some(Resolver.file("file",  new File(Path.userHome + "/.m2/repository")) )

EclipseKeys.withSource := true
