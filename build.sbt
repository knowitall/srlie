name := "openie-srl"

version := "1.0-SNAPSHOT"

scalaVersion := "2.9.2"

libraryDependencies += "edu.washington.cs.knowitall.nlptools" %% "nlptools-srl-clear" % "2.3.1-SNAPSHOT"

resolvers += "sonatype-snapshot" at "https://oss.sonatype.org/content/repositories/snapshots/"

EclipseKeys.withSource := true
