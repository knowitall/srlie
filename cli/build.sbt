seq(Revolver.settings: _*)

name := "srlie-cli"

libraryDependencies ++= Seq(
    // server
    "com.typesafe.akka" %% "akka-actor" % "2.2.3",
    "io.spray" % "spray-can" % "1.2.0",
    "io.spray" % "spray-routing" % "1.2.0",
    // extractor
    "edu.washington.cs.knowitall.nlptools" %% "nlptools-parse-clear" % "2.4.4") ++ loggingImpls
