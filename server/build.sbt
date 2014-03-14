seq(Revolver.settings: _*)

name := "srlie-server"

libraryDependencies ++= Seq(
    // server
    "com.typesafe.akka" %% "akka-actor" % "2.2.3",
    "io.spray" % "spray-can" % "1.2.0",
    "io.spray" % "spray-routing" % "1.2.0",
    // extractor
    "org.allenai.nlptools" %% "nlptools-parse-clear" % "2.5.0-SNAPSHOT") ++ loggingImpls
