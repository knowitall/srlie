seq(Revolver.settings: _*)

name := "srlie-cli"

libraryDependencies ++= Seq(
    // extractor
    "org.allenai.nlptools" %% "nlptools-parse-clear" % "2.5.0-SNAPSHOT") ++ loggingImpls
