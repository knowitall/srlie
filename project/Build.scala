import sbt._
import Keys._

import spray.revolver.RevolverPlugin._

object SrlieBuild extends Build {
  lazy val root = Project(id = "srlie-root", base = file(".")).settings (
    publish := { },
    publishTo := Some("bogus" at "http://nowhere.com"),
    publishLocal := { }
  ).aggregate(core, cli, server)

  val buildSettings = Defaults.defaultSettings ++ ReleaseSettings.defaults ++ Revolver.settings ++
    Seq(
      organization := "org.allenai.srlie",
      crossScalaVersions := Seq("2.10.3"),
      scalaVersion <<= crossScalaVersions { (vs: Seq[String]) => vs.head },
      resolvers += "Sonatype SNAPSHOTS" at "https://oss.sonatype.org/content/repositories/snapshots/",
      licenses := Seq("Ollie Software License Agreement" -> url("https://raw.github.com/knowitall/ollie/master/LICENSE")),
      homepage := Some(url("https://github.com/knowitall/openie-srl")))

  lazy val core = Project(
    id = "core",
    base = file("core"),
    settings = buildSettings)

  lazy val cli = Project(
    id = "cli",
    base = file("cli"),
    settings = buildSettings) dependsOn(core)

  lazy val server = Project(
    id = "server",
    base = file("server"),
    settings = buildSettings) dependsOn(core)
}
