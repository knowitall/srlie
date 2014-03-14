package org.allenai.srlie

import edu.knowitall.srlie.confidence.SrlConfidenceFunction
import edu.knowitall.srlie.confidence.SrlFeatureSet
import edu.knowitall.srlie.SrlExtractor
import edu.knowitall.tool.parse.ClearParser
import edu.knowitall.tool.parse.graph.DependencyGraph
import edu.knowitall.tool.parse.RemoteDependencyParser
import edu.knowitall.tool.srl.ClearSrl
import edu.knowitall.tool.srl.RemoteSrl
import edu.knowitall.tool.stem.MorphaStemmer

import akka.actor._
import com.typesafe.config.ConfigFactory
import org.slf4j.LoggerFactory
import spray.http._
import spray.routing._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Try

class SrlieServer(port: Int, remoteParser: Option[String], remoteSrl: Option[String]) extends SimpleRoutingApp {
  val logger = LoggerFactory.getLogger(this.getClass)

  val parser = remoteParser match {
    case Some(url) =>
      logger.info("Using remote parser at: " + url)
      new RemoteDependencyParser(url)
    case None =>
      logger.info("Creating new ClearParser.")
      new ClearParser()
  }

  val srl = remoteSrl match {
    case Some(url) =>
      logger.info("Using remote srl at: " + url)
      new RemoteSrl(url)
    case None =>
      logger.info("Creating new ClearSrl.")
      new ClearSrl()
  }

  val srlie = new SrlExtractor(srl)
  val metric = SrlConfidenceFunction.fromUrl(SrlFeatureSet, SrlConfidenceFunction.defaultModelUrl)

  def run() {
    val cacheControlMaxAge = HttpHeaders.`Cache-Control`(CacheDirectives.`max-age`(60))

    implicit val system = ActorSystem("srlie-server")

    startServer(interface = "0.0.0.0", port = port) {
      respondWithHeader(cacheControlMaxAge) {
        path ("") {
          get {
            complete("Post to extract with srlie.")
          } ~
          post {
            entity(as[String]) { sentence =>
              srlie.synchronized {
                logger.info("Processing: " + sentence)
                val (tokens, dgraph) = parser(sentence)
                logger.debug("dgraph: " + DependencyGraph.singlelineStringFormat.write(dgraph))
                val insts = srlie(tokens map MorphaStemmer.lemmatizePostaggedToken, dgraph)
                val extrs = insts map (_.extr)
                logger.debug("extrs: " + extrs)
                complete(extrs mkString "\n")
              }
            }
          }
        } ~
        path ("info" / "name") {
          get {
            complete("srlie")
          }
        }
      }
    }
  }
}

object SrlieServerMain extends App {
  val config = ConfigFactory.load().getConfig("srlie.server")
  val port = config.getInt("port")
  val remoteParser = Try(Option(config.getString("remote.parser"))).toOption.flatten
  val remoteSrl = Try(Option(config.getString("remote.srl"))).toOption.flatten

  val server = new SrlieServer(port, remoteParser, remoteSrl)
  server.run()
}
