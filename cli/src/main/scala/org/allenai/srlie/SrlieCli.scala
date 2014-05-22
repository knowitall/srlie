package org.allenai.srlie

import edu.knowitall.common.Resource
import edu.knowitall.common.Timing
import edu.knowitall.srlie._
import edu.knowitall.srlie.confidence.SrlConfidenceFunction
import edu.knowitall.srlie.confidence.SrlFeatureSet
import edu.knowitall.tool.parse.ClearParser
import edu.knowitall.tool.parse.DependencyParser
import edu.knowitall.tool.parse.RemoteDependencyParser
import edu.knowitall.tool.parse.graph.DependencyGraph
import edu.knowitall.tool.postag.PostaggedToken
import edu.knowitall.tool.srl.ClearSrl
import edu.knowitall.tool.srl.Frame
import edu.knowitall.tool.srl.FrameHierarchy
import edu.knowitall.tool.srl.RemoteSrl
import edu.knowitall.tool.srl.Roles
import edu.knowitall.tool.srl.Srl
import edu.knowitall.tool.stem.Lemmatized
import edu.knowitall.tool.stem.MorphaStemmer
import edu.knowitall.tool.tokenize.Tokenizer

import java.io.File
import java.io.PrintWriter
import java.net.URL
import scala.io.Source
import scala.util.control.Exception

object SrlieCli extends App {
  sealed abstract class OutputFormat
  object OutputFormat {
    def apply(format: String): OutputFormat = {
      format.toLowerCase match {
        case "standard" => Standard
        case "annotation" => Annotation
        case "evaluation" => Evaluation
        case _ => throw new IllegalArgumentException("Unknown output format: " + format)
      }
    }

    case object Standard extends OutputFormat
    case object Annotation extends OutputFormat
    case object Evaluation extends OutputFormat
  }

  case class Config(inputFile: Option[File] = None,
    outputFile: Option[File] = None,
    outputFormat: OutputFormat = OutputFormat.Standard,
    gold: Map[String, Boolean] = Map.empty,
    remoteParser: Option[URL] = None,
    remoteSrl: Option[URL] = None,
    classifierUrl: URL = SrlConfidenceFunction.defaultModelUrl) {
    def source() = {
      inputFile match {
        case Some(file) => Source.fromFile(file, "UTF8")
        case None => Source.stdin
      }
    }

    def writer() = {
      outputFile match {
        case Some(file) => new PrintWriter(file, "UTF8")
        case None => new PrintWriter(System.out)
      }
    }
  }

  val argumentParser = new scopt.OptionParser[Config]("srl-ie") {
      opt[String]('i', "input-file") action { (string, config) =>
        val file = new File(string)
        require(file.exists, "input file does not exist: " + file)
        config.copy(inputFile = Some(file))
      }
      opt[String]('o', "ouput-file") action { (string, config) =>
        val file = new File(string)
        config.copy(outputFile = Some(file))
      }
      opt[String]('g', "gold") action { (string, config) =>
        val file = new File(string)
        require(file.exists, "gold file does not exist: " + file)
        val gold = Resource.using(Source.fromFile(file, "UTF8")) { source =>
          (for {
            line <- source.getLines
            (annotation, string) = line.split("\t") match {
              case Array(annotation, string, _@ _*) => (annotation, string)
              case _ => throw new MatchError("Could not parse gold entry: " + line)
            }
            boolean = if (annotation == "1") true else false
          } yield {
            string -> boolean
          }).toMap
        }
        config.copy(gold = gold)
      }
      opt[String]('p', "remote-parser") text("URL to parser server") action { (string, config) =>
        config.copy(remoteParser = Some(new URL(string)))
      }
      opt[String]('s', "remote-srl") text("URL to srl server") action { (string, config) =>
        config.copy(remoteSrl = Some(new URL(string)))
      }
      opt[String]('c', "classifier") text("url to classifier model") action { (string, config) =>
        val file = new File(string)
        require(file.exists, "classifier file does not exist: " + file)
        config.copy(classifierUrl = file.toURI.toURL)
      }
      opt[String]('f', "format") text("output format: {standard, annotation, evaluation}") action { (string, config) =>
        config.copy(outputFormat = OutputFormat(string))
      }
  }

  argumentParser.parse(args, Config()) match {
    case Some(config) => run(config)
    case None =>
  }

  def run(config: Config) {
    import scala.concurrent.ExecutionContext.Implicits.global
    val parser = config.remoteParser match {
      case Some(url) => new RemoteDependencyParser(url.toString)
      case None => new ClearParser()
    }
    val srl = config.remoteSrl match {
      case Some(url) => new RemoteSrl(url.toString)
      case None => new ClearSrl()
    }
    val srlie = new SrlExtractor(srl)
    val conf = SrlConfidenceFunction.fromUrl(SrlFeatureSet, config.classifierUrl)

    Resource.using(config.source()) { source =>
      Resource.using(config.writer()) { writer =>
        Timing.timeThen {
          for (line <- source.getLines) {
            try {
              val (tokens, graph) = SrlExtractor.graphify(parser)(line)
              val insts = srlie.apply(tokens, graph)
              val triples = insts.flatMap(_.triplize(true))

              if (config.outputFormat == OutputFormat.Standard) {
                writer.println(DependencyGraph.singlelineStringFormat.write(graph))
                writer.println()

                val frames = srl(tokens map (_.token), graph)
                writer.println("frames:")
                frames.map(_.serialize) foreach writer.println
                writer.println()

                val hierarchy = FrameHierarchy.fromFrames(graph, frames)
                writer.println("hierarchical frames:")
                hierarchy foreach writer.println
                writer.println()

                writer.println("extractions:")
                insts.foreach { inst =>
                  val score = conf(inst)
                  writer.println(("%1.2f" format score) + ": " + inst.extr)
                }
                writer.println()

                writer.println("triples:")
                triples.map(_.extr) foreach writer.println

                val transformations = insts.flatMap(_.extr.transformations(SrlExtraction.PassiveDobj))
                if (transformations.size > 0) {
                  writer.println("transformations:")
                  transformations foreach writer.println
                }

                writer.println()
              } else if (config.outputFormat == OutputFormat.Annotation) {
                for (inst <- triples) {
                  val extr = inst.extr
                  val string = extr.basicTripleString
                  writer.println(Iterable(config.gold.get(string).map(if (_) 1 else 0).getOrElse(""), string, extr.arg1, extr.relation, extr.arg2s.mkString("; "), line).mkString("\t"))
                }
              } else if (config.outputFormat == OutputFormat.Evaluation) {
                for (inst <- triples) {
                  val extr = inst.extr
                  val string = extr.basicTripleString
                  writer.println(Iterable(config.gold.get(string).map(if (_) 1 else 0).getOrElse(""), conf(inst), string, extr.arg1, extr.relation, extr.arg2s.mkString("; "), line).mkString("\t"))
                }
              }

              writer.flush()
            } catch {
              case e: Exception => e.printStackTrace()
            }
          }
        } { ns => System.err.println("Extractions in: " + Timing.Seconds.format(ns)) }
      }
    }
  }
}