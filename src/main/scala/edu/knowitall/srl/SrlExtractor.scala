package edu.knowitall.srl

import scala.io.Source
import edu.knowitall.tool.parse.ClearParser
import edu.knowitall.tool.srl.ClearSrl
import edu.knowitall.tool.srl.Srl
import edu.knowitall.tool.parse.graph.DependencyGraph
import scala.util.control.Exception
import java.io.File
import edu.knowitall.common.Resource
import edu.knowitall.tool.srl.FrameHierarchy
import edu.knowitall.tool.srl.Frame
import edu.knowitall.tool.srl.Roles

class SrlExtractor(val srl: Srl = new ClearSrl()) {
  def apply(dgraph: DependencyGraph): Seq[SrlExtractionInstance] = {
    val frames = srl.apply(dgraph)
    this.extract(dgraph)(frames)
  }

  def extract(dgraph: DependencyGraph)(frames: Seq[Frame]) = {
    val hierarchy = FrameHierarchy.fromFrames(dgraph, frames).toSeq
    hierarchy.flatMap { hierarchy =>
      val extrs = SrlExtraction.fromFrameHierarchy(dgraph)(hierarchy)
      extrs.map { extr => SrlExtractionInstance(extr, hierarchy, dgraph) }
    }
  }
}

object SrlExtractor extends App {
  sealed abstract class OutputFormat
  object OutputFormat {
    def apply(format: String): OutputFormat = {
      format match {
        case "standard" => Standard
        case "annotation" => Annotation
        case _ => throw new IllegalArgumentException("Unknown output format: " + format)
      }
    }

    case object Standard extends OutputFormat
    case object Annotation extends OutputFormat
  }

  case class Config(inputFile: Option[File] = None, outputFormat: OutputFormat = OutputFormat.Standard, gold: Map[String, Boolean] = Map.empty) {
    def source() = {
      inputFile match {
        case Some(file) => Source.fromFile(file)
        case None => Source.stdin
      }
    }
  }

  val argumentParser = new scopt.immutable.OptionParser[Config]("srl-ie") {
    def options = Seq(
      argOpt("input file", "input file") { (string, config) =>
        val file = new File(string)
        require(file.exists, "input file does not exist: " + file)
        config.copy(inputFile = Some(file))
      },
      opt("gold", "gold file") { (string, config) =>
        val file = new File(string)
        require(file.exists, "gold file does not exist: " + file)
        val gold = Resource.using (Source.fromFile(file)) { source =>
          (for {
            line <- source.getLines
            Array(annotation, string, _ @ _*) = line.split("\t")
            boolean = if (annotation == "1") true else false
          } yield {
            string -> boolean
          }).toMap
        }
        config.copy(gold = gold)
      },
      opt("format", "output format: {standard, annotation}") { (string, config) =>
        config.copy(outputFormat = OutputFormat(string))
      })
  }

  argumentParser.parse(args, Config()) match {
    case Some(config) => run(config)
    case None =>
  }

  def run(config: Config) {
    lazy val parser = new ClearParser()
    val srl = new ClearSrl()

    def graphify(line: String) = {
      (Exception.catching(classOf[DependencyGraph.SerializationException]) opt DependencyGraph.deserialize(line)) match {
        case Some(graph) => graph
        case None => parser.dependencyGraph(line)
      }
    }

    Resource.using(config.source()) { source =>
      for (line <- source.getLines) {
        val graph = graphify(line)
        val frames = srl.apply(graph)
        val hierarchy = FrameHierarchy.fromFrames(graph, frames.toIndexedSeq)
        val extrs = hierarchy flatMap SrlExtraction.fromFrameHierarchy(graph)
        val triples = extrs flatMap (_.triplize(true))

        if (config.outputFormat == OutputFormat.Standard) {
          println(graph.serialize)
          println()

          println("frames:")
          frames.map(_.serialize) foreach println
          println()

          println("hierarchy:")
          hierarchy foreach println
          println()

          println("extractions:")
          extrs foreach println
          println()

          println("transformations:")
          extrs.flatMap(_.transformations(SrlExtraction.PassiveDobj)) foreach println
          println()

          println("triples:")
          triples foreach println
        }
        else if (config.outputFormat == OutputFormat.Annotation) {
          for (extr <- triples) {
            println(Iterable(config.gold.get(extr.toString).map(if (_) 1 else 0).getOrElse(""), extr.toString, extr.arg1, extr.relation, extr.arg2s.mkString("; "), line).mkString("\t"))
          }
        }
      }
    }
  }
}
