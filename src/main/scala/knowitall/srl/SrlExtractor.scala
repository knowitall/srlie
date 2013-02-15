package knowitall.srl

import scala.io.Source
import edu.washington.cs.knowitall.tool.parse.ClearParser
import edu.washington.cs.knowitall.tool.srl.ClearSrl
import edu.washington.cs.knowitall.tool.parse.graph.DependencyGraph
import scala.util.control.Exception
import java.io.File
import edu.washington.cs.knowitall.common.Resource

class SrlExtractor(val srl: ClearSrl = new ClearSrl()) {
  def apply(graph: DependencyGraph) = {
    val frames = srl.apply(graph)
    frames flatMap Extraction.fromFrame(graph)
  }
}

object SrlExtractor extends App {
  case class Config(inputFile: Option[File] = None) {
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
        require(file.exists, "file does not exist: " + file)
        config.copy(inputFile = Some(file))
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
        println(graph.serialize)
        val frames = srl.apply(graph)
        frames foreach println

        val extrs = frames flatMap Extraction.fromFrame(graph)
        extrs foreach println
      }
    }
  }
}
