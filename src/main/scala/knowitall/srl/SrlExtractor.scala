package knowitall.srl

import scala.io.Source

import edu.washington.cs.knowitall.tool.parse.ClearParser
import edu.washington.cs.knowitall.tool.srl.ClearSrl

object SrlExtractor extends App {
  val parser = new ClearParser()
  val srl = new ClearSrl()

  for (line <- Source.stdin.getLines) {
    val graph = parser.dependencyGraph(line)
    println(graph.serialize)
    val frames = srl.apply(graph)
    frames foreach println
    
    val extrs = frames map Extraction.fromFrame(graph)
    extrs foreach println
  }
}