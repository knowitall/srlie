package edu.knowitall.srlie

import edu.knowitall.tool.parse.graph.DependencyGraph
import edu.knowitall.tool.parse.DependencyParser
import edu.knowitall.tool.postag.PostaggedToken
import edu.knowitall.tool.srl.ClearSrl
import edu.knowitall.tool.srl.Frame
import edu.knowitall.tool.srl.FrameHierarchy
import edu.knowitall.tool.srl.Srl
import edu.knowitall.tool.stem.Lemmatized
import edu.knowitall.tool.stem.MorphaStemmer
import edu.knowitall.tool.tokenize.Tokenizer

import java.io.File
import java.io.PrintWriter
import java.net.URL
import scala.io.Source
import scala.util.control.Exception

class SrlExtractor(val srl: Srl = new ClearSrl()) {
  def lemmatizeAndApply(tokens: Seq[PostaggedToken], dgraph: DependencyGraph): Seq[SrlExtractionInstance] = {
    this.apply(tokens map MorphaStemmer.lemmatizePostaggedToken, dgraph)
  }

  def apply(tokens: Seq[Lemmatized[PostaggedToken]], dgraph: DependencyGraph): Seq[SrlExtractionInstance] = {
    val frames = srl.apply(tokens map (_.token), dgraph)
    this.extract(tokens, dgraph)(frames)
  }

  def extract(tokens: Seq[Lemmatized[PostaggedToken]], dgraph: DependencyGraph)(frames: Seq[Frame]) = {
    val hierarchy = FrameHierarchy.fromFrames(dgraph, frames).toSeq
    hierarchy.flatMap { hierarchy =>
      val extrs = SrlExtraction.fromFrameHierarchy(tokens, dgraph)(hierarchy)
      extrs.map { extr => SrlExtractionInstance(extr, Tokenizer.originalText(tokens map (_.token)), hierarchy, dgraph, tokens) }
    }
  }
}

object SrlExtractor {
  def graphify(parser: DependencyParser)(line: String): (Seq[Lemmatized[PostaggedToken]], DependencyGraph) = {
    val (tokens, graph) = parser.dependencyGraph(line)
    (tokens map MorphaStemmer.lemmatizePostaggedToken, graph)
  }
}
