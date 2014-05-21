package edu.knowitall.srlie

import edu.knowitall.tool.srl.FrameHierarchy
import edu.knowitall.tool.stem.Lemmatized
import edu.knowitall.tool.parse.graph.DependencyGraph
import edu.knowitall.tool.parse.graph.TokenDependencyNode
import edu.knowitall.tool.postag.PostaggedToken

case class SrlExtractionInstance(extr: SrlExtraction, sentenceText: String, frame: FrameHierarchy,
  dgraph: DependencyGraph, tokens: Seq[Lemmatized[PostaggedToken]]) {
  def extraction = extr

  override def toString = extr.toString + " <- " + frame.toString

  def triplize(includeDobj: Boolean = true): Seq[SrlExtractionInstance] = {
    extr.triplize(includeDobj).map(extr => this.copy(extr = extr))
  }

  lazy val tokenIdMap: Map[Int, TokenDependencyNode] = dgraph.tokenized(tokens).vertices.map(t => (t.id, t)).toMap
  def tokenFromId(tokenId: Int): Option[TokenDependencyNode] = tokenIdMap.get(tokenId)

}
