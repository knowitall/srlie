package edu.knowitall.srl

import edu.knowitall.tool.srl.FrameHierarchy
import edu.knowitall.tool.parse.graph.DependencyGraph

case class SrlExtractionInstance(extr: SrlExtraction, frame: FrameHierarchy, dgraph: DependencyGraph) {
  def extraction = extr

  override def toString = extr.toString + " <- " + frame.toString

  def triplize(includeDobj: Boolean = true) = {
    extr.triplize(includeDobj).map(extr => this.copy(extr = extr))
  }
}
