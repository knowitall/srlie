package knowitall.srl

import edu.washington.cs.knowitall.collection.immutable.Interval
import scala.util.control.Exception
import edu.washington.cs.knowitall.tool.parse.graph.DependencyNode
import edu.washington.cs.knowitall.tool.srl.Role
import edu.washington.cs.knowitall.tool.srl.Frame
import edu.washington.cs.knowitall.tool.parse.graph.DependencyGraph
import edu.washington.cs.knowitall.tool.srl.Roles
import edu.washington.cs.knowitall.collection.immutable.graph.Graph.Edge
import edu.washington.cs.knowitall.collection.immutable.graph.DirectedEdge
import edu.washington.cs.knowitall.collection.immutable.graph.Direction

case class Extraction(relation: Relation, arguments: Seq[Argument]) {
  val arg1 = arguments.find(arg => (arg.role.label matches "A\\d+") && (arg.interval leftOf relation.interval)).getOrElse {
    throw new IllegalArgumentException("Extraction has no arg1.")
  }

  val arg2s = arguments.filter { arg =>
    arg.interval rightOf relation.interval
  }

  require(!arg2s.isEmpty, "No arg2s")

  override def toString = {

    val parts = Iterable(arg1.text, relation.text, arg2s.iterator.map(_.text).mkString("; "))
    parts.mkString("(", "; ", ")")
  }

  // an extraction is active if A0 is the first A*
  def active = {
    arguments.find(_.role.label startsWith "A") match {
      case Some(node) => node.role == Roles.A0
      case None => false
    }
  }

  // an extraction is active if it's not passive
  def passive = !active
}
case class Sense(name: String, id: Int)
abstract class ExtractionPart {
  def text: String
  def tokens: Seq[DependencyNode]
  def interval: Interval
}
case class Argument(override val text: String, override val tokens: Seq[DependencyNode], override val interval: Interval, role: Role) extends ExtractionPart {
  override def toString = text
}
case class Relation(override val text: String, sense: Sense, override val tokens: Seq[DependencyNode], override val interval: Interval) extends ExtractionPart {
  override def toString = text
}
object Relation {
  val expansionLabels = Set("advmod", "neg", "aux", "cop", "auxpass", "prt", "acomp")
}

object Extraction {
  def contiguousAdjacent(graph: DependencyGraph, node: DependencyNode, cond: DirectedEdge[DependencyNode]=>Boolean, until: Set[DependencyNode]) = {
    def takeAdjacent(interval: Interval, nodes: List[DependencyNode], pool: List[DependencyNode]): List[DependencyNode] = pool match {
      // can we add the top node?
      case head :: tail if (head.indices borders interval) && !until.contains(head) =>
        takeAdjacent(interval union head.indices, head :: nodes, tail)
      // otherwise abort
      case _ => nodes
    }

    val inferiors = graph.graph.connected(node, cond)
    val span = Interval.span(inferiors.map(_.indices))
    val contiguous = graph.nodes.drop(span.start).take(span.length).toList.sorted

    // split into nodes left and right of node
    val lefts = contiguous.takeWhile(_ != node).reverse
    val rights = contiguous.dropWhile(_ != node).drop(1)

    // take adjacent nodes from each list
    val withLefts = takeAdjacent(node.indices, List(node), lefts)
    val expanded = takeAdjacent(node.indices, withLefts, rights)

    expanded
  }

  def fromFrame(dgraph: DependencyGraph)(frame: Frame): Option[Extraction] = {
    val rel = {
      val nodes = dgraph.graph.inferiors(frame.relation.node, edge => Relation.expansionLabels contains edge.label)
      val nodeSeq = nodes.toSeq.sorted
      val text = nodeSeq.iterator.map(_.text).mkString(" ")
      Relation(text, Sense(frame.relation.name, frame.relation.sense), nodeSeq, frame.relation.node.indices)
    }

    val boundaries = frame.arguments.map(_.node).toSet
    println(frame)
    println(boundaries)
    val args = frame.arguments.map { arg =>
      val nodes = contiguousAdjacent(dgraph, arg.node, dedge => dedge.dir == Direction.Down, boundaries).sorted
      val text = dgraph.text.substring(nodes.head.interval.start, nodes.last.interval.end)
      Argument(text, nodes.toSeq, Interval.span(nodes.map(_.indices)), arg.role)
    }

    Exception.catching(classOf[IllegalArgumentException]) opt Extraction(rel, args)
  }
}
