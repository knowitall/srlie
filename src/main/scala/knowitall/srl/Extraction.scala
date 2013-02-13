package knowitall.srl

import edu.washington.cs.knowitall.collection.immutable.Interval
import edu.washington.cs.knowitall.tool.parse.graph.DependencyNode
import edu.washington.cs.knowitall.tool.srl.Role
import edu.washington.cs.knowitall.tool.srl.Frame
import edu.washington.cs.knowitall.tool.parse.graph.DependencyGraph
import edu.washington.cs.knowitall.tool.srl.Roles

case class Extraction(relation: Relation, arguments: Seq[Argument]) {
  override def toString = {
    val arg1 = arguments.head
    val arg2s = arguments.filter { arg =>
      arg.interval rightOf relation.interval
    }

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
case class Argument(text: String, tokens: Seq[DependencyNode], interval: Interval, role: Role) {
  override def toString = text
}
case class Relation(text: String, sense: Sense, tokens: Seq[DependencyNode], interval: Interval) {
  override def toString = text
}
object Relation {
  val expansionLabels = Set("advmod", "neg", "aux", "cop", "auxpass", "prt", "acomp")
}

object Extraction {
  def fromFrame(dgraph: DependencyGraph)(frame: Frame) = {
    val rel = {
      val nodes = dgraph.graph.inferiors(frame.relation.node, edge => Relation.expansionLabels contains edge.label)
      val nodeSeq = nodes.toSeq.sorted
      val text = nodeSeq.iterator.map(_.text).mkString(" ")
      Relation(text, Sense(frame.relation.name, frame.relation.sense), nodeSeq, frame.relation.node.indices)
    }
    val args = frame.arguments.map { arg =>
      val nodes = dgraph.graph.inferiors(arg.node).toSeq.sorted
      val text = dgraph.text.substring(nodes.head.interval.start, nodes.last.interval.end)
      Argument(text, nodes.toSeq, Interval.span(nodes.map(_.indices)), arg.role)
    }
    
    Extraction(rel, args)
  }
}