package edu.knowitall.srl

import edu.knowitall.collection.immutable.Interval
import scala.util.control.Exception
import edu.knowitall.tool.parse.graph.DependencyNode
import edu.knowitall.tool.srl.Role
import edu.knowitall.tool.srl.Frame
import edu.knowitall.tool.parse.graph.DependencyGraph
import edu.knowitall.tool.srl.Roles
import edu.knowitall.collection.immutable.graph.Graph.Edge
import edu.knowitall.collection.immutable.graph.DirectedEdge
import edu.knowitall.collection.immutable.graph.Direction
import edu.knowitall.tool.srl.Roles.R
import edu.knowitall.tool.srl.FrameHierarchy
import edu.knowitall.collection.immutable.graph.UpEdge
import edu.knowitall.collection.immutable.graph.Graph
import edu.knowitall.srl.SrlExtraction._

case class SrlExtraction(relation: Relation, arguments: Seq[Argument], negated: Boolean) {
  val arg1 = arguments.find(arg => (arg.role.label matches "A\\d+") && (relation.intervals.forall(interval => arg.interval leftOf interval))).getOrElse {
    throw new IllegalArgumentException("Extraction has no arg1.")
  }

  def rel = relation

  val arg2s = arguments.filter { arg =>
    relation.intervals.forall(interval => arg.interval rightOf interval)
  }

  def triplize(includeDobj: Boolean = true) = {
    val relArg =
      if (this.active && includeDobj) arg2s.find(_.role == Roles.A1)
      else if (this.passive && includeDobj) arg2s.find(_.role == Roles.A2)
      else None

    val filteredArg2s = (arg2s filterNot (arg2 => relArg.exists(_ == arg2)))
    if (filteredArg2s.isEmpty) {
      Seq(this)
    }
    else {
      filteredArg2s.map { arg2 =>
        val args = arguments filterNot (arg => arg != arg2 && arg2s.contains(arg)) filterNot (arg => relArg.exists(_ == arg))
        val rel = relArg match {
          case Some(relArg) =>
            val tokens = (relation.tokens ++ relArg.tokens).sortBy(_.interval)
            val text = tokens.iterator.map(_.text).mkString(" ")
            relation.copy(text=text, tokens=tokens, intervals=relation.intervals :+ relArg.interval)
          case None => relation
        }
        new SrlExtraction(rel, args, negated)
      }
    }
  }

  override def toString = {
    val parts = Iterable(arg1.text, relation.text, arg2s.iterator.map(_.toString).mkString("; "))
    parts.mkString("(", "; ", ")")
  }

  // an extraction is active if A0 is the first A*
  def active = {
    arguments.find(_.role.label matches "A\\d+") match {
      case Some(node) => node.role == Roles.A0
      case None => false
    }
  }

  // an extraction is active if it's not passive
  def passive = !active
}

object SrlExtraction {
  case class Sense(name: String, id: String)

  abstract class Part {
    def text: String
    def tokens: Seq[DependencyNode]
  }

  class Argument(val text: String, val tokens: Seq[DependencyNode], val interval: Interval, val role: Role) extends Part {
    override def toString = text
  }

  class TemporalArgument(text: String, tokens: Seq[DependencyNode], interval: Interval, role: Role)
    extends Argument(text, tokens, interval, role) {
    override def toString = "T:" + super.toString
  }

  class LocationArgument(text: String, tokens: Seq[DependencyNode], interval: Interval, role: Role)
    extends Argument(text, tokens, interval, role) {
    override def toString = "L:" + super.toString
  }

  case class Relation(text: String, sense: Option[Sense], tokens: Seq[DependencyNode], intervals: Seq[Interval]) extends Part {
    // make sure all the intervals are disjoint
    require(intervals.forall(x => !intervals.exists(y => x != y && (x intersects y))))

    override def toString = text

    def span = Interval.span(intervals)

    def concat(other: Relation) = {
      Relation(this.text + " " + other.text, None, this.tokens ++ other.tokens, this.intervals ++ other.intervals)
    }
  }

  object Relation {
    val expansionLabels = Set("advmod", "neg", "aux", "cop", "auxpass", "prt", "acomp")
  }

  def contiguousAdjacent(graph: DependencyGraph, node: DependencyNode, cond: DirectedEdge[DependencyNode] => Boolean, until: Set[DependencyNode]) = {
    def takeAdjacent(interval: Interval, nodes: List[DependencyNode], pool: List[DependencyNode]): List[DependencyNode] = pool match {
      // can we add the top node?
      case head :: tail if (head.indices borders interval) && !until.contains(head) =>
        takeAdjacent(interval union head.indices, head :: nodes, tail)
      // otherwise abort
      case _ => nodes
    }

    val inferiors = graph.graph.connected(node, dedge => cond(dedge) && !(until contains dedge.end))
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

  /**
   *  Find all nodes in a components next to the node.
   *  @param  node  components will be found adjacent to this node
   *  @param  labels  components may be connected by edges with any of these labels
   *  @param  without  components may not include any of these nodes
   */
  def components(graph: DependencyGraph, node: DependencyNode, labels: Set[String], without: Set[DependencyNode], nested: Boolean) = {
    // nodes across an allowed label to a subcomponent
    val across = graph.graph.neighbors(node, (dedge: DirectedEdge[_]) => dedge.dir match {
      case Direction.Down if labels.contains(dedge.edge.label) => true
      case _ => false
    })

    across.flatMap { start =>
      // get inferiors without passing back to node
      val inferiors = graph.graph.inferiors(start,
        (e: Graph.Edge[DependencyNode]) =>
          // don't cross a conjunction that goes back an across node
          !((e.label startsWith "conj") && (across contains e.dest)) &&
            // make sure we don't cycle out of the component
            e.dest != node &&
            // make sure we don't descend into another component
            // i.e. "John M. Synge who came to us with his play direct
            // from the Aran Islands , where the material for most of
            // his later works was gathered" if nested is false
            (nested || !labels.contains(e.label)))

      // make sure none of the without nodes are in the component
      if (without.forall(!inferiors.contains(_))) {
        val span = Interval.span(inferiors.map(_.indices).toSeq)
        Some(graph.nodes.filter(node => span.superset(node.indices)).toList)
      } else None
    }
  }

  val componentEdgeLabels = Set("rcmod", "infmod", "partmod", "ref", "prepc_of")
  val forbiddenEdgeLabel = Set("appos", "punct") ++ componentEdgeLabels
  def fromFrame(dgraph: DependencyGraph)(frame: Frame): Option[SrlExtraction] = {
    val argsNotBoundaries = frame.arguments.filterNot { arg =>
      arg.role match {
        case Roles.AM_MNR => true
        case Roles.AM_MOD => true
        case Roles.AM_NEG => true
        case Roles.AM_ADV => true
        case _: Roles.C => true
        case _ => false
      }
    }

    // context information
    val negated = frame.arguments.find(_.role == Roles.AM_NEG).isDefined

    val boundaries = argsNotBoundaries.map(_.node).toSet + frame.relation.node

    val args = argsNotBoundaries.filterNot { arg =>
      arg.role match {
        case _: Roles.R => true
        case _ => false
      }
    }

    val rel = {
      // sometimes we need detached tokens: "John shouts profanities out loud."
      val nodes = dgraph.graph.inferiors(frame.relation.node, edge => (Relation.expansionLabels contains edge.label) && !(boundaries contains edge.dest)) ++
        // sometimes we need to go up a pcomp
        dgraph.graph.predecessors(frame.relation.node, edge => edge.label == "pcomp")
      val remoteNodes = (
        // expand to certain nodes connected by a conj edge
        (dgraph.graph.superiors(frame.relation.node, edge => edge.label == "conj") - frame.relation.node) flatMap (node => dgraph.graph.inferiors(node, edge => edge.label == "aux" && edge.dest.text == "to") - node)).filter(_.index < frame.relation.node.index)
      val nodeSeq = (remoteNodes ++ nodes).toSeq.sorted
      val text = nodeSeq.iterator.map(_.text).mkString(" ")
      Relation(text, Some(Sense(frame.relation.name, frame.relation.sense)), nodeSeq, Seq(frame.relation.node.indices))
    }

    val mappedArgs = args.map { arg =>
      val immediateNodes =
        // expand along certain contiguous nodes
        contiguousAdjacent(dgraph, arg.node, dedge => dedge.dir == Direction.Down && !(forbiddenEdgeLabel contains dedge.edge.label), boundaries)

      val componentNodes =
        if (immediateNodes.exists(_.isProperNoun)) Set.empty
        else components(dgraph, arg.node, componentEdgeLabels, boundaries, false)

      val nodeSpan = Interval.span(immediateNodes.map(_.tokenInterval) ++ componentNodes.map(_.tokenInterval))
      val nodes = dgraph.nodes.slice(nodeSpan.start, nodeSpan.end)

      val text =
        dgraph.text.substring(nodes.head.offsets.start, nodes.last.offsets.end)
      val nodeSeq = nodes.toSeq
      arg.role match {
        case Roles.AM_TMP => new TemporalArgument(text, nodeSeq, Interval.span(nodes.map(_.indices)), arg.role)
        case Roles.AM_LOC => new LocationArgument(text, nodeSeq, Interval.span(nodes.map(_.indices)), arg.role)
        case _ => new Argument(text, nodeSeq, Interval.span(nodes.map(_.indices)), arg.role)
      }
    }

    Exception.catching(classOf[IllegalArgumentException]) opt SrlExtraction(rel, mappedArgs, negated)
  }

  def fromFrameHierarchy(dgraph: DependencyGraph)(frameh: FrameHierarchy): Seq[SrlExtraction] = {
    def rec(frameh: FrameHierarchy): Seq[SrlExtraction] = {
      if (frameh.children.isEmpty) SrlExtraction.fromFrame(dgraph)(frameh.frame).toSeq
      else {
        SrlExtraction.fromFrame(dgraph)(frameh.frame) match {
          case Some(extr) =>
            val subextrs = frameh.children flatMap rec

            extr +: (subextrs flatMap { subextr =>
              Exception.catching(classOf[IllegalArgumentException]) opt
                new SrlExtraction(extr.relation concat subextr.relation, subextr.arguments, extr.negated)
            })
          case None => Seq.empty
        }
      }
    }

    rec(frameh)
  }
}
