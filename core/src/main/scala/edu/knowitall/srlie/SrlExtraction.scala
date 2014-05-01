package edu.knowitall.srlie

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
import edu.knowitall.srlie.SrlExtraction._
import scala.collection.immutable.SortedSet
import edu.knowitall.tool.tokenize.Token
import edu.knowitall.tool.tokenize.Tokenizer
import edu.knowitall.tool.stem.Lemmatized
import edu.knowitall.tool.postag.PostaggedToken
import edu.knowitall.tool.parse.graph.TokenDependencyNode

case class SrlExtraction(relation: Relation, arg1: Argument, arg2s: Seq[Argument], context: Option[Context], negated: Boolean) {
  def rel = relation

  def relationArgument = {
    if (this.active) this.arg2s.find(arg2 => arg2.role == Roles.A1 || arg2.role == Roles.A2)
    else if (this.passive) this.arg2s.find(_.role == Roles.A2)
    else None
  }

  def intervals = (arg1.interval +: relation.intervals) ++ arg2s.map(_.interval)

  def tokens = relation.tokens ++ arg1.tokens ++ arg2s.flatMap(_.tokens)

  private val bePresentVerbs = Set("am", "are", "is")
  private val bePastVerbs = Set("was", "were")
  private val beVerbs = bePastVerbs ++ bePresentVerbs
  def transformations(transformations: Transformation*): Seq[SrlExtraction] = {
    def passiveDobj = {
      for {
        a1 <- arg2s.find(_.role == Roles.A1).toSeq
        a2 <- arg2s.find(_.role == Roles.A2)

        if a1.tokens.exists(node => node.token.isNoun || node.token.isPronoun)
        if a2.tokens.exists(node => node.token.isNoun || node.token.isPronoun)

        a0 = arg1
        if a0.role == Roles.A0
        if !this.context.isDefined
      } yield {
        // val a0New = new Argument("[by] " + a0.text, a0.tokens, a0.interval, a0.role)
        val arg2s = /* a0New +: */ this.arg2s.filterNot(_ == a1)

        val inferred =
          if (this.rel.tokens.exists(node => node.string == "has" || node.string == "have")) "[been]"
          else if (arg1.plural) "[were]"
          else "[was]"

        val (before, after) = this.rel.tokens.filter { node =>
          !beVerbs.contains(node.token.string)
        }.span(node => node.postag == "MD" || node.string == "has" || node.string == "have")
        val text = Iterable(before.iterator.map(_.string), Iterable(inferred), after.iterator.map(_.string)).flatten.mkString(" ")
        val rel = new Relation(text, this.rel.sense, this.rel.tokens, this.rel.intervals)

        SrlExtraction(rel, a1, arg2s, context, negated)
      }
    }

    if (transformations.contains(PassiveDobj)) {
      passiveDobj
    } else {
      Seq.empty
    }
  }

  def triplize(includeDobj: Boolean = true): Seq[SrlExtraction] = {
    val relArg = if (includeDobj) this.relationArgument else None

    val filteredArg2s = (arg2s filterNot (arg2 => relArg.exists(_ == arg2)))
    val tripleExtrs = if (filteredArg2s.isEmpty) {
      Seq(this)
    } else {
      val extrs = relArg match {
        case Some(relArg) => arg2s.map { arg2 =>
          val rel =
            if (arg2 == relArg) {
              relation
            } else {
              val tokens = (relation.tokens ++ relArg.tokens).sortBy(_.id)
              val text = relation.text + " " + relArg.tokens.iterator.map(_.string).mkString(" ")
              relation.copy(text = text, tokens = tokens, intervals = relation.intervals :+ relArg.interval)
            }
          new SrlExtraction(rel, this.arg1, Seq(arg2), context, negated)
        }
        case None => filteredArg2s.map { arg2 =>
          new SrlExtraction(rel, this.arg1, Seq(arg2), context, negated)
        }
      }

      // don't include dobj if we create any intransitives
      if (extrs exists (_.intransitive)) this.triplize(false)
      else extrs
    }

    tripleExtrs.filter { extr =>
      // filter extractions where the rel overlaps the arg2
      extr.arg2s.forall(arg2 => extr.rel.intervals.forall(relInterval => !(arg2.interval intersects relInterval)))
    }.flatMap { extr =>
      // move preposition to relation if it leads the arg2
      extr.arg2s match {
        case Seq(arg2) if arg2.hasLeadingPreposition =>
          // since we are removing text and tokens from the arg2,
          // we might violate a requirement that the text and the
          // number of tokens is non empty.
          Exception.catching(classOf[Exception]).opt(arg2.withoutLeadingPreposition).map { newArg2 =>
            val leadingPreposition = arg2.leadingPrepositionToken.get
            val newRel = extr.rel.copy(
                text = extr.rel.text + " " + leadingPreposition.string,
                tokens = extr.rel.tokens :+ leadingPreposition,
                intervals = extr.rel.intervals :+ Interval.singleton(arg2.interval.start)
                )
            extr.copy(relation = newRel, arg2s = Seq(newArg2))
          }
        case _ => Some(extr)
      }
    // make sure the arg2 still contains tokens
    }.filter { extr =>
      extr.arg2s.forall(!_.tokens.isEmpty)
    }
  }

  def basicTripleString = {
    Iterable(arg1.text, relation.text, arg2s.iterator.map(_.text).mkString("; ")).mkString("(", "; ", ")")
  }
  override def toString = {
    val parts = Iterable(arg1.toString, relation.toString, arg2s.iterator.map(_.toString).mkString("; "))
    val prefix = context match {
      case Some(context) => context.text + ":"
      case None => ""
    }
    prefix + parts.mkString("(", "; ", ")")
  }

  def intransitive = arg2s.isEmpty

  // an extraction is active if A0 is the first A*
  def active = !intransitive && this.arg1.role == Roles.A0

  // an extraction is active if it's not passive
  def passive = !intransitive && !active
}

object SrlExtraction {
  case class Sense(name: String, id: String)

  sealed abstract class Transformation
  case object PassiveDobj extends Transformation

  def create(relation: Relation, arguments: Seq[Argument], context: Option[Context], negated: Boolean) = {
    val arg1 = arguments.find(arg => (arg.role.label matches "A\\d+") && (relation.intervals.forall(interval => arg.interval leftOf interval))).getOrElse {
      throw new IllegalArgumentException("Extraction has no arg1.")
    }

    // look for potential arguments that are right of the relation
    val rightArg2s = arguments.filter { arg =>
      relation.intervals.forall(interval => arg.interval rightOf interval)
    } ++ arguments.filter { arg =>
      !relation.intervals.forall(interval => arg.interval rightOf interval) &&
        (arg.role == Roles.AM_TMP || arg.role == Roles.AM_LOC)
    }

    val arg2s =
      // if we didn't find any, use any prime arguments that were found
      if (rightArg2s.isEmpty) arguments.filter(arg => arg != arg1 && (arg.role.label matches "A\\d+"))
      else rightArg2s

    new SrlExtraction(relation, arg1, arg2s, context, negated)
  }

  abstract class Part {
    def text: String
    def tokens: Seq[TokenDependencyNode]

    def tokenSpan: Interval

    require(!text.isEmpty, "Extraction part text may not be empty.")
    require(!tokens.isEmpty, "Extraction part tokens may not be empty.")
  }

  abstract class MultiPart extends Part {
    def intervals: Seq[Interval]
    def span = Interval.span(intervals)

    def tokenIntervals = intervals
    def tokenSpan = span

    override def hashCode = intervals.hashCode * 39 + tokens.hashCode
  }

  abstract class SinglePart extends Part {
    def interval: Interval

    def tokenInterval = interval
    def tokenSpan = tokenInterval
    def offsets = Interval.open(
      this.tokens.head.token.offsets.start,
      this.tokens.last.token.offsets.end)
  }

  class Context(val text: String, val tokens: Seq[TokenDependencyNode], val intervals: Seq[Interval]) extends MultiPart {
    override def toString = text

    def canEqual(that: Any): Boolean = that.isInstanceOf[Context]
    override def equals(that: Any): Boolean = that match {
      case that: Context => (
        that.canEqual(this)
        && this.text == that.text
        && this.tokens == that.tokens
        && this.intervals == that.intervals)
      case _ => false
    }
  }

  abstract class Argument extends SinglePart {
    require(tokens.sortBy(_.id) == tokens, "Tokens are not sorted: " + tokens.toList)

    def text: String
    def tokens: Seq[TokenDependencyNode]
    def interval: Interval
    def role: Role

    override def toString = text

    def plural = tokens.exists { node =>
      node.token.isPluralNoun
    }

    def hasLeadingPreposition: Boolean = {
      leadingPrepositionToken.isDefined
    }

    def leadingPrepositionToken: Option[TokenDependencyNode] = {
      tokens.headOption.filter { head =>
        head.token.postag == "IN" || head.token.postag == "TO"
      }
    }

    def withoutLeadingPreposition: Argument = {
      if (!leadingPrepositionToken.isDefined) {
        this
      }
      else {
        new SimpleArgument(tokens.drop(1), Interval.open(interval.start + 1, interval.end), role)
      }
    }
  }

  abstract class SemanticArgument extends Argument

  case class SimpleArgument(val text: String, val tokens: Seq[TokenDependencyNode], val interval: Interval, val role: Role) extends Argument {
    def this(nodes: Seq[TokenDependencyNode], interval: Interval, role: Role) =
      this(Tokenizer.originalText(nodes map (_.token.token)).trim, nodes, interval, role)
    override def toString = text
  }

  case class TemporalArgument(val text: String, val tokens: Seq[TokenDependencyNode], val interval: Interval, val role: Role)
    extends SemanticArgument {

    def this(nodes: Seq[TokenDependencyNode], interval: Interval, role: Role) =
      this(Tokenizer.originalText(nodes map (_.token.token)).trim, nodes, interval, role)

    override def toString = "T:" + super.toString

    override def withoutLeadingPreposition: TemporalArgument = {
      if (!leadingPrepositionToken.isDefined) {
        this
      }
      else {
        new TemporalArgument(tokens.drop(1), Interval.open(interval.start + 1, interval.end), role)
      }
    }
  }

  case class LocationArgument(val text: String, val tokens: Seq[TokenDependencyNode], val interval: Interval, val role: Role)
    extends SemanticArgument {

    def this(nodes: Seq[TokenDependencyNode], interval: Interval, role: Role) =
      this(Tokenizer.originalText(nodes map (_.token.token)).trim, nodes, interval, role)

    override def toString = "L:" + super.toString

    override def withoutLeadingPreposition: LocationArgument = {
      if (!leadingPrepositionToken.isDefined) {
        this
      }
      else {
        new LocationArgument(tokens.drop(1), Interval.open(interval.start + 1, interval.end), role)
      }
    }
  }

  case class Relation(text: String, sense: Option[Sense], tokens: Seq[TokenDependencyNode], intervals: Seq[Interval]) extends MultiPart {
    // make sure all the intervals are disjoint
    require(intervals.forall(x => !intervals.exists(y => x != y && (x intersects y))))

    override def toString = text

    def canEqual(that: Any): Boolean = that.isInstanceOf[Relation]
    override def equals(that: Any): Boolean = that match {
      case that: Relation => (
        that.canEqual(this)
        && this.text == that.text
        && this.tokens == that.tokens
        && this.intervals == that.intervals)
      case _ => false
    }

    def concat(other: Relation) = {
      Relation(this.text + " " + other.text, None, this.tokens ++ other.tokens, this.intervals ++ other.intervals)
    }
  }

  object Relation {
    val expansionLabels = Set("advmod", "neg", "aux", "cop", "auxpass", "prt", "acomp")
  }

  def contiguousAdjacent(graph: Graph[TokenDependencyNode], node: TokenDependencyNode, cond: DirectedEdge[TokenDependencyNode] => Boolean, until: Set[TokenDependencyNode]) = {
    def takeAdjacent(interval: Interval, nodes: List[TokenDependencyNode], pool: List[TokenDependencyNode]): List[TokenDependencyNode] = pool match {
      // can we add the top node?
      case head :: tail if (Interval.singleton(head.id) borders interval) && !until.contains(head) =>
        takeAdjacent(interval union Interval.singleton(head.id), head :: nodes, tail)
      // otherwise abort
      case _ => nodes
    }

    val inferiors = graph.connected(node, dedge => cond(dedge) && !(until contains dedge.end))
    val span = Interval.span(inferiors map (inf => Interval.singleton(inf.id)))
    val contiguous = graph.vertices.toList.sortBy(_.id).drop(span.start).take(span.length)

    // split into nodes left and right of node
    val lefts = contiguous.takeWhile(_ != node).reverse
    val rights = contiguous.dropWhile(_ != node).drop(1)

    // take adjacent nodes from each list
    val withLefts = takeAdjacent(Interval.singleton(node.id), List(node), lefts)
    val expanded = takeAdjacent(Interval.singleton(node.id), withLefts, rights).sortBy(_.id)

    // only take items that are inferiors
    expanded.slice(expanded.indexWhere(inferiors contains _), 1 + expanded.lastIndexWhere(inferiors contains _))
  }

  /**
   *  Find all nodes in a components next to the node.
   *  @param  node  components will be found adjacent to this node
   *  @param  labels  components may be connected by edges with any of these labels
   *  @param  without  components may not include any of these nodes
   */
  def components(graph: Graph[TokenDependencyNode], node: TokenDependencyNode, labels: Set[String], without: Set[TokenDependencyNode], nested: Boolean) = {
    // nodes across an allowed label to a subcomponent
    val across = graph.neighbors(node, (dedge: DirectedEdge[_]) => dedge.dir match {
      case Direction.Down if labels.contains(dedge.edge.label) => true
      case _ => false
    })

    across.flatMap { start =>
      // get inferiors without passing back to node
      val inferiors = graph.inferiors(start,
        (e: Graph.Edge[TokenDependencyNode]) =>
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
        val span = Interval.span(inferiors.map(inf => Interval.singleton(inf.id)).toSeq)
        Some(graph.vertices.filter(node => span.superset(Interval.singleton(node.id))).toList)
      } else None
    }
  }

  val componentEdgeLabels = Set("rcmod", "infmod", "partmod", "ref", "prepc_of", "advcl")
  val forbiddenEdgeLabel = Set("appos", "punct", "cc") ++ componentEdgeLabels
  def fromFrame(graph: Graph[TokenDependencyNode])(frame: Frame): Option[SrlExtraction] = {
    def tokenNode(node: DependencyNode) = {
      (graph.vertices find (_.id == node.id)).getOrElse {
        throw new IllegalArgumentException("Couldn't find node: " + node.id)
      }
    }

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

    val boundaries = (argsNotBoundaries.map(_.node).toSet + frame.relation.node) map tokenNode

    val args = argsNotBoundaries.filterNot { arg =>
      arg.role match {
        case _: Roles.R => true
        case _ => false
      }
    }

    implicit def tokenNodeOrdering = new Ordering[TokenDependencyNode] {
      def compare(a: TokenDependencyNode, b: TokenDependencyNode) = {
        b.id - a.id
      }
    }
    val rel = {
      // sometimes we need detached tokens: "John shouts profanities out loud."
      val nodes = graph.inferiors(tokenNode(frame.relation.node), edge => (Relation.expansionLabels contains edge.label) && !(boundaries contains edge.dest))
      val additionalNodes =
        // sometimes we need to go up a pcomp
        graph.predecessors(tokenNode(frame.relation.node), edge => edge.label == "pcomp" && nodes.exists { node => Interval.singleton(node.id) borders Interval.singleton(edge.source.id) })
      val remoteNodes = (
        // expand to certain nodes connected by a conj edge
        (graph.superiors(tokenNode(frame.relation.node), edge => edge.label == "conj") - tokenNode(frame.relation.node)) flatMap (node => graph.inferiors(node, edge => edge.label == "aux" && edge.dest.string == "to") - node)).filter(_.id < frame.relation.node.id)
      val nodeSeq = (remoteNodes ++ nodes ++ additionalNodes).toSeq.sortBy(_.id)

      /**
       * create a minimal spanning set of the supplied intervals.
       *
       * @returns  a sorted minimal spanning set
       */
      def minimal(intervals: Iterable[Interval]): List[Interval] = {
        val set = collection.immutable.SortedSet.empty[Int] ++ intervals.flatten
        set.foldLeft(List.empty[Interval]) {
          case (list, i) =>
            val singleton = Interval.singleton(i)
            list match {
              case Nil => List(singleton)
              case x :: xs if x borders singleton => (x union singleton) :: xs
              case xs => singleton :: xs
            }
        }.reverse
      }

      val intervals = minimal(nodeSeq.map(node => Interval.singleton(node.id)))
      val text = nodeSeq.iterator.map(_.string).mkString(" ")
      Relation(text, Some(Sense(frame.relation.name, frame.relation.sense)), nodeSeq, intervals)
    }

    val mappedArgs = args.map { arg =>
      val immediateNodes =
        // expand along certain contiguous nodes
        contiguousAdjacent(graph, tokenNode(arg.node), dedge => dedge.dir == Direction.Down && !(forbiddenEdgeLabel contains dedge.edge.label), boundaries)

      val componentNodes: Set[List[TokenDependencyNode]] =
        if (immediateNodes.exists(_.token.isProperNoun)) Set.empty
        else components(graph, tokenNode(arg.node), componentEdgeLabels, boundaries, false)

      val nodeSpan = Interval.span(immediateNodes.map(node => Interval.singleton(node.id)) ++ componentNodes.flatten.map(node => Interval.singleton(node.id)))
      val nodes = graph.vertices.toSeq.sortBy(_.id).slice(nodeSpan.start, nodeSpan.end)

      val nodeSeq = nodes.toSeq
      arg.role match {
        case Roles.AM_TMP => new TemporalArgument(nodeSeq, Interval.span(nodes map (node => Interval.singleton(node.id))), arg.role)
        case Roles.AM_LOC => new LocationArgument(nodeSeq, Interval.span(nodes map (node => Interval.singleton(node.id))), arg.role)
        case _ => new SimpleArgument(nodeSeq, Interval.span(nodes map (node => Interval.singleton(node.id))), arg.role)
      }
    }

    Exception.catching(classOf[IllegalArgumentException]) opt SrlExtraction.create(rel, mappedArgs, None, negated)
  }

  def fromFrameHierarchy(tokens: Seq[Lemmatized[PostaggedToken]], dgraph: DependencyGraph)(frameh: FrameHierarchy): Seq[SrlExtraction] = {
    val graph = dgraph.tokenized(tokens)
    def rec(frameh: FrameHierarchy): Seq[SrlExtraction] = {
      if (frameh.children.isEmpty) SrlExtraction.fromFrame(graph)(frameh.frame).toSeq
      else {
        SrlExtraction.fromFrame(graph)(frameh.frame) match {
          case Some(extr) =>
            val context = {
              extr.context match {
                case Some(context) =>
                  val intervals = SortedSet.empty[Interval] ++ extr.rel.intervals + extr.arg1.interval
                  val tokens = (Set.empty[TokenDependencyNode] ++ extr.arg1.tokens ++ extr.rel.tokens).toSeq.sortBy(_.id)
                  new Context(tokens.iterator.map(_.string).mkString(" "), tokens, intervals.toSeq)
                case None =>
                  val intervals = extr.rel.intervals :+ extr.arg1.interval
                  val tokens = extr.arg1.tokens ++ extr.rel.tokens
                  new Context(tokens.iterator.map(_.string).mkString(" "), tokens, intervals)
              }
            }
            val subextrs = frameh.children flatMap rec

            // triplize to include dobj in rel
            val relation = extr.relationArgument match {
              case Some(arg) if subextrs.forall(_.arg2s.forall(arg2 => !(arg2.interval intersects arg.interval))) &&
                (arg.interval borders extr.relation.span) => extr.relation.copy(tokens = (arg.tokens ++ extr.relation.tokens).sortBy(_.id), text = extr.relation.text + " " + arg.text)
              case _ => extr.relation
            }

            extr +: (subextrs flatMap { subextr =>
              Exception.catching(classOf[IllegalArgumentException]) opt
                {
                  val combinedContext = subextr.context match {
                    case Some(subcontext) =>
                      val intervals = (context.intervals ++ subcontext.intervals).toSet.toSeq.sorted
                      val tokens = (context.tokens ++ subcontext.tokens).toSet
                      val sortedToken = tokens.toSeq.sortBy(_.id)
                      new Context(sortedToken.iterator.map(_.string).mkString(" "), sortedToken, intervals)
                    case None => context
                  }
                  if (extr.arg1 == subextr.arg1)
                    // combine the relations to make a more informative relation phrase
                    SrlExtraction(relation concat subextr.relation, subextr.arg1, subextr.arg2s, Some(combinedContext), extr.negated || subextr.negated)
                  else
                    // the nested extraction has a different arg1
                    SrlExtraction(subextr.relation, subextr.arg1, subextr.arg2s, Some(combinedContext), subextr.negated)
                }
            })
          case None => Seq.empty
        }
      }
    }

    rec(frameh)
  }
}
