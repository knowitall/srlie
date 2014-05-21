package edu.knowitall.srlie.confidence

import edu.knowitall.tool.conf.FeatureSet
import edu.knowitall.tool.conf.Feature
import edu.knowitall.srlie.SrlExtractionInstance
import scala.collection.immutable.SortedMap
import edu.knowitall.tool.srl.FrameHierarchy
import edu.knowitall.srlie.SrlExtraction.Part
import java.util.regex.Pattern
import java.util.regex.Pattern

object SrlFeatureSet extends FeatureSet[SrlExtractionInstance, Double](SrlFeatures.featureMap)

/** Features defined for OllieExtractionInstances */
object SrlFeatures {
  type SrlFeature = Feature[SrlExtractionInstance, Double]

  implicit def boolToDouble(bool: Boolean) = if (bool) 1.0 else 0.0

  /** Turn a hierarchical frame to a list of frames. */
  def flattenFrame(frame: FrameHierarchy): Seq[FrameHierarchy] = {
    if (frame.children.isEmpty) Seq.empty
    else frame +: frame.children.flatMap(flattenFrame(_))
  }

  object hierarchicalFrames extends SrlFeature("hierarchical frames") {
    override def apply(inst: SrlExtractionInstance): Double = {
      !inst.frame.children.isEmpty
    }
  }

  object fewFrameArguments extends SrlFeature("< 2 frame arguments") {
    override def apply(inst: SrlExtractionInstance): Double = {
      flattenFrame(inst.frame).exists(_.frame.arguments.size < 2)
    }
  }

  object manyFrameArguments extends SrlFeature("> 2 frame arguments") {
    override def apply(inst: SrlExtractionInstance): Double = {
      flattenFrame(inst.frame).exists(_.frame.arguments.size > 2)
    }
  }

  object intransitiveExtraction extends SrlFeature("intransitive extraction") {
    override def apply(inst: SrlExtractionInstance): Double = {
      inst.extr.arg2s.size == 0
    }
  }

  object context extends SrlFeature("extraction has context") {
    override def apply(inst: SrlExtractionInstance): Double = {
      inst.extr.context.isDefined
    }
  }

  object activeExtraction extends SrlFeature("active extractor") {
    override def apply(inst: SrlExtractionInstance): Double = {
      inst.extr.active
    }
  }

  object naryExtraction extends SrlFeature("nary extraction") {
    override def apply(inst: SrlExtractionInstance): Double = {
      inst.extr.arg2s.size > 1
    }
  }

  object pronoun {
    def extraPronouns = Set("ive", "im", "things", "thing")
  }
  object arg1ContainsPronoun extends SrlFeature("arg1 contains a pronoun or EX") {
    override def apply(inst: SrlExtractionInstance): Double = {
      inst.extr.arg1.tokens.exists(node => node.token.isPronoun ||
        node.token.postag == "EX" ||
        pronoun.extraPronouns.contains(node.token.string.toLowerCase))
    }
  }

  object arg2ContainsPronoun extends SrlFeature("at least one arg2 contains a pronoun or EX") {
    override def apply(inst: SrlExtractionInstance): Double = {
      inst.extr.arg2s.exists(_.tokens.exists(node => node.token.isPronoun || node.token.postag == "EX" || node.string.toLowerCase == "ive"))
    }
  }

  /* the Arg1 and Arg2 versions are inconsistent here, Arg1 requires all words to be blacklisted */
  object weirdArg {
    val blacklist = Set("that", "those", "ive", "im")
  }
  object weirdArg1 extends SrlFeature("arg1 is blacklisted") {
    override def apply(inst: SrlExtractionInstance): Double = {
      inst.extr.arg1.tokens.forall(tok =>
        weirdArg.blacklist contains tok.string)
    }
  }

  object weirdArg2 extends SrlFeature("arg2 is blacklisted") {
    override def apply(inst: SrlExtractionInstance): Double = {
      inst.extr.arg2s.exists(_.tokens.exists(tok =>
        weirdArg.blacklist contains tok.string))
    }
  }

  object arg1Noun extends SrlFeature("arg1 is noun") {
    override def apply(inst: SrlExtractionInstance): Double = {
      inst.extr.arg1.tokens.exists(node => node.token.isNoun || node.token.isPronoun)
    }
  }

  object arg2Noun extends SrlFeature("at least one arg2 is noun") {
    override def apply(inst: SrlExtractionInstance): Double = {
      inst.extr.arg2s.exists(_.tokens.exists(node =>
        node.token.isNoun || node.token.isPronoun))
    }
  }

  object arg1Proper extends SrlFeature("arg1 is proper") {
    override def apply(inst: SrlExtractionInstance): Double = {
      inst.extr.arg1.tokens.exists(_.token.isProperNoun)
    }
  }

  object arg2Proper extends SrlFeature("at least one arg2 is proper") {
    override def apply(inst: SrlExtractionInstance): Double = {
      inst.extr.arg2s.exists(_.tokens.exists(_.token.isProperNoun))
    }
  }

  object arg1BeforeRel extends SrlFeature("arg1 appears before rel in sentence") {
    override def apply(inst: SrlExtractionInstance): Double = {
      inst.extr.arg1.interval leftOf inst.extr.relation.span
    }
  }

  object arg2sAfterRel extends SrlFeature("all arg2s appear after rel in sentence") {
    override def apply(inst: SrlExtractionInstance): Double = {
      inst.extr.arg2s.forall(_.interval rightOf inst.extr.relation.span)
    }
  }

  object arg1BordersRel extends SrlFeature("arg1 borders rel") {
    override def apply(inst: SrlExtractionInstance): Double = {
      inst.extr.arg1.interval borders inst.extr.relation.span
    }
  }

  object arg2BordersRel extends SrlFeature("arg2 borders rel") {
    override def apply(inst: SrlExtractionInstance): Double = {
      inst.extr.arg2s.exists(_.interval borders inst.extr.relation.span)
    }
  }

  object relContainsVerb extends SrlFeature("rel contains verb") {
    override def apply(inst: SrlExtractionInstance): Double = {
      inst.extr.relation.tokens exists (_.token.token.isVerb)
    }
  }
  
  object relIsSingleVerb extends SrlFeature("rel is single verb") {
    override def apply(inst: SrlExtractionInstance): Double = {
      (inst.extr.relation.tokens.length == 1) && inst.extr.relation.tokens.head.token.token.isVerb
    }
  }
  
  object relStartWithVerbEndsWithPrep extends SrlFeature("rel matches VW+P") {
    override def apply(inst: SrlExtractionInstance): Double = {
      ((inst.extr.relation.tokens.length > 2) && 
        inst.extr.relation.tokens.head.token.token.isVerb &&
        inst.extr.relation.tokens.last.token.token.isPreposition)
    }
  }

  object relContiguous extends SrlFeature("rel contiguous") {
    override def apply(inst: SrlExtractionInstance): Double = {
      inst.extr.relation.span.forall { i =>
        inst.extr.relation.tokens.exists(_.id == i)
      }
    }
  }

  object longRelation extends SrlFeature("rel contains > 10 tokens") {
    override def apply(inst: SrlExtractionInstance): Double = {
      inst.extr.relation.tokens.length >= 10
    }
  }

  object longArg1 extends SrlFeature("arg1 contains > 10 tokens") {
    override def apply(inst: SrlExtractionInstance): Double = {
      inst.extr.arg1.tokens.length >= 10
    }
  }

  object longArg2 extends SrlFeature("an arg2 contains > 10 tokens") {
    override def apply(inst: SrlExtractionInstance): Double = {
      inst.extr.arg2s.exists(_.tokens.length >= 10)
    }
  }

  object shortSentence extends SrlFeature("sentence contains < 10 tokens") {
    override def apply(inst: SrlExtractionInstance): Double = {
      inst.dgraph.nodes.size < 10
    }
  }

  object longSentence extends SrlFeature("sentence contains > 20 tokens") {
    override def apply(inst: SrlExtractionInstance): Double = {
      inst.dgraph.nodes.size > 20
    }
  }

  object longerSentence extends SrlFeature("sentence contains > 40 tokens") {
    override def apply(inst: SrlExtractionInstance): Double = {
      inst.dgraph.nodes.size > 40
    }
  }
    
  object prepBeforeArg1 extends SrlFeature("prep right before arg1") {
    override def apply(inst: SrlExtractionInstance): Double = {
      getTokenRightBefore(inst, inst.extr.arg1) match {
        case Some(token) => token.postag.matches("TO|IN")
        case None => 0.0
      }
    }   
  }
  
  object conjBeforeRel extends SrlFeature("conj right before rel") {
    override def apply(inst: SrlExtractionInstance): Double = {
      getTokenRightBefore(inst, inst.extr.rel) match {
        case Some(t) => t.token.token.isCoordinatingConjunction
        case None => 0.0
      }
    }
  }
  
  def getTokenRightBefore(inst: SrlExtractionInstance, part: Part) = {
    val tokens = part.tokens
    if (tokens.size == 0) None
    else inst.tokenFromId(tokens.head.id - 1)
  }
  
  def getTokenRightAfter(inst: SrlExtractionInstance, part: Part) = {
    val tokens = part.tokens
    if (tokens.size == 0) None
    else inst.tokenFromId(tokens.last.id + 1)
  }
  
  class MatchWordRightBefore(getPart: SrlExtractionInstance => Part, partName: String, word: String) extends SrlFeature(word + " right before " + partName) {
    override def apply(inst: SrlExtractionInstance): Double = {
      getTokenRightBefore(inst, getPart(inst)) match {
        case Some(t) => t.string.toLowerCase.matches(word)
        case None => 0.0
      }
    }  
  }
  
  class MatchWordAnywhereBefore(getPart: SrlExtractionInstance => Part, partName: String, word: String, wordName: String) extends SrlFeature(wordName + " anywhere before " + partName) {
    override def apply(inst: SrlExtractionInstance): Double = {
      val t = getPart(inst).tokens
      if (t.size ==0) false else{
        val matches = for (i <- 1 to (t.head.id - 1) if (inst.tokenFromId(i) match {
          case Some(t1) => t1.string.toLowerCase.matches(word)
          case _ => false
        })) yield true
        !matches.isEmpty
      }
    }  
  }
  
  class MatchWordRightAfter(partName: String, word: String) extends SrlFeature(word + " right after " + partName) {
    override def apply(inst: SrlExtractionInstance): Double = {
      val part = partName match {
        case "arg1" => Some(inst.extr.arg1)
        case "rel" => Some(inst.extr.rel)
        case "arg2s" => {
          val part2 = inst.extr.arg2s
          if (part2.size == 0) None else Some(part2.last)
        }
        case _ => None
      }
      part match {
        case Some(p) => getTokenRightAfter(inst, p) match {
          case Some(t) => t.string.toLowerCase.matches(word)
          case None => 0.0
        }
        case None => 0.0
      }
    }  
  }
  
  class PartContainsPostag(partName: String, postag: String) extends SrlFeature(partName + " contains " + postag) {
    override def apply(inst: SrlExtractionInstance): Double = {
      val tokens = partName match {
        case "arg1" => Some(inst.extr.arg1.tokens)
        case "rel" => Some(inst.extr.rel.tokens)
        case "arg2s" => Some(inst.extr.arg2s.flatMap(_.tokens))
        case _ => None
      }
      tokens match {
        case Some(t) => t.exists(_.token.token.postag.matches(postag))
        case None => 0.0
      }
    }  
  }
  
  class RelEndsWithWord(word: String) extends SrlFeature("rel ends with " + word) {
    override def apply(inst: SrlExtractionInstance): Double = {
      val part = inst.extr.rel.tokens
      (part.length > 0) && part.last.token.string.matches(word)    
    }
  }

  class BadCharacters(getPart: SrlExtractionInstance => Part, partName: String) extends SrlFeature(partName + " contains bad characters") {
    val notCapsPattern = Pattern.compile("[^A-Z]")
    val weirdChars = Pattern.compile("[^AEIOUYaeiouy0-9]")
    override def apply(inst: SrlExtractionInstance): Double = {
      // are there more than 5 caps?
      if (notCapsPattern.matcher(getPart(inst).text).replaceAll("").length() > 5)
        1.0
      // are there not enough good characters?
      else if (weirdChars.matcher(getPart(inst).text).replaceAll("").length() < 2)
        1.0
      else
        0.0
    }
  }
  
  /* the following two lists are copied from ReVerb */
  val comWords = List("acknowledge",
    "add", "address", "admit", "advertise", "advise", "agree",
    "allege", "announce", "answer", "appear", "argue", "ask", "assert",
    "assume", "assure", "believe", "boast", "claim", "comment",
    "complain", "conclude", "confirm", "consider", "contend",
    "convince", "decide", "declare", "demand", "demonstrate", "deny",
    "describe", "determine", "disclose", "discover", "discuss",
    "doubt", "emphasize", "expect", "explain", "express", "fear",
    "feel", "figure", "forget", "hear", "hope", "imply", "indicate",
    "inform", "insist", "instruct", "know", "learn", "maintain",
    "mean", "mention", "note", "notice", "observe", "pray", "predict",
    "proclaim", "promise", "propose", "repeat", "reply", "report",
    "request", "respond", "reveal", "say", "signal", "specify",
    "speculate", "state", "suggest", "teach", "tell", "testify",
    "warn", "write")
    
  val cogWords = List("estimate",
    "pretend", "prove", "realise", "realize", "recognize", "remember",
    "remind", "saw", "seem", "surmise", "suspect", "suspect",
    "theorize", "think", "understand", "verify", "wish", "worry")

  def features: Seq[SrlFeature] = Seq(
    //context,
    hierarchicalFrames,
    fewFrameArguments,
    manyFrameArguments,
    intransitiveExtraction,
    naryExtraction,
    arg1ContainsPronoun,
    arg2ContainsPronoun,
    arg1Proper,
    arg2Proper,
    arg1Noun,
    arg2Noun,
    weirdArg1,
    weirdArg2,
    arg1BeforeRel,
    arg2sAfterRel,
    arg1BordersRel,
    arg2BordersRel,
    relContainsVerb,
    relContiguous,
    longRelation,
    longArg1,
    longArg2,
    new BadCharacters(inst => inst.extr.arg1, "arg1"),
    new BadCharacters(inst => inst.extr.relation, "rel")
    
    //New features added by Oyvind (the first four simply reactivated after bug fix)
    ,
    activeExtraction,
    shortSentence,
    longSentence,
    longerSentence,
    prepBeforeArg1,
    conjBeforeRel,
    relIsSingleVerb,
    relStartWithVerbEndsWithPrep,
    /* the following classes could be made simpler and more consistent if need be */
    new MatchWordRightBefore(inst => inst.extr.arg1, "arg1", "if"),
    new MatchWordRightBefore(inst => inst.extr.arg1, "arg1", "in"),
    new MatchWordRightBefore(inst => inst.extr.arg1, "arg1", "that|which|who"),
    new MatchWordRightBefore(inst => inst.extr.arg1, "arg1", ","),
    new MatchWordAnywhereBefore(inst => inst.extr.arg1, "arg1", "if|whether|though|although", "if|whether|though|although"),
    new MatchWordAnywhereBefore(inst => inst.extr.arg1, "arg1", "may|might|would|could|should|suppose", "may|might|would|could|should|suppose"),
    new MatchWordAnywhereBefore(inst => inst.extr.arg1, "arg1", comWords.mkString("|"), "communic words"),
    new MatchWordAnywhereBefore(inst => inst.extr.arg1, "arg1", cogWords.mkString("|"), "cognitive words"),
    new MatchWordRightBefore(inst => inst.extr.rel, "rel", "that"),
    new MatchWordRightBefore(inst => inst.extr.rel, "rel", "who|which"),
    new MatchWordRightAfter("arg2s", "to"),
    new MatchWordRightAfter("arg2s", "that"),
    new PartContainsPostag("rel", "VBZ"),
    new PartContainsPostag("rel", "VBG"),
    new PartContainsPostag("rel", "VBD"),
    new PartContainsPostag("rel", "VBN"),
    new PartContainsPostag("rel", "VBP"),
    new PartContainsPostag("rel", "VB"),
    new RelEndsWithWord("to"),
    new RelEndsWithWord("in"),
    new RelEndsWithWord("for"),
    new RelEndsWithWord("of"),
    new RelEndsWithWord("on")
    )
    
    

  def featureMap: SortedMap[String, SrlFeature] = {
    (for (f <- features) yield (f.name -> Feature.from(f.name, f.apply _)))(scala.collection.breakOut)
  }
}
