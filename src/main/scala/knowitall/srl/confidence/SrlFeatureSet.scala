package knowitall.srl.confidence

import edu.knowitall.tool.conf.FeatureSet
import edu.knowitall.tool.conf.Feature
import knowitall.srl.SrlExtractionInstance
import scala.collection.immutable.SortedMap
import edu.knowitall.tool.srl.FrameHierarchy
import knowitall.srl.SrlExtraction.Part
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

  object naryExtraction extends SrlFeature("nary extraction") {
    override def apply(inst: SrlExtractionInstance): Double = {
      inst.extr.arg2s.size > 1
    }
  }

  object arg1ContainsPronoun extends SrlFeature("arg1 contains a pronoun") {
    override def apply(inst: SrlExtractionInstance): Double = {
      inst.extr.arg1.tokens.exists(_.isPronoun)
    }
  }

  object arg2ContainsPronoun extends SrlFeature("at least one arg2 contains a pronoun") {
    override def apply(inst: SrlExtractionInstance): Double = {
      inst.extr.arg2s.exists(_.tokens.exists(_.isPronoun))
    }
  }

  object arg1Proper extends SrlFeature("arg1 is proper") {
    override def apply(inst: SrlExtractionInstance): Double = {
      inst.extr.arg1.tokens.exists(_.isProperNoun)
    }
  }

  object arg2Proper extends SrlFeature("at least one arg2 is proper") {
    override def apply(inst: SrlExtractionInstance): Double = {
      inst.extr.arg2s.exists(_.tokens.exists(_.isProperNoun))
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

  object relContainsVerb extends SrlFeature("rel contains verb") {
    override def apply(inst: SrlExtractionInstance): Double = {
      inst.extr.relation.tokens exists (_.isVerb)
    }
  }

  object longRelation extends SrlFeature("rel contains > 10 tokens") {
    override def apply(inst: SrlExtractionInstance): Double = {
      inst.extr.relation.tokens.length >= 10
    }
  }

  object longArg1 extends SrlFeature("arg1 contains > 10 tokens") {
    override def apply(inst: SrlExtractionInstance): Double = {
      inst.extr.arg1.tokens.length
    }
  }

  object longArg2 extends SrlFeature("an arg2 contains > 10 tokens") {
    override def apply(inst: SrlExtractionInstance): Double = {
      inst.extr.arg2s.exists(_.tokens.length >= 10)
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

  def features: Seq[SrlFeature] = Seq(
    hierarchicalFrames,
    fewFrameArguments,
    manyFrameArguments,
    intransitiveExtraction,
    naryExtraction,
    arg1ContainsPronoun,
    arg2ContainsPronoun,
    arg1Proper,
    arg2Proper,
    arg1BeforeRel,
    arg2sAfterRel,
    relContainsVerb,
    longRelation,
    longArg1,
    longArg2,
    new BadCharacters(inst => inst.extr.arg1, "arg1"),
    new BadCharacters(inst => inst.extr.relation, "rel"))

  def featureMap: SortedMap[String, SrlFeature] = {
    (for (f <- features) yield (f.name -> Feature.from(f.name, f.apply _)))(scala.collection.breakOut)
  }
}