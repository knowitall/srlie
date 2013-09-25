package edu.knowitall.srlie.nested

import edu.knowitall.srlie.SrlExtraction.Relation
import edu.knowitall.srlie.SrlExtraction.Argument
import SrlNestedExtraction._
import edu.knowitall.srlie.SrlExtraction
import edu.knowitall.tool.parse.graph.DependencyNode

case class SrlNestedExtraction(
  arg1: SrlNestedArgument,
  rel: Relation,
  arg2s: Seq[SrlNestedArgument]) {

  def tokens = {
    nestedArgumentTokens(arg1) ++ rel.tokens ++ arg2s.flatMap(nestedArgumentTokens)
  }
}

object SrlNestedExtraction {
  type SrlNestedArgument = Either[Argument, SrlNestedExtraction]
  def nestedArgumentTokens: Either[Argument, SrlNestedExtraction] => Seq[DependencyNode] = {
    case Left(arg) => arg.tokens
    case Right(extr) => extr.tokens
  }

  def from(extrs: Seq[SrlExtraction]): Seq[SrlNestedExtraction] = {
    val nested = extrs.map { extr =>
      new SrlNestedExtraction(Left(extr.arg1), extr.rel, extr.arg2s.map(Left(_)))
    }

    combine(nested)
  }

  private def combine(extrs: Seq[SrlNestedExtraction]): Seq[SrlNestedExtraction] = {
    import scalaz.Scalaz._
    import scalaz.Zipper._

    for {
      startZipper <- extrs.toStream.toZipper
      zipper <- startZipper.positions.toStream
    } {
      val extr1 = zipper.focus
      for (extr2 <- zipper.lefts ++ zipper.rights) {
        val extr2Tokens = extr2.tokens.toSet
        if (nestedArgumentTokens(extr1.arg1).toSet == extr2Tokens) {
          return combine(((zipper.lefts.toSeq filterNot (_ == extr2)) :+ extr1.copy(arg1 = Right(extr2))) ++ (zipper.rights filterNot (_ == extr2)))
        }

        for {
          startArg2Zipper <- extr1.arg2s.toStream.toZipper
          arg2Zipper <- startArg2Zipper.positions.toStream
        } {
          val arg2 = arg2Zipper.focus
          if (nestedArgumentTokens(arg2).toSet == extr2Tokens) {
            return combine(((zipper.lefts.toStream filterNot (_ == extr2)) :+ extr1.copy(arg2s = (arg2Zipper.lefts.toSeq :+ Right(extr2)) ++ arg2Zipper.rights)) ++ (zipper.rights filterNot (_ == extr2)))
          }
        }
      }
    }

    return extrs
  }
}
