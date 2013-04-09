package edu.knowitall.srl.confidence

import java.io.File
import scala.io.Source
import scopt.mutable.OptionParser
import edu.knowitall.tool.conf.BreezeLogisticRegressionTrainer
import edu.knowitall.common.Resource
import edu.knowitall.srl.SrlExtractor
import edu.knowitall.tool.parse.ClearParser
import edu.knowitall.tool.conf.Labelled

object TrainSrlConfidence {
  def main(args: Array[String]) {
    object settings extends Settings {
      var inputFile: File = _
      var outputFile: Option[File] = None
    }

    val parser = new OptionParser("scoreextr") {
      arg("gold", "gold set", { path: String => settings.inputFile = new File(path) })
      argOpt("output", "output file", { path: String => settings.outputFile = Some(new File(path)) })
    }

    if (parser.parse(args)) {
      run(settings)
    }
  }

   abstract class Settings {
     def inputFile: File
     def outputFile: Option[File]
   }

  def run(settings: Settings) = {
    val extractor = new SrlExtractor()
    val parser = new ClearParser()

    val trainer = new BreezeLogisticRegressionTrainer(SrlFeatureSet)

    val input =
      Resource.using (Source.fromFile(settings.inputFile)) { source =>
        source.getLines.map { line =>
          val (score, extraction, sentence) =
            try {
              line.split("\t") match { case Array(score, extraction, _, _, _, sentence) =>
                (score, extraction, sentence)
              }
            }
            catch {
              case e: MatchError => throw new MatchError("Could not match line: " + line)
            }
          val annotation = score match {
            case "" => None
            case "0" | "1" => Some(if (score == "1") true else false)
          }

          (annotation, extraction, sentence)
        }.toList
      }

    val gold = input.flatMap { case (annotation, extraction, sentence) =>
      annotation.map(extraction -> _)
    }.toMap

    val sentences = input.map(_._3)
    val inst = sentences map parser.apply flatMap extractor.apply
    val data = inst.flatMap { inst =>
      gold.get(inst.extr.basicTripleString).map { label =>
        new Labelled(label, inst)
      }
    }

    val classifier = trainer.train(data)
    settings.outputFile match {
      case Some(file) => classifier.saveFile(file)
      case None =>
        classifier.save(System.out)
    }
  }
}
