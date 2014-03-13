package edu.knowitall.srlie.confidence

import java.io.File
import scala.io.Source
import edu.knowitall.tool.conf.BreezeLogisticRegressionTrainer
import edu.knowitall.common.Resource
import edu.knowitall.srlie.SrlExtractor
import edu.knowitall.tool.parse.ClearParser
import edu.knowitall.tool.conf.Labelled
import edu.knowitall.srlie.SrlExtractionInstance
import scala.util.Random
import edu.knowitall.tool.parse.graph.DependencyGraph
import edu.knowitall.common.Analysis
import org.slf4j.LoggerFactory
import edu.knowitall.tool.stem.MorphaStemmer

object TrainSrlConfidence {
  val logger = LoggerFactory.getLogger(this.getClass)

  case class Settings (
    var inputFile: File = null,
    var outputFile: Option[File] = None,
    var evaluate: Boolean = false,
    var count: Int = Int.MaxValue
  )

  def main(args: Array[String]) {

    val parser = new scopt.OptionParser[Settings]("scoreextr") {
      arg[String]("gold") text("gold set") action { (path: String, config) => config.copy(inputFile = new File(path)) }
      opt[String]('o', "output") text("output file") action { (path: String, config: Settings) => config.copy(outputFile = Some(new File(path))) }
      opt[Unit]('e', "evaluate") text("evaluate using folds") action { (b, config) => config.copy(evaluate = true) }
      opt[Int]('c', "count") text("number of sentences to use") action { (i: Int, config) => config.copy(count = i) }
    }

    parser.parse(args, Settings()) match {
      case Some(config) => run(config)
      case None =>
    }
  }

  def run(settings: Settings) = {
    lazy val parser = new ClearParser()
    lazy val extractor = new SrlExtractor()

    logger.info("Creating trainer...")
    val trainer = new BreezeLogisticRegressionTrainer(SrlFeatureSet)
    def train(gold: Map[String, Boolean], instances: Seq[SrlExtractionInstance]) = {
      val data = instances.flatMap { inst =>
        gold.get(inst.extr.basicTripleString).map { label =>
          new Labelled(label, inst)
        }
      }

      trainer.train(data)
    }

    logger.info("Reading input...")
    val input =
      Resource.using (Source.fromFile(settings.inputFile, "UTF8")) { source =>
        source.getLines.map { line =>
          val (score, extraction, sentence) =
            line.split("\t") match {
              case Array(score, extraction, _, _, _, sentence) => (score, extraction, sentence)
              case _ => throw new MatchError("Could not deserialize line: " + line)
            }
          val annotation = score match {
            case "" => None
            case "0" | "1" => Some(if (score == "1") true else false)
          }

          (annotation, extraction, sentence)
        }.toList
      }

    logger.info("Creating gold map...")
    val gold = input.flatMap { case (annotation, extraction, sentence) =>
      annotation.map(extraction -> _)
    }.toMap

    val sentences = Random.shuffle(input.map(_._3).toSet.toSeq).take(settings.count)
    logger.info("Sentence count: " + sentences.size)
    if (settings.evaluate) {
      logger.info("Extracting sentences.")
      case class Sentence(text: String, insts: Seq[SrlExtractionInstance])
      val extracted = sentences.map { sentence =>
        val (tokens, graph) = parser(sentence)
        val insts = extractor(tokens map MorphaStemmer.lemmatizePostaggedToken, graph)
        Sentence(sentence, insts)
      }

      // use cross validation to test the annotations and feature set
      logger.info("Shuffling sentences.")

      val folds = 10
      val foldWidth = sentences.size / folds
      logger.info("Executing " + folds + " folds of size: " + foldWidth)
      val annotated = for {
        i <- 0 until folds
        _ = logger.info("Executing fold: " + i)
        test = extracted.drop(i * foldWidth).take(foldWidth)
        training = extracted.take(i * foldWidth) ++ extracted.drop((i + 1) * foldWidth)

        // make sure test and train are disjoint
        _ = require((test.map(_.text).toSet intersect training.map(_.text).toSet) == Set.empty,
            "test is not disjoint from training: " + (test.map(_.text).toSet intersect training.map(_.text).toSet))

        classifier = train(gold, training.flatMap(_.insts))

        sentence <- test
        example <-sentence.insts
        annotation <- gold.get(example.extraction.basicTripleString)
      } yield {
        (classifier(example), annotation, example)
      }

      val sorted = annotated.sortBy(-_._1)
      val points = Analysis.precisionYieldMeta(sorted.map { case (conf, annotation, example) => (conf, annotation) })
      val auc = Analysis.areaUnderCurve(points.map { case (conf, y, p) => (y, p) })

      println("AUC: " + auc)
      for (i <- 1 to 10) {
        val threshold = 1.0 - i * 0.1
        println("Y at " + threshold + ": " + points.takeWhile(_._3 > threshold).lastOption.map(_._2).getOrElse("N/A"))
      }
      points foreach { case (conf, y, p) =>
        println(Iterable(conf, y, p).mkString("\t"))
      }

      println("Misclassified:")
      sorted.filter(_._2 == false) foreach { case (conf, annotation, ex) =>
        println(("%2f" format conf) + "\t" + ex.extr + "\t" + ex.sentenceText)
      }

      /* Charting code does not work with 2.9.3!
      import scalax.chart._
      import scalax.chart.Charting._
      val pys = points.map { case (conf, y, p) => (y, p) }
      val dataset = pys.toXYSeriesCollection()
      val chart = XYLineChart(dataset, title = "Precision - Yield", domainAxisLabel = "Yield", rangeAxisLabel = "Precision")

      // save as file and read bytes
      settings.outputFile.foreach { file => chart.saveAsPNG(file, (1024, 768)) }
      */

    } else {
      // train a classifier
      val insts = sentences map parser.apply flatMap (extractor.lemmatizeAndApply _).tupled

      val classifier = train(gold, insts)
      settings.outputFile match {
        case Some(file) => classifier.saveFile(file)
        case None =>
          classifier.save(System.out)
      }
    }
  }
}
