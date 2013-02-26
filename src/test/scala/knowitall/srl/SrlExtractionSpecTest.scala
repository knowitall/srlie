package knowitall.srl

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import edu.washington.cs.knowitall.tool.parse.ClearParser
import edu.washington.cs.knowitall.tool.srl.Frame
import edu.washington.cs.knowitall.tool.parse.graph.DependencyGraph

@RunWith(classOf[JUnitRunner])
class SrlExtractionSpecTest extends Specification {
  val srl = new SrlExtractor()
  val parser = new ClearParser()

  "John wants to fly to Boston this Thursday for a much needed vacation." in {
    val sentence = "John wants to fly to Boston this Thursday for a much needed vacation."
    val extrs = srl.synchronized {
      val graph = parser.dependencyGraph(sentence)
      srl(graph)
    }

    val target = extrs.find(_.arg1.text == "John")
    target must beSome

    target.get.active must beTrue
  }

  "Obama was criticized for his tax plan." in {
    val sentence = "Obama was criticized for his tax plan."
    val extrs = srl.synchronized {
      val graph = parser.dependencyGraph(sentence)
      srl(graph)
    }

    val target = extrs.find(_.arg1.text == "Obama")
    target must beSome

    target.get.active must beFalse
  }

  {
    val sentence = "Obama was criticized by Romney for his tax plan."
      sentence in {
      val extrs = srl.synchronized {
        val graph = parser.dependencyGraph(sentence)
        srl(graph)
      }

      val target = extrs.find(_.arg1.text == "Obama")
      target must beSome

      target.get.active must beFalse
    }
  }

  {
    val sentence = "Clay holds water well , sometimes perhaps too well"
    ("extractions from: " + sentence) in {
      val dgraph = DependencyGraph.deserialize("nsubj(holds_VBZ_1_5, Clay_NNP_0_0); dobj(holds_VBZ_1_5, water_NN_2_11); advmod(holds_VBZ_1_5, well_RB_3_17); punct(holds_VBZ_1_5, ,_,_4_22); advmod(holds_VBZ_1_5, sometimes_RB_5_24); advmod(sometimes_RB_5_24, well_RB_8_46); advmod(well_RB_8_46, perhaps_RB_6_34); advmod(well_RB_8_46, too_RB_7_42)")
      val frames = Seq(Frame.deserialize(dgraph)("hold_1.01:[A0=Clay_0, A1=water_2, AM-MNR=well_3, AM-TMP=sometimes_5]"))
      val extrs = srl.synchronized {
        srl.extract(dgraph)(frames)
      }

      extrs.size must_== 1
      extrs.head.toString must_== "(Clay; holds well; water; sometimes perhaps too well)"
    }
  }
}