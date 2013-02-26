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

  {
    val sentence = "John wants to fly to Boston this Thursday for a much needed vacation."
    sentence in {
      val extrs = srl.synchronized {
        val graph = parser.dependencyGraph(sentence)
        srl(graph)
      }

      val target = extrs.find(_.arg1.text == "John")
      target must beSome

      target.get.active must beTrue
    }
  }

  {
    val sentence = "Obama was criticized for his tax plan."
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
    val sentence = "Alcohol is a drug, a sedative, which depresses the central nervous system"
    ("extractions from: " + sentence) in {
      val dgraph = DependencyGraph.deserialize("nsubj(is_VBZ_1_8, Alcohol_NN_0_0); attr(is_VBZ_1_8, drug_NN_3_13); det(drug_NN_3_13, a_DT_2_11); punct(drug_NN_3_13, ,_,_4_17); appos(drug_NN_3_13, sedative_NN_6_21); det(sedative_NN_6_21, a_DT_5_19); punct(sedative_NN_6_21, ,_,_7_29); rcmod(sedative_NN_6_21, depresses_VBZ_9_37); nsubj(depresses_VBZ_9_37, which_WDT_8_31); dobj(depresses_VBZ_9_37, system_NN_13_67); det(system_NN_13_67, the_DT_10_47); amod(system_NN_13_67, central_JJ_11_51); amod(system_NN_13_67, nervous_JJ_12_59)")
      val frames = Seq("be_1.01:[A1=Alcohol_0, A2=drug_3]", "depress_9.02:[A0=sedative_6, R=which_8, A1=system_13]") map Frame.deserialize(dgraph)

      val extrs = srl.synchronized {
        srl.extract(dgraph)(frames)
      }

      extrs.size must_== 2
      extrs.map(_.toString) must haveTheSameElementsAs(Seq("(Alcohol; is; a drug,)", "(a sedative,; depresses; the central nervous system)"))
    }
  }

  {
    val sentence = "If Grandma had wheels, she will not be a tea trolley."
    ("extractions from: " + sentence) in {
      val dgraph = DependencyGraph.deserialize("mark(had_VBD_2_11, If_IN_0_0); nsubj(had_VBD_2_11, Grandma_NNP_1_3); dobj(had_VBD_2_11, wheels_NNS_3_15); advcl(be_VB_8_36, had_VBD_2_11); punct(be_VB_8_36, ,_,_4_21); nsubj(be_VB_8_36, she_PRP_5_23); aux(be_VB_8_36, will_MD_6_27); neg(be_VB_8_36, not_RB_7_32); attr(be_VB_8_36, trolley_NN_11_45); punct(be_VB_8_36, ._._12_52); det(trolley_NN_11_45, a_DT_9_39); nn(trolley_NN_11_45, tea_NN_10_41)")
      val frames = Seq("have_2.03:[A0=Grandma_1, A1=wheels_3]", "be_8.01:[AM-ADV=had_2, A1=she_5, AM-MOD=will_6, AM-NEG=not_7, A2=trolley_11]") map Frame.deserialize(dgraph)

      val extrs = srl.synchronized {
        srl.extract(dgraph)(frames)
      }

      extrs.size must_== 2
      extrs.map(_.toString) must contain("(she; will not be; a tea trolley)")
    }
  }
}
