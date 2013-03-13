package knowitall.srl

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import edu.knowitall.tool.parse.ClearParser
import edu.knowitall.tool.srl.Frame
import edu.knowitall.tool.parse.graph.DependencyGraph

@RunWith(classOf[JUnitRunner])
class SrlExtractionSpecTest extends Specification {
  val srl = new SrlExtractor(null)
  // val parser = new ClearParser()

  /*
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
  */

  {
    val sentence = "John read a book in which philosophy was discussed."
    ("extractions from: " + sentence) in {
      val dgraph = DependencyGraph.deserialize("nsubj(read_VBD_1_5, John_NNP_0_0); dobj(read_VBD_1_5, book_NN_3_12); punct(read_VBD_1_5, ._._9_50); det(book_NN_3_12, a_DT_2_10); rcmod(book_NN_3_12, discussed_VBN_8_41); pcomp(in_IN_4_17, which_WDT_5_20); prep(discussed_VBN_8_41, in_IN_4_17); nsubjpass(discussed_VBN_8_41, philosophy_NN_6_26); auxpass(discussed_VBN_8_41, was_VBD_7_37)")
      val frames = Seq("read_1.01:[A0=John_0, A1=book_3]", "discuss_8.01:[AM-LOC=book_3, R-AM-LOC=in_4, A1=philosophy_6]") map Frame.deserialize(dgraph)

      val extrs = srl.synchronized {
        srl.extract(dgraph)(frames)
      }

      extrs.size must_== 2
      extrs.map(_.toString) must contain("(philosophy; was discussed; )")
    }
  }

  {
    val sentence = "John lives in Orlando."
    ("extractions from: " + sentence) in {
      val dgraph = DependencyGraph.deserialize("nsubj(lives_VBZ_1_5, John_NNP_0_0); prep(lives_VBZ_1_5, in_IN_2_11); punct(lives_VBZ_1_5, ._._4_21); pobj(in_IN_2_11, Orlando_NNP_3_14)")
      val frames = Seq("live_1.01:[A0=John_0, AM-LOC=in_2]") map Frame.deserialize(dgraph)

      val extrs = srl.synchronized {
        srl.extract(dgraph)(frames)
      }

      extrs.size must_== 1
      extrs.map(_.toString) must contain("(John; lives; L:in Orlando)")
    }
  }

  {
    val sentence = "John wants to blow his nose."
    ("extractions from: " + sentence) in {
      val dgraph = DependencyGraph.deserialize("nsubj(wants_VBZ_1_5, John_NNP_0_0); xcomp(wants_VBZ_1_5, blow_VB_3_14); punct(wants_VBZ_1_5, ._._6_27); aux(blow_VB_3_14, to_TO_2_11); dobj(blow_VB_3_14, nose_NN_5_23); poss(nose_NN_5_23, his_PRP$_4_19)")
      val frames = Seq("want_1.01:[A0=John_0, A1=blow_3]", "blow_3.09:[A0=John_0, A1=nose_5]") map Frame.deserialize(dgraph)

      val extrs = srl.synchronized {
        srl.extract(dgraph)(frames)
      }

      extrs.size must_== 2
      extrs.map(_.toString) must haveTheSameElementsAs(Seq("(John; wants; to blow his nose)", "(John; wants to blow; his nose)"))
    }
  }

  {
    val sentence = "John wants to blow his nose and eat corn."
    ("no errors with: '" + sentence + "'") in {
      val dgraph = DependencyGraph.deserialize("nsubj(wants_VBZ_1_6, John_NNP_0_0); xcomp(wants_VBZ_1_6, blow_VB_3_15); punct(wants_VBZ_1_6, ._._9_41); aux(blow_VB_3_15, to_TO_2_12); dobj(blow_VB_3_15, nose_NN_5_24); cc(blow_VB_3_15, and_CC_6_29); conj(blow_VB_3_15, eat_VB_7_33); poss(nose_NN_5_24, his_PRP$_4_20); dobj(eat_VB_7_33, corn_NN_8_37)")
      val frames = IndexedSeq(
        "want_1.01:[A0=John_0, A1=blow_3]",
        "blow_3.09:[A0=John_0, A1=nose_5]",
        "eat_7.01:[A0=John_0, A1=corn_8]") map Frame.deserialize(dgraph)

      val extrs = srl.synchronized {
        srl.extract(dgraph)(frames)
      }

      extrs.map(_.toString) must haveTheSameElementsAs(Seq("(John; wants; to blow his nose and eat corn)", "(John; wants to blow; his nose)", "(John; wants to eat; corn)"))
    }
  }

  {
    val sentence = "Meteorites are the oldest rocks found on Earth."
    ("no errors with: '" + sentence + "'") in {
      val dgraph = DependencyGraph.deserialize("nsubj(are_VBP_1_11, Meteorites_NNPS_0_0); attr(are_VBP_1_11, rocks_NNS_4_26); punct(are_VBP_1_11, ._._8_46); det(rocks_NNS_4_26, the_DT_2_15); amod(rocks_NNS_4_26, oldest_JJS_3_19); partmod(rocks_NNS_4_26, found_VBN_5_32); prep(found_VBN_5_32, on_IN_6_38); pobj(on_IN_6_38, Earth_NNP_7_41)")
      val frames = IndexedSeq(
        "be_1.01:[A1=Meteorites_0, A2=rocks_4]",
        "bind_5.01:[A1=rocks_4, AM-LOC=on_6]") map Frame.deserialize(dgraph)
      srl.synchronized {
        srl.extract(dgraph)(frames) must not(throwA[Exception])
      }
    }
  }

  {
    val sentence = "John, who ran a mile, ate corn on the cob."
    ("no errors with: '" + sentence + "'") in {
      val dgraph = DependencyGraph.deserialize("punct(John_NNP_0_0, ,_,_1_4); rcmod(John_NNP_0_0, was_VBD_3_10); nsubj(was_VBD_3_10, who_WP_2_6); acomp(was_VBD_3_10, old_JJ_6_22); num(years_NNS_5_16, 5_CD_4_14); npadvmod(old_JJ_6_22, years_NNS_5_16); nsubj(ate_VBD_8_27, John_NNP_0_0); punct(ate_VBD_8_27, ,_,_7_25); dobj(ate_VBD_8_27, corn_NN_9_31); prep(ate_VBD_8_27, on_IN_10_36); punct(ate_VBD_8_27, ._._13_46); pobj(on_IN_10_36, cob_NN_12_43); det(cob_NN_12_43, the_DT_11_39)")
      val frames = IndexedSeq(
         "be_3.01:[A1=John_0, R-A1=who_2, A2=old_6]",
         "eat_8.01:[A0=John_0, A1=corn_9, AM-LOC=on_10]") map Frame.deserialize(dgraph)
      srl.synchronized {
        srl.extract(dgraph)(frames).map(_.toString) must haveTheSameElementsAs(List("(John; was; 5 years old)", "(John; ate; corn; L:on the cob)"))
      }
    }
  }
}
