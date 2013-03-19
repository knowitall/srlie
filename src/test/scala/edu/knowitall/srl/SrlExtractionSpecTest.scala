package edu.knowitall.srl

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
  */

  {
    val sentence = "Alcohol is a drug, a sedative, which depresses the central nervous system"
    ("extractions from: " + sentence) in {
      val dgraph = DependencyGraph.deserialize("nsubj(is_VBZ_1_8, Alcohol_NN_0_0); attr(is_VBZ_1_8, drug_NN_3_13); det(drug_NN_3_13, a_DT_2_11); punct(drug_NN_3_13, ,_,_4_17); appos(drug_NN_3_13, sedative_NN_6_21); det(sedative_NN_6_21, a_DT_5_19); punct(sedative_NN_6_21, ,_,_7_29); rcmod(sedative_NN_6_21, depresses_VBZ_9_37); nsubj(depresses_VBZ_9_37, which_WDT_8_31); dobj(depresses_VBZ_9_37, system_NN_13_67); det(system_NN_13_67, the_DT_10_47); amod(system_NN_13_67, central_JJ_11_51); amod(system_NN_13_67, nervous_JJ_12_59)")
      val frames = Seq("be_1.01:[A1=Alcohol_0, A2=drug_3]", "depress_9.02:[A0=sedative_6, R=which_8, A1=system_13]") map Frame.deserialize(dgraph)

      val extrs = srl.synchronized {
        srl.extract(dgraph)(frames).map(_.extr)
      }

      extrs.size must_== 2
      extrs.map(_.toString) must haveTheSameElementsAs(Seq("(Alcohol; is; a drug)", "(a sedative; depresses; the central nervous system)"))
    }
  }

  {
    val sentence = "If Grandma had wheels, she will not be a tea trolley."
    ("extractions from: " + sentence) in {
      val dgraph = DependencyGraph.deserialize("mark(had_VBD_2_11, If_IN_0_0); nsubj(had_VBD_2_11, Grandma_NNP_1_3); dobj(had_VBD_2_11, wheels_NNS_3_15); advcl(be_VB_8_36, had_VBD_2_11); punct(be_VB_8_36, ,_,_4_21); nsubj(be_VB_8_36, she_PRP_5_23); aux(be_VB_8_36, will_MD_6_27); neg(be_VB_8_36, not_RB_7_32); attr(be_VB_8_36, trolley_NN_11_45); punct(be_VB_8_36, ._._12_52); det(trolley_NN_11_45, a_DT_9_39); nn(trolley_NN_11_45, tea_NN_10_41)")
      val frames = Seq("have_2.03:[A0=Grandma_1, A1=wheels_3]", "be_8.01:[AM-ADV=had_2, A1=she_5, AM-MOD=will_6, AM-NEG=not_7, A2=trolley_11]") map Frame.deserialize(dgraph)

      val extrs = srl.synchronized {
        srl.extract(dgraph)(frames).map(_.extr)
      }

      extrs.size must_== 2
      extrs.map(_.toString) must contain("(she; will not be; a tea trolley)")
    }
  }

  {
    val sentence = "John read a book in which philosophy was discussed."
    ("extractions from: " + sentence) in {
      val dgraph = DependencyGraph.deserialize("nsubj(read_VBD_1_5, John_NNP_0_0); dobj(read_VBD_1_5, book_NN_3_12); punct(read_VBD_1_5, ._._9_50); det(book_NN_3_12, a_DT_2_10); rcmod(book_NN_3_12, discussed_VBN_8_41); pcomp(in_IN_4_17, which_WDT_5_20); prep(discussed_VBN_8_41, in_IN_4_17); nsubjpass(discussed_VBN_8_41, philosophy_NN_6_26); auxpass(discussed_VBN_8_41, was_VBD_7_37)")
      val frames = Seq("read_1.01:[A0=John_0, A1=book_3]", "discuss_8.01:[AM-LOC=book_3, R-AM-LOC=in_4, A1=philosophy_6]") map Frame.deserialize(dgraph)

      val extrs = srl.synchronized {
        srl.extract(dgraph)(frames).map(_.extr)
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
        srl.extract(dgraph)(frames).map(_.extr)
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
        srl.extract(dgraph)(frames).map(_.extr)
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
        srl.extract(dgraph)(frames).map(_.extr)
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
        srl.extract(dgraph)(frames).map(_.extr) must not(throwA[Exception])
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
        srl.extract(dgraph)(frames).map(_.extr).map(_.toString) must haveTheSameElementsAs(List("(John; was; 5 years old)", "(John; ate; corn; L:on the cob)"))
      }
    }
  }

  {
    val sentence = "NJ threw the ball to Michae on Friday after work."
    ("no errors with: '" + sentence + "'") in {
      val dgraph = DependencyGraph.deserialize("nsubj(threw_VBD_1_3, NJ_NNP_0_0); dobj(threw_VBD_1_3, ball_NN_3_13); prep(threw_VBD_1_3, to_TO_4_18); prep(threw_VBD_1_3, on_IN_6_28); prep(threw_VBD_1_3, after_IN_8_38); punct(threw_VBD_1_3, ._._10_48); det(ball_NN_3_13, the_DT_2_9); pobj(to_TO_4_18, Michae_NNP_5_21); pobj(on_IN_6_28, Friday_NNP_7_31); pobj(after_IN_8_38, work_NN_9_44)")
      val frames = IndexedSeq(
         "throw_1.01:[A0=NJ_0, A1=ball_3, A2=to_4, AM-TMP=on_6, AM-TMP=after_8]") map Frame.deserialize(dgraph)
      srl.synchronized {
        val extrs = srl.extract(dgraph)(frames).map(_.extr)
        extrs.flatMap(_.triplize(true)).map(_.toString) must haveTheSameElementsAs(List("(NJ; threw the ball; to Michae)", "(NJ; threw the ball; T:on Friday)", "(NJ; threw the ball; T:after work)"))
        extrs.flatMap(_.triplize(false)).map(_.toString) must haveTheSameElementsAs(List("(NJ; threw; the ball)", "(NJ; threw; to Michae)", "(NJ; threw; T:on Friday)", "(NJ; threw; T:after work)"))
      }
    }
  }

  def expectedExtractions(sentence: String, dgraphString: String, frameStrings: Seq[String], expectedExtractions: Seq[String]) = {
    ("no errors with: '" + sentence + "'") in {
      val dgraph = DependencyGraph.deserialize(dgraphString)
      val frames = frameStrings map Frame.deserialize(dgraph)
      srl.synchronized {
        val extrs = srl.extract(dgraph)(frames).map(_.extr)
        extrs.map(_.toString) must haveTheSameElementsAs(expectedExtractions)
      }
    }
  }

  expectedExtractions(
    sentence = "Microsoft plans on filing a lawsuit against Google in New York.",
    dgraphString = "nsubj(plans_VBZ_1_10, Microsoft_NNP_0_0); prep(plans_VBZ_1_10, on_IN_2_16); punct(plans_VBZ_1_10, ._._11_62); pcomp(on_IN_2_16, filing_VBG_3_19); dobj(filing_VBG_3_19, lawsuit_NN_5_28); det(lawsuit_NN_5_28, a_DT_4_26); prep(lawsuit_NN_5_28, against_IN_6_36); pobj(against_IN_6_36, Google_NNP_7_44); prep(Google_NNP_7_44, in_IN_8_51); pobj(in_IN_8_51, York_NNP_10_58); nn(York_NNP_10_58, New_NNP_9_54)",
    frameStrings = Seq("plan_1.01:[A0=Microsoft_0, A1=on_2]", "file_3.01:[A0=Microsoft_0, A1=lawsuit_5, A3=against_6]"),
    expectedExtractions = Seq("(Microsoft; plans; on filing a lawsuit against Google in New York)", "(Microsoft; plans on filing; a lawsuit; against Google in New York)"))

  expectedExtractions(
    sentence = "That mis-characterizes when Shark said he loves ham.",
    dgraphString = """nsubj(characterizes_VBZ_3_9, That_DT_0_0); hmod(characterizes_VBZ_3_9, mis_NN_1_5); hyph(characterizes_VBZ_3_9, -_HYPH_2_8); advcl(characterizes_VBZ_3_9, said_VBD_6_34); punct(characterizes_VBZ_3_9, ._._10_51); advmod(said_VBD_6_34, when_WRB_4_23); nsubj(said_VBD_6_34, Shark_NNP_5_28); ccomp(said_VBD_6_34, loves_VBZ_8_42); nsubj(loves_VBZ_8_42, he_PRP_7_39); dobj(loves_VBZ_8_42, ham_NN_9_48)""",
    frameStrings = Seq("characterize_3.01:[A0=That_0, AM-TMP=said_6]", "say_6.01:[R-AM-TMP=when_4, A0=Shark_5, A1=loves_8]", "love_8.01:[A0=he_7, A1=ham_9]"),
    expectedExtractions = Seq("(That; characterizes; T:when Shark said he loves ham)", "(Shark; said; he loves ham)", "(he; loves; ham)"))

  expectedExtractions(
    sentence = "It was only when Jesus was in the boat and shouted , Peace be still that the disciples were saved from that terrible storm on the Sea of Galilee .",
    dgraphString = "nsubj(was_VBD_1_3, It_PRP_0_0); advcl(was_VBD_1_3, was_VBD_5_23); punct(was_VBD_1_3, ._._29_145); advmod(was_VBD_5_23, when_WRB_3_12); nsubj(was_VBD_5_23, Jesus_NNP_4_17); prep(was_VBD_5_23, in_IN_6_27); cc(was_VBD_5_23, and_CC_9_39); conj(was_VBD_5_23, shouted_VBD_10_43); pobj(in_IN_6_27, boat_NN_8_34); det(boat_NN_8_34, the_DT_7_30); punct(shouted_VBD_10_43, ,_,_11_51); ccomp(shouted_VBD_10_43, be_VB_13_59); advmod(be_VB_13_59, only_RB_2_7); nsubj(be_VB_13_59, Peace_NN_12_53); advmod(be_VB_13_59, still_RB_14_62); ccomp(be_VB_13_59, saved_VBN_19_92); det(disciples_NNS_17_77, the_DT_16_73); complm(saved_VBN_19_92, that_IN_15_68); nsubjpass(saved_VBN_19_92, disciples_NNS_17_77); auxpass(saved_VBN_19_92, were_VBD_18_87); prep(saved_VBN_19_92, from_IN_20_98); pobj(from_IN_20_98, storm_NN_23_117); det(storm_NN_23_117, that_DT_21_103); amod(storm_NN_23_117, terrible_JJ_22_108); prep(storm_NN_23_117, on_IN_24_123); pobj(on_IN_24_123, Sea_NNP_26_130); det(Sea_NNP_26_130, the_DT_25_126); prep(Sea_NNP_26_130, of_IN_27_134); pobj(of_IN_27_134, Galilee_NNP_28_137)",
    frameStrings = Seq("be_1.01:[A1=It_0, A2=was_5]",
      "be_5.01:[R-AM-TMP=when_3, A1=Jesus_4, A2=in_6]",
      "shout_10.01:[R-AM-TMP=when_3, A0=Jesus_4, A1=be_13]",
      "be_13.01:[AM-ADV=only_2, A1=Peace_12, AM-TMP=still_14, A2=saved_19]",
      "save_19.02:[A1=disciples_17, A2=from_20]"),
    expectedExtractions = Seq("(It; was; only when Jesus was in the boat and shouted , Peace be still that the disciples were saved from that terrible storm on the Sea of Galilee)",
      "(Jesus; shouted; Peace be still that the disciples were saved from that terrible storm on the Sea of Galilee)"))
}