package edu.knowitall.srlie

import edu.knowitall.tool.parse.ClearParser
import edu.knowitall.tool.srl.Frame
import edu.knowitall.tool.parse.DependencyParser
import edu.knowitall.tool.srl.FrameHierarchy
import edu.knowitall.tool.stem.MorphaStemmer

import org.scalatest._

class SrlExtractionSpecTest extends FlatSpec with Matchers {
  val srl = new SrlExtractor(null)

  def expectedExtractions(sentence: String, dgraphString: String, frameStrings: Seq[String], expectedExtractions: Seq[String]) = {
    it must ("have no errors with: '" + sentence + "'") in {
      val (tokens, dgraph) = DependencyParser.multilineStringFormat.read(dgraphString)
      val frames = frameStrings map Frame.deserialize(dgraph)
      srl.synchronized {
        val insts = srl.extract(tokens map MorphaStemmer.lemmatizePostaggedToken, dgraph)(frames)
        val extrs = insts.map(_.extr)

        // make sure character offsets are correct for arguments (args must be contiguous)
        require(insts.forall(inst => inst.extr.arg1.text == inst.sentenceText.substring(inst.extr.arg1.offsets.start, inst.extr.arg1.offsets.end)))
        require(insts.forall(inst => inst.extr.arg2s.forall(arg2 => arg2.text == inst.sentenceText.substring(arg2.offsets.start, arg2.offsets.end))))

        extrs.map(_.toString) should contain theSameElementsAs (expectedExtractions)
      }
    }
  }

  def expectedTriples(sentence: String, dgraphString: String, frameStrings: Seq[String], expectedTriples: Seq[String]) = {
    it must ("have no errors with: '" + sentence + "'") in {
      val (tokens, dgraph) = DependencyParser.multilineStringFormat.read(dgraphString)
      val frames = frameStrings map Frame.deserialize(dgraph)
      srl.synchronized {
        val triples = srl.extract(tokens map MorphaStemmer.lemmatizePostaggedToken, dgraph)(frames).flatMap(_.extr.triplize())
        triples.map(_.toString) should contain theSameElementsAs (expectedTriples)
      }
    }
  }

  expectedExtractions(
    "Alcohol is a drug, a sedative, which depresses the central nervous system",
    """Alcohol 0 NNP
is 8 VBZ
a 11 DT
drug 13 NN
, 17 ,
a 19 DT
sedative 21 NN
, 29 ,
which 31 WDT
depresses 37 VBZ
the 47 DT
central 51 JJ
nervous 59 JJ
system 67 NN

nsubj(is-2, Alcohol-1)
root(ROOT-0, is-2)
det(drug-4, a-3)
attr(is-2, drug-4)
punct(drug-4, ,-5)
det(sedative-7, a-6)
appos(drug-4, sedative-7)
punct(sedative-7, ,-8)
nsubj(depresses-10, which-9)
rcmod(sedative-7, depresses-10)
det(system-14, the-11)
amod(system-14, central-12)
amod(system-14, nervous-13)
dobj(depresses-10, system-14)""",
    Seq("be_1.01:[A1=Alcohol_0, A2=drug_3]", "depress_9.02:[A0=sedative_6, R=which_8, A1=system_13]"),
    Seq("(Alcohol; is; a drug)", "(a sedative; depresses; the central nervous system)"))

  expectedExtractions(
    "John wants to blow his nose.",
    """John 0 NNP
wants 5 VBZ
to 11 TO
blow 14 VB
his 19 PRP$
nose 23 NN
. 27 .

nsubj(wants-2, John-1)
root(ROOT-0, wants-2)
aux(blow-4, to-3)
xcomp(wants-2, blow-4)
poss(nose-6, his-5)
dobj(blow-4, nose-6)
punct(wants-2, .-7)""",
    Seq("want_1.01:[A0=John_0, A1=blow_3]", "blow_3.09:[A0=John_0, A1=nose_5]"),
    Seq("(John; wants; to blow his nose)", "John wants:(John; wants to blow; his nose)"))

  expectedExtractions(
    "John wants to blow his nose and eat corn.",
    """John 0 NNP
wants 5 VBZ
to 11 TO
blow 14 VB
his 19 PRP$
nose 23 NN
and 28 CC
eat 32 VB
corn 36 NN
. 40 .

nsubj(wants-2, John-1)
root(ROOT-0, wants-2)
aux(blow-4, to-3)
xcomp(wants-2, blow-4)
poss(nose-6, his-5)
dobj(blow-4, nose-6)
cc(blow-4, and-7)
conj(blow-4, eat-8)
dobj(eat-8, corn-9)
punct(wants-2, .-10)""",
    Seq("want_1.01:[A0=John_0, A1=blow_3]", "blow_3.09:[A0=John_0, A1=nose_5]", "eat_7.01:[A0=John_0, A1=corn_8]"),
    Seq("(John; wants; to blow his nose and eat corn)", "John wants:(John; wants to blow; his nose)", "John wants:(John; wants to eat; corn)"))
            
  expectedExtractions(
    sentence = "Microsoft plans on filing a lawsuit against Google in New York.",
    dgraphString = """Microsoft 0 NNP
plans 10 VBZ
on 16 IN
filing 19 VBG
a 26 DT
lawsuit 28 NN
against 36 IN
Google 44 NNP
in 51 IN
New 54 NNP
York 58 NNP
. 62 .

nsubj(plans-2, Microsoft-1)
root(ROOT-0, plans-2)
prep(plans-2, on-3)
pcomp(on-3, filing-4)
det(lawsuit-6, a-5)
dobj(filing-4, lawsuit-6)
prep(lawsuit-6, against-7)
pobj(against-7, Google-8)
prep(Google-8, in-9)
nn(York-11, New-10)
pobj(in-9, York-11)
punct(plans-2, .-12)""",
    frameStrings = Seq("plan_1.01:[A0=Microsoft_0, A1=on_2]", "file_3.01:[A0=Microsoft_0, A1=lawsuit_5, A3=against_6]"),
    expectedExtractions = Seq(
      "(Microsoft; plans; on filing a lawsuit against Google in New York)",
      "Microsoft plans:(Microsoft; plans on filing; a lawsuit; against Google in New York)"))

  expectedExtractions(
    sentence = "That mis-characterizes when Shark said he loves ham.",
    dgraphString = """That 0 DT
mis 5 NN
- 8 HYPH
characterizes 9 VBZ
when 23 WRB
Shark 28 NNP
said 34 VBD
he 39 PRP
loves 42 VBZ
ham 48 NN
. 51 .

nsubj(characterizes-4, That-1)
hmod(characterizes-4, mis-2)
hyph(characterizes-4, x-3)
root(ROOT-0, characterizes-4)
advmod(said-7, when-5)
nsubj(said-7, Shark-6)
advcl(characterizes-4, said-7)
nsubj(loves-9, he-8)
ccomp(said-7, loves-9)
dobj(loves-9, ham-10)
punct(characterizes-4, .-11)""",
    frameStrings = Seq("characterize_3.01:[A0=That_0, AM-TMP=said_6]", "say_6.01:[R-AM-TMP=when_4, A0=Shark_5, A1=loves_8]", "love_8.01:[A0=he_7, A1=ham_9]"),
    expectedExtractions = Seq(
      "(That; characterizes; T:when Shark said he loves ham)",
      "(Shark; said; he loves ham)",
      "Shark said:(he; loves; ham)"))

  expectedExtractions(
    sentence = "Suharto moved his hands and spoke in a whisper today in what doctors called a miraculous recovery.",
    dgraphString = """Suharto 0 NNP
moved 8 VBD
his 14 PRP$
hands 18 NNS
and 24 CC
spoke 28 VBD
in 34 IN
a 37 DT
whisper 39 NN
today 47 NN
in 53 IN
what 56 WP
doctors 61 NNS
called 69 VBD
a 76 DT
miraculous 78 JJ
recovery 89 NN
. 97 .

nsubj(moved-2, Suharto-1)
root(ROOT-0, moved-2)
poss(hands-4, his-3)
dobj(moved-2, hands-4)
cc(moved-2, and-5)
conj(moved-2, spoke-6)
prep(spoke-6, in-7)
det(whisper-9, a-8)
pobj(in-7, whisper-9)
npadvmod(spoke-6, today-10)
prep(spoke-6, in-11)
dobj(called-14, what-12)
nsubj(called-14, doctors-13)
pcomp(in-11, called-14)
det(recovery-17, a-15)
amod(recovery-17, miraculous-16)
oprd(called-14, recovery-17)
punct(moved-2, .-18)""",
    frameStrings = Seq("move_1.01:[A0=Suharto_0, A1=hands_3]", "speak_5.01:[A0=Suharto_0, AM-LOC=in_6, AM-TMP=today_9, AM-LOC=in_10]", "call_13.01:[R-A1=what_11, A0=doctors_12, A2=recovery_16]"),
    expectedExtractions = Seq(
      "(Suharto; moved; his hands)",
      "(Suharto; spoke; L:in a whisper; T:today; L:in what doctors called a miraculous recovery)",
      "(doctors; called; a miraculous recovery)"))

  expectedExtractions(
    sentence = "In 2005 , Gruner + Jahr exited the U.S. magazine business.",
    dgraphString = """In 0 IN
2005 3 CD
, 8 ,
Gruner 10 NNP
+ 17 SYM
Jahr 19 NNP
exited 24 VBD
the 31 DT
U.S. 35 NNP
magazine 40 NN
business 49 NN
. 57 .

prep(exited-7, In-1)
pobj(In-1, 2005-2)
punct(exited-7, ,-3)
nn(Jahr-6, Gruner-4)
punct(Gruner-4, +-5)
nsubj(exited-7, Jahr-6)
root(ROOT-0, exited-7)
det(business-11, the-8)
nn(business-11, U.S.-9)
nn(business-11, magazine-10)
dobj(exited-7, business-11)
punct(exited-7, .-12)""",
    frameStrings = Seq("exit_6.01:[AM-TMP=In_0, A0=Jahr_5, A1=business_10]"),
    expectedExtractions = Seq(
      "(Gruner + Jahr; exited; the U.S. magazine business; T:In 2005)"))

  expectedTriples(
    sentence = "The president asked Americans to imagine the suicide terrorists who attacked the United States if they had been armed by Iraq .",
    dgraphString = "The 0 DT\npresident 4 NN\nasked 14 VBD\nAmericans 20 NNPS\nto 30 TO\nimagine 33 VB\nthe 41 DT\nsuicide 45 NN\nterrorists 53 NNS\nwho 64 WP\nattacked 68 VBD\nthe 77 DT\nUnited 81 NNP\nStates 88 NNP\nif 95 IN\nthey 98 PRP\nhad 103 VBD\nbeen 107 VBN\narmed 112 VBN\nby 118 IN\nIraq 121 NNP\n. 126 .\n\ndet(president-2, The-1)\nnsubj(asked-3, president-2)\nroot(ROOT-0, asked-3)\ndobj(asked-3, Americans-4)\naux(imagine-6, to-5)\nxcomp(asked-3, imagine-6)\ndet(terrorists-9, the-7)\nnn(terrorists-9, suicide-8)\ndobj(imagine-6, terrorists-9)\nnsubj(attacked-11, who-10)\nrcmod(terrorists-9, attacked-11)\ndet(States-14, the-12)\nnn(States-14, United-13)\ndobj(attacked-11, States-14)\nmark(armed-19, if-15)\nnsubjpass(armed-19, they-16)\naux(armed-19, had-17)\nauxpass(armed-19, been-18)\nadvcl(attacked-11, armed-19)\nagent(armed-19, by-20)\npobj(by-20, Iraq-21)\npunct(asked-3, .-22)",
    frameStrings = Seq("ask_2.02:[A0=president_1, A2=Americans_3, A1=imagine_5]",
      "imagine_5.01:[A0=Americans_3, A1=terrorists_8]",
      "attack_10.01:[A0=terrorists_8, R-A0=who_9, A1=States_13, AM-ADV=armed_18]",
      "arm_18.01:[A1=they_15, A0=by_19]"),
    expectedTriples = Seq("(The president; asked Americans to; imagine the suicide terrorists)",
      "(Americans; to imagine; the suicide terrorists who attacked the United States)",
      "(the suicide terrorists; attacked; the United States)",
      "(they; had been armed by; Iraq)",
      "(The president; asked; Americans)"))

  expectedExtractions(
    sentence = "John said Frank works in the yard.",
    dgraphString = "John 0 NNP\nsaid 5 VBD\nFrank 10 NNP\nworks 16 VBZ\nin 22 IN\nthe 25 DT\nyard 29 NN\n. 33 .\n\nnsubj(said-2, John-1)\nroot(ROOT-0, said-2)\nnsubj(works-4, Frank-3)\nccomp(said-2, works-4)\nprep(works-4, in-5)\ndet(yard-7, the-6)\npobj(in-5, yard-7)\npunct(said-2, .-8)",
    frameStrings = Seq("ay_1.01:[A0=John_0, A1=works_3]",
      "work_3.01:[A0=Frank_2, AM-LOC=in_4]"),
    expectedExtractions = Seq("(John; said; Frank works in the yard)", "John said:(Frank; works; L:in the yard)"))

  expectedTriples(
    sentence = "John gave the ball to Paul.",
    dgraphString = "John 0 NNP\ngave 5 VBD\nthe 10 DT\nball 14 NN\nto 19 IN\nPaul 22 NNP\n. 26 .\n\nnsubj(gave-2, John-1)\nroot(ROOT-0, gave-2)\ndet(ball-4, the-3)\ndobj(gave-2, ball-4)\nprep(gave-2, to-5)\npobj(to-5, Paul-6)\npunct(gave-2, .-7)",
    frameStrings = Seq("give_1.01:[A0=John_0, A1=ball_3, A2=to_4]"),
    expectedTriples = Seq("(John; gave the ball to; Paul)", "(John; gave; the ball)"))

  expectedExtractions(
    sentence = "She is trying to get the Pope to proclaim that Mary is CoxRedemptrix .",
    dgraphString = "She 0 PRP\nis 4 VBZ\ntrying 7 VBG\nto 14 TO\nget 17 VB\nthe 21 DT\nPope 25 NNP\nto 30 TO\nproclaim 33 VB\nthat 42 IN\nMary 47 NNP\nis 52 VBZ\nCoxRedemptrix 55 NNP\n. 69 .\n\nnsubj(trying-3, She-1)\naux(trying-3, is-2)\nroot(ROOT-0, trying-3)\naux(get-5, to-4)\nxcomp(trying-3, get-5)\ndet(Pope-7, the-6)\nnsubj(proclaim-9, Pope-7)\naux(proclaim-9, to-8)\nccomp(get-5, proclaim-9)\ncomplm(is-12, that-10)\nnsubj(is-12, Mary-11)\nccomp(proclaim-9, is-12)\nattr(is-12, CoxRedemptrix-13)\npunct(trying-3, .-14)",
    frameStrings = Seq("try_2.01:[A0=She_0, A1=get_4]",
      "get_4.04:[A0=She_0, A1=proclaim_8]",
      "proclaim_8.01:[A0=Pope_6, A1=is_11]",
      "be_11.01:[A1=Mary_10, A2=CoxRedemptrix_12]"),
    expectedExtractions = Seq("(She; is trying; to get the Pope to proclaim that Mary is CoxRedemptrix)",
      "She is trying:(She; is trying to get; the Pope to proclaim that Mary is CoxRedemptrix)",
      "She is trying to get:(the Pope; to proclaim; that Mary is CoxRedemptrix)",
      "She is trying to get the Pope to proclaim:(Mary; is; CoxRedemptrix)"))
}
