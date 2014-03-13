package edu.knowitall.srlie.confidence

import edu.knowitall.tool.parse.ClearParser
import edu.knowitall.tool.srl.Frame
import edu.knowitall.tool.parse.graph.DependencyGraph
import edu.knowitall.srlie.SrlExtractor
import edu.knowitall.tool.parse.DependencyParser
import org.scalatest._
import edu.knowitall.tool.stem.MorphaStemmer

class SrlConfidenceFunctionTest extends FlatSpec {
  val srl = new SrlExtractor(null)

  val sentence = "John, who ran a mile, ate corn on the cob."
  "srl confidence function" must ("not throw an exception on '" + sentence + "'") in {
    val conf = SrlConfidenceFunction.loadDefaultClassifier()
    val (tokens, dgraph) = DependencyParser.multilineStringFormat.read(
        """John 0 NNP
, 4 ,
who 6 WP
ran 10 VBD
a 14 DT
mile 16 NN
, 20 ,
ate 22 VBD
corn 26 NN
on 31 IN
the 34 DT
cob 38 NN
. 41 .

nsubj(ate-8, John-1)
punct(John-1, ,-2)
nsubj(ran-4, who-3)
rcmod(John-1, ran-4)
det(mile-6, a-5)
dobj(ran-4, mile-6)
punct(ate-8, ,-7)
root(ROOT-0, ate-8)
dobj(ate-8, corn-9)
prep(ate-8, on-10)
det(cob-12, the-11)
pobj(on-10, cob-12)
punct(ate-8, .-13)""")
    val frames = IndexedSeq(
      "run_3.01:[A0=John_0, R-A0=who_2, A1=mile_5]",
      "eat_7.01:[A0=John_0, A1=corn_8, AM-LOC=on_9]") map Frame.deserialize(dgraph)
    val insts = srl.synchronized {
      srl.extract(tokens map MorphaStemmer.lemmatizePostaggedToken, dgraph)(frames)
    }

    insts.map { inst => (inst, conf(inst)) }
  }
}
