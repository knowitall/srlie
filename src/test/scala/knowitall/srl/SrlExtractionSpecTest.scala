package knowitall.srl

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import edu.washington.cs.knowitall.tool.parse.ClearParser

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

  "Obama was criticized by Romney for his tax plan." in {
    val sentence = "Obama was criticized by Romney for his tax plan."
    val extrs = srl.synchronized {
      val graph = parser.dependencyGraph(sentence)
      srl(graph)
    }

    val target = extrs.find(_.arg1.text == "Obama")
    target must beSome

    target.get.active must beFalse
  }

}
