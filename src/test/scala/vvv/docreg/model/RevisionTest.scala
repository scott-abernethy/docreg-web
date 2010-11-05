package vvv.docreg.model

import org.specs._
import org.specs.runner.JUnit4
import org.specs.runner.ConsoleRunner
import org.specs.matcher._
import org.specs.specification._

class RevisionTestSpecsAsTest extends JUnit4(RevisionTestSpecs)
object RevisionTestSpecsRunner extends ConsoleRunner(RevisionTestSpecs)

object RevisionTestSpecs extends Specification {
  "Revision Model" should {
    "have a full title representing key version title" in {
      val d = Document.create.key("0567").title("Foo bar 2")
      Revision.create.document(d).version(45).fullTitle must be equalTo("0567-45: Foo bar 2")
    }
  }
}
