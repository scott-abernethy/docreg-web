package vvv.docreg.model

import org.specs._
import org.specs.runner.JUnit4
import org.specs.runner.ConsoleRunner
import org.specs.matcher._
import org.specs.specification._
import vvv.docreg.db.TestDbVendor
import java.util.Date

class RevisionTestSpecsAsTest extends JUnit4(RevisionTestSpecs)
object RevisionTestSpecsRunner extends ConsoleRunner(RevisionTestSpecs)

object RevisionTestSpecs extends Specification {
  "Revision Model" should {
    "have a full title representing key version title" in {
      val d = Document.create.key("0567").title("Foo bar 2")
      Revision.create.document(d).version(45).fullTitle must be equalTo("0567-45: Foo bar 2")
    }
    "not dirty itself when comment set but not changed" >> {
      TestDbVendor.initAndClean()
      val (u1, _) = TestDbVendor.createUsers
      val d = Document.create.key("0567").title("Foo bar 2")
      d.save
      val x: Revision = Revision.create.document(d).version(1).filename("lalala.txt").author(u1).date(new Date).comment("foobarbaz")
      x.save

      x.dirty_? must beFalse

      x.comment("foobarbaz")
      x.dirty_? must beFalse
      x.save
      x.dirty_? must beFalse

      x.comment("other")
      x.dirty_? must beTrue
      x.save
      x.dirty_? must beFalse
    }
  }
}
