package vvv.docreg.util

import org.specs._
import org.specs.runner.JUnit4
import org.specs.runner.ConsoleRunner
import org.specs.matcher._
import org.specs.specification._

class StringUtilTestSpecsAsTest extends JUnit4(StringUtilTestSpecs)
object StringUtilTestSpecsRunner extends ConsoleRunner(StringUtilTestSpecs)

object StringUtilTestSpecs extends Specification {
  "StringUtil" should {
    "convert email to name" in {
      StringUtil.nameFromEmail("foo@bar.com") must be equalTo("Foo")
      StringUtil.nameFromEmail("scott.abernethy@aviatnet.com") must be equalTo("Scott Abernethy")
      StringUtil.nameFromEmail("SCOTT-andrew+abErnethy@Gmail.com") must be equalTo("Scott Andrew Abernethy")
      StringUtil.nameFromEmail("david.36.smith@bt.com") must be equalTo("David Smith")
      StringUtil.nameFromEmail("Sunil.Kumar2@Aviatnet.com") must be equalTo("Sunil Kumar")
      StringUtil.nameFromEmail("") must be equalTo("")
      StringUtil.nameFromEmail("the quick Brown fox") must be equalTo("")
      StringUtil.nameFromEmail("yogie@") must be equalTo("Yogie")
      StringUtil.nameFromEmail("@that doesn't work") must be equalTo("")
    }
  }
}
