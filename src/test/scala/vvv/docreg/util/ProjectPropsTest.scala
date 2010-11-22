package vvv.docreg.util

import org.specs._
import org.specs.runner.JUnit4
import org.specs.runner.ConsoleRunner
import org.specs.matcher._
import org.specs.specification._
import java.util.Properties
import net.liftweb.common._

class ProjectPropsTestSpecsAsTest extends JUnit4(ProjectPropsTestSpecs)
object ProjectPropsTestSpecsRunner extends ConsoleRunner(ProjectPropsTestSpecs)

object ProjectPropsTestSpecs extends Specification {
  "ProjectProps" should {
    "load from project properties file" in {
      ProjectProps.get("project.name") must be equalTo(Full("DocReg+Web"))
      ProjectProps.get("project.version") match {
        case Full(x) => x must beMatching("[0-9]+\\.[0-9]+\\.[0-9]+")
        case _ => fail
      }
    }
  }
}
