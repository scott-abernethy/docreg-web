/*
 * Copyright (c) 2013 Scott Abernethy.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

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
