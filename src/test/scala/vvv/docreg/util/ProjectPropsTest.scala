/*
 * Copyright (c) 2013 Aviat Networks.
 * This file is part of DocReg+Web. Please refer to the NOTICE.txt file for license details.
 */

package vvv.docreg.util

import org.specs2.mutable._
import net.liftweb.common._

class ProjectPropsTestSpecs extends Specification {
  "ProjectProps" should {
    "load from project properties file" in {
      ProjectProps.get("project.version") match {
        case Full(x) => x must beMatching("[0-9]+\\.[0-9]+\\.[0-9]+")
        case _ => failure
      }
      ProjectProps.get("project.name") must be equalTo(Full("DocReg+Web"))
    }
  }
}
