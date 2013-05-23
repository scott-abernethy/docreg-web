/*
 * Copyright (c) 2013 Scott Abernethy.
 * This file is part of DocReg+Web. Please refer to the NOTICE.txt file for license details.
 */

package vvv.docreg.snippet

import net.liftweb.common.Loggable
import scala.xml.{Unparsed, NodeSeq}
import net.liftweb.util.Helpers._
import vvv.docreg.model._
import net.liftweb.http._
import net.liftweb.util.{ClearNodes, PassThru, CssSel}

class ProjectListSnippet extends Loggable {

  def list() = {
    val projects = Project.findAllUsed()
    ".p-count *" #> projects.size &
    ".p-item *" #> projects.map { project =>
      project.infoLink() ++ Unparsed("&nbsp; ")
    }
  }

}

