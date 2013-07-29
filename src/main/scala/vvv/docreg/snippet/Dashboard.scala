/*
 * Copyright (c) 2013 Aviat Networks.
 * This file is part of DocReg+Web. Please refer to the NOTICE.txt file for license details.
 */

package vvv.docreg.snippet

import vvv.docreg.comet._
import vvv.docreg.model._
import vvv.docreg.helper._
import net.liftweb._
import util._
import common._
import Helpers._
import http._
import js._
import js.JsCmds._
import scala.xml.{NodeSeq, Text}

class Dashboard extends Loggable 
  with ProjectSelection {

  lazy val indexXhtml = Templates("index" :: Nil) openOr <div/>
  lazy val logXhtml = indexXhtml \\ "surround" \ "div"

  def log(in: NodeSeq): NodeSeq = {
    in
  }

  override def modeSelectionUpdate(): JsCmd = {
    reload()
  }

  override def projectSelectionUpdate(): JsCmd = {
    if (UserSession.mode.is == StreamMode.selected) {
      reload()
    }
    else {
      Noop
    }
  }

  def reload(): JsCmd = {
    CurrentLog.foreach(_ ! StreamModeChanged)
  }
}
