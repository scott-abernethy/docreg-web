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
