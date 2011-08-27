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
  def log(in: NodeSeq): NodeSeq = {
    in
  }
  lazy val indexXhtml = Templates("index" :: Nil) openOr <div/>
  //lazy val logXhtml = indexXhtml \\ "snippet" filter ( _.attribute("type") == Some(Text("Dashboard.log")) )
  lazy val logXhtml = indexXhtml \\ "surround" \ "div"
  override def projectSelectionUpdate(): JsCmd = {
    CurrentLog.foreach(_ ! ReloadLog())
  }
}
