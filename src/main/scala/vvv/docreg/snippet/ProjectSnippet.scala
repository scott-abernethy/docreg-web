package vvv.docreg.snippet

import net.liftweb.common.{Empty, Full, Box, Loggable}
import xml.NodeSeq
import net.liftweb.util.Helpers._
import vvv.docreg.model._
import net.liftweb.http._

class ProjectSnippet extends Loggable {
  val key = S.param("key") openOr ""
  val project: Box[Project] = try {
    Project.find(key.toLong)
  } catch {
    case e:NumberFormatException => None
  }

  def info(in: NodeSeq): NodeSeq = {
    project match {
      case Full(p) => {
        val t = ".p-name" #> p.name
        t(in)
      }
      case _ => {
        <div class="alert-message error"><p><strong>Invalid</strong>{" project '" + key + "'"}</p></div>
      }
    }
  }
}
