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
        val documents: List[Document] = p.documents()
        val contributors: List[User] = p.contributors()
        val t = ".p-name" #> p.name &
          ".d-count" #> documents.size &
          ".d-item" #> documents.map { d =>
            "li *" #> d.info()
          } &
          ".c-count" #> contributors.size &
          ".c-item" #> contributors.map { u =>
            "li *" #> u.profileLink()
          }
        t(in)
      }
      case _ => {
        <div class="alert-message error"><p><strong>Invalid</strong>{" project '" + key + "'"}</p></div>
      }
    }
  }
}

