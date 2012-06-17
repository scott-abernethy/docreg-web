package vvv.docreg.comet

import vvv.docreg.model._
import vvv.docreg.backend._
import vvv.docreg.helper._
import scala.xml.{NodeSeq,Text}
import net.liftweb._
import http._
import actor._
import common._
import mapper._
import util._
import util.Helpers._
import net.liftweb.http.js._
import net.liftweb.http.js.JsCmds._
import net.liftweb.http.js.jquery.JqJsCmds._
import vvv.docreg.util.Environment

object CurrentLog extends SessionVar[Box[Log]](Empty)

case class ReloadLog()

class Log extends DocumentSubscriber {
  val loadLimit = 80
  private val documentServer = Environment.env.documentServer
  private var revisions: List[(Document,Revision,Project)] = Nil
  private lazy val revisionPart: NodeSeq = (".log-item ^^" #> "foo").apply(defaultHtml)

  CurrentLog.set(Full(this))

  override def defaultPrefix = Full("log")

  override def localSetup {
    documentServer ! Subscribe(this)
    super.localSetup
  }

  override def localShutdown {
    documentServer ! Unsubscribe(this)
    super.localShutdown
  }

  override def lowPriority = {
    case Subscribed() => 
      this ! ReloadLog()
    case DocumentAdded(document) =>
      add(document, document.latest)
    case DocumentRevised(document, latest) =>
      add(document, latest)
    case DocumentChanged(document) =>
      revisions = revisions flatMap {x => if (x._2.documentId == document.id) x._2.reload.map( (x._1, _, x._3) ) else Some(x)}
      val update = revisions filter {x => x._2.documentId == document.id} map {x => JsCmds.Replace(x._2.id.toString, bindRevision(x._1, x._2, false))}
      partialUpdate(update)
    case ReloadLog() =>
      revisions = FilteredRevision.findRecent(-1).filter(i => UserSession.isAuthorized(i._1, i._3)).take(loadLimit)
      reRender(true)
    case _ =>
  }

  private def add(d: Document, r: Revision) = {
    d.project() match {
      case Some(p) if (ProjectSelection.isSelected(p) && UserSession.isAuthorized(d,p)) => {
        revisions.lastOption match {
          case Some(remove) if revisions.size > (loadLimit + 10) => {
            revisions = (d,r,p) :: revisions.dropRight(1)
            partialUpdate(PrependHtml("log", bindRevision(d, r, true)) & FadeIn(r.id.toString) & Replace(remove._2.id.toString, Text("")))
          }
          case _ => {
            revisions = (d,r,p) :: revisions
            partialUpdate(PrependHtml("log", bindRevision(d, r, true)) & FadeIn(r.id.toString))
          }
        }
      }
      case _ => {}
    }
  }

  def render = {
    ".log-item" #> revisions.map { x => transformRevision(false)(x._1, x._2) }
  }

  private def bindRevision(d: Document, r: Revision, hidden: Boolean): NodeSeq = {
    transformRevision(hidden)(d, r).apply(revisionPart)
  }

  private def transformRevision(hidden: Boolean)(d: Document, r: Revision): CssBindFunc = {
    val blank: Option[String] = None
    ".log-item [id]" #> r.id.toString &
    ".log-item [class+]" #> (if (hidden) Some("hide") else blank) &
    ".doc-title"  #> <a href={d.infoLink}>{d.fullTitle}</a> &
    ".doc-author" #> r.author().flatMap(_.knownOption).map(_.profileLink()).getOrElse(Text(r.rawAuthor)) &
    ".doc-comment" #> r.comment &
    ".doc-when" #> r.when &
    ".doc-info [href]" #> d.infoLink
  }

//  private def bindRevision(xml: NodeSeq, r: Revision, hidden: Boolean): NodeSeq = {
//    r.document.obj match {
//      case Full(d: Document) =>
//    bind("doc", xml,
//      AttrBindParam("id_attr", r.id.is.toString, "id"),
//      AttrBindParam("style_attr", if (hidden) "display:none" else "", "style"),
//      AttrBindParam("info_attr", r.info, "href"),
//      "link" -> <span><a href={d.latest.link}>{d.key}</a></span><span class="quiet">v<a href={r.link}>{r.version}</a></span>,
//      "info" -> <span><a href={d.infoLink}>more</a></span>,
//      "key" -> d.key,
//      "version" -> r.version,
//      "project" -> d.projectName,
//      "title" -> <a href={d.infoLink}>{r.fullTitle}</a>,
//      "author" -> r.authorLink,
//      "date" -> r.date,
//      "when" -> r.when,
//      "comment" -> r.comment)
//      case _ => NodeSeq.Empty
//    }
//  }
}