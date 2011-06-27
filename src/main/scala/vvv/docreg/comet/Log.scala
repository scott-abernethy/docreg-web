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
import util.Helpers._
import net.liftweb.http.js._
import net.liftweb.http.js.JsCmds._
import net.liftweb.http.js.jquery.JqJsCmds._
import vvv.docreg.util.Environment

object CurrentLog extends SessionVar[Box[Log]](Empty)

case class ReloadLog()

class Log extends DocumentSubscriber {
  private val documentServer = Environment.env.documentServer
  private var revisions: List[Revision] = Nil
  private lazy val revisionPart: NodeSeq = deepFindKids(defaultXml, "log", "item")
  private lazy val revisionInnerPart: NodeSeq = revisionPart \ "div"

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
      revisions = revisions map {r => if (r.document == document) r.reload else r}
      val update = revisions filter {r => r.document == document} map {r => SetHtml(r.id.is.toString, bindRevision(revisionInnerPart, r, false))}
      partialUpdate(update)
    case ReloadLog() =>
      revisions = FilteredRevision.findRecent()
      reRender(true)
    case _ =>
  }

  private def add(d: Document, r: Revision) = {
    if (d.project.map(ProjectSelection.projects.is contains _) openOr false) {
      // TODO bug here if no revisions! 
      val remove = revisions.last
      revisions = r :: revisions.dropRight(1)
      partialUpdate(PrependHtml("log", bindRevision(revisionPart, r, true)) & FadeIn(r.id.is.toString) & Replace(remove.id.is.toString, Text("")))
    }
  }

  def render = bind("log", "item" -> bindRevisions _)

  private def bindRevisions(xml: NodeSeq): NodeSeq =
    revisions.flatMap(bindRevision(xml, _, false))

  private def bindRevision(xml: NodeSeq, r: Revision, hidden: Boolean): NodeSeq = {
    val d: Document = r.document.obj openOr null
    bind("doc", xml, 
      AttrBindParam("id_attr", r.id.is.toString, "id"),
      AttrBindParam("style_attr", if (hidden) "display:none" else "", "style"),
      AttrBindParam("info_attr", r.info, "href"),
      "link" -> <span><a href={d.latest.link}>{d.key}</a></span><span class="quiet">v<a href={r.link}>{r.version}</a></span>,
      "info" -> <span><a href={d.infoLink}>more</a></span>,
      "key" -> d.key,
      "version" -> r.version,
      "project" -> d.projectName, 
      "title" -> <a href={d.infoLink}>{r.fullTitle}</a>, 
      "author" -> r.author,
      "date" -> r.date,
      "when" -> r.when,
      "comment" -> r.comment)
  }
}
