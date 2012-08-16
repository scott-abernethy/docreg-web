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

case object StreamModeChanged

class Log extends CometActor {
  val pageLimit = 80
  private val documentServer = Environment.env.documentStream
//  private var recent: List[(Document,Revision,Project)] = FilteredRevision.findRecent(-1)
  private var filtered: List[(Document,Revision,Project)] = Nil
  private lazy val revisionPart: NodeSeq = (".log-item ^^" #> "foo").apply(defaultHtml)
  var cachedFilter = UserSession.inStreamFilter()
  var ready = false

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
    case StreamState(items) => {
      ready = true
      renderState(items)
    }
    case StreamModeChanged => {
      documentServer ! StreamQuery(this)
      ready = false
      cachedFilter = UserSession.inStreamFilter()
      reRender(true) // will render as "loading..."
    }
    case _ if (!ready) => {
      // Ignore
    }
    case StreamAddition(document, revision, project) => {
      add(document, revision, project)
    }
    case StreamInsert(document, revision, project, items) => {
      // TODO this is inefficient, could insert new item instead
      renderState(items)
    }
    case StreamChange(documentId) => {
      // TODO this is inefficient. DocumentStream has already done the same. Send in message.
      // TODO check project and revision for change to?
      filtered = filtered flatMap {x => if (x._2.documentId == documentId) x._2.reload.map( (x._1, _, x._3) ) else Some(x)}
      val update = filtered filter {x => x._2.documentId == documentId} map {x => JsCmds.Replace(x._2.id.toString, bindRevision(x._1, x._2, false))}
      partialUpdate(update)
    }
    case _ => {}
  }

  def renderState(items: List[(Document,Revision,Project)]) {
    filtered = items.filter(i => cachedFilter(i._1, i._2, i._3)).take(pageLimit)
    partialUpdate(Replace("log", transform.apply(defaultHtml)))
  }

  private def add(d: Document, r: Revision, p: Project) = {
    val addition = (d,r,p)
    if (cachedFilter.apply(d,r,p)) {
      filtered.lastOption match {
        case Some(remove) if filtered.size > (pageLimit + 10) => {
          filtered = (d,r,p) :: filtered.dropRight(1)
          partialUpdate(PrependHtml("log", bindRevision(d, r, true)) & FadeIn(r.id.toString) & Replace(remove._2.id.toString, Text("")))
        }
        case _ => {
          filtered = (d,r,p) :: filtered
          partialUpdate(PrependHtml("log", bindRevision(d, r, true)) & FadeIn(r.id.toString))
        }
      }
    }
  }

  def render = {
    if (ready) {
      transform
    }
    else {
      ".log-item *" #> <img src="/static/img/load-w.gif" title="Loading..."></img>
    }
  }

  def transform: NodeSeq => NodeSeq = {
    filtered match {
      case Nil => {
        ".log-item *" #> "None"
      }
      case list => {
        ".log-item" #> list.map {
          x => transformRevision(false)(x._1, x._2)
        }
      }
    }
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
    ".doc-comment" #> <span>{r.comment}</span> &
    ".doc-when" #> r.when &
    ".doc-project" #> d.project().map(_.infoLink()).getOrElse(NodeSeq.Empty) &
    ".doc-info [href]" #> d.infoLink
  }
}