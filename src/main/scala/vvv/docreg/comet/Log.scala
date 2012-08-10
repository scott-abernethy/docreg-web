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

case object ResetLog

class Log extends DocumentSubscriber {
  val pageLimit = 80
  private val documentServer = Environment.env.documentServer
  private var recent: List[(Document,Revision,Project)] = FilteredRevision.findRecent(-1)
  private var stream: List[(Document,Revision,Project)] = Nil
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
    case Subscribed() => {}
    case DocumentAdded(document) => {
      add(document, document.latest)
    }
    case DocumentRevised(document, latest) => {
      add(document, latest)
    }
    case DocumentChanged(document) => {
      // todo inefficient and lame
      recent = recent flatMap {x => if (x._2.documentId == document.id) x._2.reload.map( (x._1, _, x._3) ) else Some(x)}
      stream = stream flatMap {x => if (x._2.documentId == document.id) x._2.reload.map( (x._1, _, x._3) ) else Some(x)}
      val update = stream filter {x => x._2.documentId == document.id} map {x => JsCmds.Replace(x._2.id.toString, bindRevision(x._1, x._2, false))}
      partialUpdate(update)
    }
    case ResetLog => {
      this ! 'Update
      reRender(true)
    }
    case 'Update => {
      val f = UserSession.inStreamFilter()
      stream = recent.filter(i => f(i._1, i._2, i._3)).take(pageLimit)
      partialUpdate(Replace("log", transform.apply(defaultHtml)))
    }
    case _ => {}
  }

  private def add(d: Document, r: Revision) = {
    d.project() match {
      case Some(p) => {
        val addition = (d,r,p)
        // TODO recent will increase in size forever. Need to reload once per day.
        recent = addition :: recent
        if (UserSession.inStreamFilter.apply(d,r,p)) {
          stream.lastOption match {
            case Some(remove) if stream.size > (pageLimit + 10) => {
              stream = (d,r,p) :: stream.dropRight(1)
              partialUpdate(PrependHtml("log", bindRevision(d, r, true)) & FadeIn(r.id.toString) & Replace(remove._2.id.toString, Text("")))
            }
            case _ => {
              stream = (d,r,p) :: stream
              partialUpdate(PrependHtml("log", bindRevision(d, r, true)) & FadeIn(r.id.toString))
            }
          }
        }
      }
      case _ => {}
    }
  }

  def render = {
    this ! 'Update
    ".log-item *" #> <img src="/static/img/load-w.gif" title="Loading..."></img>
  }

  def transform: NodeSeq => NodeSeq = {
    stream match {
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