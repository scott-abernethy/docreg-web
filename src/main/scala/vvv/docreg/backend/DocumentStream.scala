package vvv.docreg.backend

import akka.actor.{ActorRef, Actor}
import vvv.docreg.model.{FilteredRevision, Project, Revision, Document}
import akka.event.Logging
import net.liftweb.actor.LiftActor
import java.sql.Timestamp
import akka.util.duration._
import vvv.docreg.util.T
import org.squeryl.PrimitiveTypeMode._

case class Subscribe(subscriber: LiftActor)
case class Unsubscribe(subscriber: LiftActor)
case class DocumentAdded(document: Document)
case class DocumentRevised(document: Document, latest: Revision)
case class DocumentChanged(document: Document)

case class StreamState(items: List[(Document,Revision,Project)])
case class StreamAddition(document: Document, revision: Revision, project: Project)
case class StreamInsert(document: Document, revision: Revision, project: Project, result: List[(Document,Revision,Project)])
case class StreamChange(documentId: Long)
case class StreamQuery(subscriber: LiftActor)

trait DocumentStreamComponent {
  val documentStream: ActorRef
}

class DocumentStream extends Actor {
  val logger = Logging(context.system, this)
  var subscribers = Set.empty[LiftActor]
  var start = cutoff()
  var stream = List.empty[(Document,Revision,Project)]

  override def preStart() {
    stream = FilteredRevision.findRecent(-1)
    context.system.scheduler.schedule(24 hours, 24 hours, self, 'SetStart)
    super.preStart()
  }

  def receive = {
    case 'SetStart => {
      start = cutoff()
      // TODO filter stream
      // TODO distribute
    }
    case Subscribe(subscriber) => {
      subscribers = subscribers + subscriber
      subscriber ! StreamState(stream)
      logger.debug("Stream subscription, now have [{}] subscribers.", subscribers.size)
    }
    case Unsubscribe(subscriber) => {
      subscribers = subscribers - subscriber
    }
    case DocumentAdded(d) => {
      logger.info("Stream +++ [{}] added", d.key)
      insertInStream(d, d.latest, d.project()) foreach (distribute _)
    }
    case DocumentRevised(d, r) => {
      logger.info("Stream *** [{}] revised", d.key)
      insertInStream(d, r, d.project()) foreach (distribute _)
    }
    case DocumentChanged(d) => {
      // TODO check project and revision for change to? Send in distributed message.
      logger.info("Stream ~~~ [{}] changed", d.key)
      stream = inTransaction( stream flatMap { x =>
        if (x._2.documentId == d.id) x._2.reload.map( (x._1, _, x._3) ) else Some(x)
      })
      distribute(StreamChange(d.id))
    }
    case StreamQuery(subscriber) => {
      subscriber ! StreamState(stream)
    }
    case other => {
      unhandled(other)
    }
  }

  def cutoff(): Timestamp = {
    T.ago(1000L * 60 * 60 * 24 * 31)
  }

  def distribute(msg: AnyRef) {
    subscribers foreach (_ ! msg)
  }

  def insertInStream(document: Document, revision: Revision, projectOption: Option[Project]): Option[AnyRef] = {
    projectOption match {
      case _ if (!withinScope(revision.date)) => {
        None
      }
      case Some(project) => {
        if (mostRecent(revision.date)) {
          stream = (document, revision, project) :: stream
          Some(StreamAddition(document, revision, project))
        }
        else {
          stream = ((document, revision, project) :: stream) sortWith (mostRecentFirst _)
          Some(StreamInsert(document, revision, project, stream))
        }
      }
      case None => {
        logger.warning("Stream failed to insert for [{}], could not identify project!", document.key)
        None
      }
    }
  }

  def withinScope(timestamp: Timestamp): Boolean = {
    start.before(timestamp)
  }

  def mostRecent(timestamp: Timestamp): Boolean = {
    stream.headOption.map(_._2.date) match {
      case None => true
      case Some(current) => current.before(timestamp)
    }
  }

  def mostRecentFirst(a: (Document, Revision, Project), b:(Document, Revision, Project)): Boolean = {
    a._2.date.compareTo(b._2.date) match {
      case x if (x > 0) => true
      case 0 => a._2.id > b._2.id
      case _ => false
    }
  }
}
