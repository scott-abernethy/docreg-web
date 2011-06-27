package vvv.docreg.backend

import scala.actors.Actor
import scala.actors.Actor._
import net.liftweb.http._
import net.liftweb.actor._
import net.liftweb.util._
import net.liftweb.common._
import vvv.docreg.model._

case class Subscribe(subscriber: DocumentSubscriber)
case class Subscribed()
case class Unsubscribe(subscriber: DocumentSubscriber)
case class DocumentAdded(document: Document)
case class DocumentRevised(document: Document, latest: Revision)
case class DocumentChanged(document: Document)

trait DocumentSubscriber extends CometActor {
}

trait DocumentServerComponent {
  val documentServer: DocumentServer
  trait DocumentServer extends Actor
}

trait DocumentServerComponentImpl extends DocumentServerComponent {
  val documentServer = new DocumentServer with Loggable {

  private var subscribers: List[DocumentSubscriber] = Nil
  def act() {
    loop {
      react {
        case Subscribe(subscriber) =>
          subscribers ::= subscriber
          subscriber ! Subscribed()
        case Unsubscribe(subscriber) =>
          subscribers = subscribers filterNot (_ == subscriber)
        case a @ DocumentAdded(d) =>
          logger.info(d.key + " added")
          distribute(a)
        case r @ DocumentRevised(d, _) =>
          logger.info(d.key + " revised")
          distribute(r)
        case c @ DocumentChanged(d) =>
          logger.info(d.key + " changed")
          distribute(c)
        case _ =>
      }
    }
  }

  private def distribute(msg: AnyRef) { subscribers.foreach(_ ! msg) }
}

}
