package vvv.docreg.backend

import scala.actors._
import scala.actors.Actor._
import net.liftweb.http._
import net.liftweb.actor._
import net.liftweb.util._
import vvv.docreg.model._

case class Subscribe(subscriber: DocumentSubscriber)
case class Subscribed()
case class Unsubscribe(subscriber: DocumentSubscriber)
case class DocumentAdded(document: Document)
case class DocumentRevised(document: Document)
case class DocumentNotice(document: Document)

trait DocumentSubscriber extends CometActor {
}

object DocumentServer extends Actor {
  private var subscribers: List[DocumentSubscriber] = Nil
  def act() {
    loop {
      react {
        case Subscribe(subscriber) =>
          subscribers ::= subscriber
          subscriber ! Subscribed()
        case Unsubscribe(subscriber) =>
          subscribers -= subscriber
        case DocumentAdded(document) =>
          subscribers.foreach(_ ! DocumentAdded(document))
        case _ =>
      }
    }
  }
}
