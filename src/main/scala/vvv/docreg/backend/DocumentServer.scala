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
case class DocumentRevised(document: Document, latest: Revision)
case class DocumentChanged(document: Document)

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
          subscribers = subscribers filterNot (_ == subscriber)
        case a @ DocumentAdded(d) =>
          println(d.key + " added")
          distribute(a)
        case r @ DocumentRevised(d, _) => 
          println(d.key + " revised")
          distribute(r)
        case c @ DocumentChanged(d) => 
          println(d.key + " changed")
          distribute(c)
        case _ =>
      }
    }
  }

  private def distribute(msg: AnyRef) { subscribers.foreach(_ ! msg) }
}
