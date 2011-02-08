package vvv.docreg.model

import net.liftweb._
import net.liftweb.common._
import mapper._

object Subscription extends Subscription with LongKeyedMetaMapper[Subscription] {

  def subscribe(d: Document, u: User) = this.create.document(d).user(u).save

  def unsubscribe(d: Document, u: User) = {
    forDocumentandUser(d, u) match {
      case Full(sub) =>
        sub.delete_!
        sub.save()
      case _ =>
        Nil
    }
  }

  def forDocumentandUser(document: Document, user: User): Box[Subscription] = {
    val ss = Subscription.findAll(By(Subscription.document, document.id), By(Subscription.user, user))
    if (ss nonEmpty) Full(ss head) else Empty
  }

  def forDocument(document: Document): List[Subscription] = {
    val ss = Subscription.findAll(By(Subscription.document, document.id))
    if (ss nonEmpty) ss else List.empty
  }

  def forUser(user: User): List[Subscription] = {
    val ss = Subscription.findAll(By(Subscription.user, user))
    if (ss nonEmpty) ss else List.empty
  }

}

class Subscription extends LongKeyedMapper[Subscription] with IdPK {
  def getSingleton = Subscription
  object document extends LongMappedMapper(this, Document)
  object user extends LongMappedMapper(this, User)
}