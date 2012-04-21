package vvv.docreg.model

import net.liftweb._
import net.liftweb.common._
import vvv.docreg.db.{DbObject, DbSchema}
import org.squeryl.PrimitiveTypeMode._

class Subscription extends DbObject[Subscription] {
  def dbTable = DbSchema.subscriptions
  var documentId: Long = 0
  var userId: Long = 0
}

object Subscription extends Subscription {

  def subscribe(d: Document, u: User) {
    val s = new Subscription
    s.documentId = d.id
    s.userId = u.id
    inTransaction( dbTable.insert(s) )
  }

  def unsubscribe(d: Document, u: User) {
    inTransaction( dbTable.deleteWhere(s => s.documentId === d.id and s.userId === u.id) )
  }

//  def forDocumentBy(document: Document, user: User): Box[Subscription] = {
//    val ss = Subscription.findAll(By(Subscription.document, document.id), By(Subscription.user, user))
//    if (ss nonEmpty) Full(ss head) else Empty
//  }
//
//  def forDocument(document: Document): List[Subscription] = {
//    val ss = Subscription.findAll(By(Subscription.document, document.id))
//    if (ss nonEmpty) ss else List.empty
//  }
//
//  def forUser(user: User): List[Subscription] = {
//    val ss = Subscription.findAll(By(Subscription.user, user))
//    if (ss nonEmpty) ss else List.empty
//  }

  def documentsForUser(user: User): List[Document] = {
    inTransaction{
      join(Subscription.dbTable, Document.dbTable)( (s,d) =>
        where(s.userId === user.id)
        select(d)
        orderBy(d.key asc)
        on(s.documentId === d.id)
      ).toList
    }
  }

  def usersFor(document: Document): List[User] = {
    inTransaction{
      join(Subscription.dbTable, User.dbTable)( (s,u) =>
        where(s.documentId === document.id)
          select(u)
          orderBy(u.email asc)
          on(s.userId === u.id)
      ).toList
    }
  }

  def forDocument(document: Document): List[Subscription] = {
    inTransaction( from(dbTable)(s => where(s.documentId === document.id) select(s)).toList )
  }
}