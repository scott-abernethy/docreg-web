/*
 * Copyright (c) 2013 Scott Abernethy.
 * This file is part of DocReg+Web. Please refer to the NOTICE.txt file for license details.
 */

package vvv.docreg.model

import vvv.docreg.db.{DbObject, DbSchema}
import org.squeryl.PrimitiveTypeMode._

class Subscription extends DbObject[Subscription] {
  def dbTable = DbSchema.subscriptions
  var documentId: Long = 0
  var userId: Long = 0
  var notification: Boolean = false // if true sets the "always" option
  var bookmark: Boolean = false // if true sets the "bookmark" option
}

object Subscription extends Subscription {

  /** @deprecated */
  def subscribe(d: Document, u: User) {
    addNotification(d, u)
  }

  def addNotification(d: Document, u: User) {
    inTransaction {
      from(dbTable)(s => where(s.documentId === d.id and s.userId === u.id) select(s)).headOption match {
        case Some(s) if (!s.notification) => {
          s.notification = true
          dbTable.update(s)
        }
        case None => {
          val s = new Subscription
          s.documentId = d.id
          s.userId = u.id
          s.notification = true
          s.bookmark = false
          dbTable.insert(s)
        }
        case _ => {} // No change
      }
    }
  }

  def addBookmark(d: Document, u: User) = {
    inTransaction {
      from(dbTable)(s => where(s.documentId === d.id and s.userId === u.id) select(s)).headOption match {
        case Some(s) if (!s.bookmark) => {
          s.bookmark = true
          dbTable.update(s)
        }
        case None => {
          val s = new Subscription
          s.documentId = d.id
          s.userId = u.id
          s.notification = false
          s.bookmark = true
          dbTable.insert(s)
        }
        case _ => {} // No change
      }
    }
  }

  def removeNotification(d: Document, u: User) = {
    inTransaction {
      from(dbTable)(s => where(s.documentId === d.id and s.userId === u.id) select(s)).headOption match {
        case Some(s) if (s.notification && s.bookmark) => {
          s.notification = false
          dbTable.update(s)
        }
        case Some(s) if (s.notification) => {
          dbTable.deleteWhere(s => s.documentId === d.id and s.userId === u.id)
        }
        case _ => {} // No change
      }
    }
  }

  def removeBookmark(d: Document, u: User) = {
    inTransaction {
      from(dbTable)(s => where(s.documentId === d.id and s.userId === u.id) select(s)).headOption match {
        case Some(s) if (s.bookmark && s.notification) => {
          s.bookmark = false
          dbTable.update(s)
        }
        case Some(s) if (s.bookmark) => {
          dbTable.deleteWhere(s => s.documentId === d.id and s.userId === u.id)
        }
        case _ => {} // No change
      }
    }
  }

  def isNotified(did: Long, uid: Long): Boolean = {
    inTransaction {
      from(dbTable)(s => where(s.documentId === did and s.userId === uid and s.notification === true) select(s)).headOption.isDefined
    }
  }

  def isBookmarked(did: Long, uid: Long): Boolean = {
    inTransaction {
      from(dbTable)(s => where(s.documentId === did and s.userId === uid and s.bookmark === true) select(s)).headOption.isDefined
    }
  }

  def optionsFor(d: Document, u: User): Option[String] = {
    inTransaction {
      from(dbTable)(s => where(s.documentId === d.id and s.userId === u.id) select(s)).headOption match {
        case Some(s) if (s.notification && s.bookmark) => {
          Some("always bookmark")
        }
        case Some(s) if (s.notification) => {
          Some("always")
        }
        case Some(s) if (s.bookmark) => {
          Some("bookmark")
        }
        case _ => None
      }
    }
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

  def watchingFor(user: User): List[Document] = {
    inTransaction{
      join(Subscription.dbTable, Document.dbTable)( (s,d) =>
        where(s.userId === user.id and s.notification === true)
        select(d)
        orderBy(d.key asc)
        on(s.documentId === d.id)
      ).toList
    }
  }

  def watchersFor(document: Document): List[User] = {
    inTransaction{
      join(Subscription.dbTable, User.dbTable)( (s,u) =>
        where(s.documentId === document.id and s.notification === true)
          select(u)
          orderBy(u.email asc)
          on(s.userId === u.id)
      ).toList
    }
  }

  def bookmarksFor(user: User): List[Document] = {
    inTransaction{
      join(Subscription.dbTable, Document.dbTable)( (s,d) =>
        where(s.userId === user.id and s.bookmark === true)
          select(d)
          orderBy(d.key asc)
          on(s.documentId === d.id)
      ).toList
    }
  }

  def forDocument(document: Document): List[Subscription] = {
    inTransaction( from(dbTable)(s => where(s.documentId === document.id) select(s)).toList )
  }
}