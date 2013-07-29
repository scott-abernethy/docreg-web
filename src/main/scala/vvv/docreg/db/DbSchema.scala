/*
 * Copyright (c) 2013 Aviat Networks.
 * This file is part of DocReg+Web. Please refer to the NOTICE.txt file for license details.
 */

package vvv.docreg.db

import vvv.docreg.model._
import org.squeryl.PrimitiveTypeMode._
import org.squeryl.{Table, KeyedEntity}

trait DbObject[T <: KeyedEntity[Long]] extends KeyedEntity[Long] {
  var id: Long = 0
  def dbTable: Table[T]

  def reload(): Option[T] = {
    dbTable.lookup(id)
  }

  def lookup(id: Long): Option[T] = {
    dbTable.lookup(id)
  }
}

object DbSchema extends org.squeryl.Schema {
  val users = table[User]
  val userLookups = table[UserLookup]
  val projects = table[Project]
  val projectAuthorizations = table[ProjectAuthorization]
  val userProjects = table[UserProject]
  val documents = table[Document]
  val revisions = table[Revision]
  val pendings = table[Pending]
  val approvals = table[Approval]
  val subscriptions = table[Subscription]
  val tags = table[Tag]

  on(users)(u => declare(
    u.username is(indexed, unique, dbType("varchar(64)")),
    u.name is(indexed, dbType("varchar(64)")),
    u.email is(indexed, dbType("varchar(64)")),
    u.description is(dbType("varchar(256)")),
    u.department is(dbType("varchar(128)")),
    u.location is(dbType("varchar(128)")),
    u.host is(dbType("varchar(64)")),
    u.localServer is(dbType("varchar(64)")),
    u.timeZone is(dbType("varchar(32)"))
  ))

  on(userLookups)(u => declare(
    u.userId is (indexed),
    u.username is(indexed, dbType("varchar(64)")),
    u.name is(indexed, dbType("varchar(64)")),
    u.email is(indexed, dbType("varchar(64)")),
    columns(u.username, u.name, u.email) are(indexed, unique)
  ))

  on(projects)(p => declare(
    p.name is(indexed, unique, dbType("varchar(64)"))
  ))

  on(projectAuthorizations)(u => declare(
    u.projectId is(indexed),
    u.userId is(indexed),
    columns(u.projectId, u.userId) are(indexed, unique)
  ))

  on(userProjects)(u => declare(
    u.userId is(indexed),
    u.projectId is(indexed),
    columns(u.userId, u.projectId) are(indexed, unique)
  ))

  on(documents)(d => declare(
    d.key is(indexed, unique, dbType("varchar(10)")),
    d.title is(dbType("varchar(128)")),
    d.access is(dbType("varchar(128)")),
    d.reconciled is(dbType("datetime"))
  ))

  on(revisions)(r => declare(
    r.documentId is(indexed),
    columns(r.documentId, r.version) are (unique, indexed),
    r.filename is(dbType("varchar(128)")),
    r.comment is(dbType("varchar(512)")),
    r.clientVersion is(dbType("varchar(16)")),
    r.rawAuthor is(dbType("varchar(64)")),
    r.date is(dbType("datetime"))
  ))

  on(pendings)(p => declare(
    p.userId is(indexed),
    p.documentId is(indexed),
    columns(p.userId, p.documentId, p.action) are (unique, indexed)
  ))

  on(approvals)(a => declare(
    a.revisionId is(indexed),
    a.userId is(indexed),
    a.comment is(dbType("varchar(128)")),
    a.rawUser is(dbType("varchar(64)")),
    columns(a.revisionId, a.userId) are (unique, indexed)
  ))

  on(subscriptions)(s => declare(
    s.documentId is(indexed),
    s.userId is(indexed),
    columns(s.documentId, s.userId) are (unique, indexed)
  ))

  on(tags)(t => declare(
    t.name is(indexed, dbType("varchar(128)")),
    t.documentId is(indexed)
  ))

  val projectsToDocuments = oneToManyRelation(projects, documents).via( (p,d) => p.id === d.projectId)
  val projectsToAuthorizations = oneToManyRelation(projects, projectAuthorizations).via( (p,a) => p.id === a.projectId)
  val documentsToRevisions = oneToManyRelation(documents, revisions).via( (d,r) => d.id === r.documentId)
  val documentsToPendings = oneToManyRelation(documents, pendings).via( (d,p) => d.id === p.documentId)
  val usersToRevisions = oneToManyRelation(users, revisions).via( (u,r) => u.id === r.authorId)
  val usersToApprovals = oneToManyRelation(users, approvals).via( (u,a) => u.id === a.userId)
  val usersToPendings = oneToManyRelation(users, pendings).via( (u,p) => u.id === p.userId)
}
