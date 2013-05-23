/*
 * Copyright (c) 2013 Scott Abernethy.
 * This file is part of DocReg+Web. Please refer to the NOTICE.txt file for license details.
 */

package vvv.docreg.model

import xml.NodeSeq
import vvv.docreg.db.{DbSchema, DbObject}
import org.squeryl.PrimitiveTypeMode._
import org.squeryl.dsl.OneToMany
import org.squeryl.Query

class Project extends DbObject[Project] {
  def dbTable = DbSchema.projects
  var name: String = ""

  def infoLink(): NodeSeq = {
    <a href={ url }>{ name }</a>
  }


  def url: String = {
    "/project/" + name.replaceAll(" ", "+")
  }

  lazy val documentsQuery: OneToMany[Document] = DbSchema.projectsToDocuments.left(this)

  def documents(): List[Document] = {
    inTransaction{
      from(Document.dbTable)(d =>
        where(d.projectId === id)
        select(d)
        orderBy(d.number)
      ).toList
    }
  }

  def contributors(): List[User] = {
    inTransaction{
      join(Document.dbTable, Revision.dbTable, User.dbTable)( (d,r,u) =>
        where(d.projectId === id)
          select(u)
          orderBy(u.email)
          on(d.id === r.documentId, r.authorId === u.id)
      ).distinct.toList
    }
  }

  def authorized(): List[User] = {
    ProjectAuthorization.authorizedUsersFor(id)
  }
}

object Project extends Project {
  def forName(name: String): Option[Project] = {
    inTransaction{
      from(DbSchema.projects)( p =>
        where(p.name === name)
        select(p)
        orderBy(p.name asc)
      ).headOption
    }
  }

  def findAll(): List[Project] = {
    inTransaction{
      from(DbSchema.projects)( p =>
        select(p)
        orderBy(p.name asc)
      ).toList
    }
  }

  def findAllUsed(): List[Project] = {
    inTransaction {
      join(DbSchema.projects, DbSchema.documents.leftOuter)( (p, d) =>
        where(d.map(_.id).~.isNotNull)
        select( p )
        orderBy(p.name asc)
        on(p.id === d.map(_.projectId))
      ).toList.distinct
    }
  }

  def recentChanges(projectId: Long, limit: Int = 100): List[(Document,Revision,User)] = {
    inTransaction {
      join(DbSchema.revisions, DbSchema.documents, DbSchema.users)( (r,d,u) =>
        where(d.projectId === projectId)
        select( (d,r,u) )
        orderBy(r.date desc, r.id desc)
        on(r.documentId === d.id, r.authorId === u.id)
      ).page(0, limit).toList
    }
  }

}

object ValidProject {
  def unapply(name: String): Option[Project] = {
    Project.forName(name)
  }
}
