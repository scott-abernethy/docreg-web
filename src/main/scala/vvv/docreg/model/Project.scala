/*
 * Copyright (c) 2013 Scott Abernethy.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
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
    <a href={ "/project/" + name.replaceAll(" ", "+") }>{ name }</a>
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
}
