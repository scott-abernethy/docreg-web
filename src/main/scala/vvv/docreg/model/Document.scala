/*
 * Copyright (c) 2013 Aviat Networks.
 * This file is part of DocReg+Web. Please refer to the NOTICE.txt file for license details.
 */

package vvv.docreg.model

import _root_.net.liftweb.util._
import _root_.net.liftweb.common._
import scala.xml._
import vvv.docreg.util.StringUtil.{prePadTo, retitleFile}
import util.matching.Regex
import vvv.docreg.db.{DbSchema, DbObject}
import org.squeryl.PrimitiveTypeMode._
import org.squeryl.dsl.OneToMany
import java.sql.Timestamp

class Document extends DbObject[Document] {
  def dbTable = DbSchema.documents
  var number: String = ""
  var projectId: Long = 0
  var title: String = ""
  var access: String = ""
  var reconciled: Timestamp = new Timestamp(0)

  def key(): String = number

  def revision(version: Long) = Revision.forDocument(this, version)

  def revisions(): List[Revision] = {
    inTransaction{
      from(Revision.dbTable)(r =>
        where(r.documentId === id)
          select(r)
          orderBy(r.version desc)
      ).toList
    }
  }

  def project(): Option[Project] = {
    inTransaction( DbSchema.projectsToDocuments.right(this).headOption )
  }

  def secure_?(): Boolean = access equalsIgnoreCase "Secure"

  def accessIcon(): NodeSeq = {
    if (secure_?()) {
      <abbr title="Secure Document"><i class="icon-lock"></i></abbr>
    } else {
      <abbr title="Public Document"><i class="icon-book"></i></abbr>
    }
  }

  def allows(user: User): Boolean = {
    secure_?() match {
      case true => ProjectAuthorization.authorizedFor_?(user.id, projectId)
      case open => true
    }
  }

  def latest: Revision = {
    inTransaction{
      from(Revision.dbTable)(r =>
        where(r.documentId === id)
        select(r)
        orderBy(r.version desc)
      ).headOption
    }.getOrElse(EmptyRevision)
  }

  def latest_?(version: Long): Boolean = {
    val r = latest
    r != null && r.version == version
  }

  def infoLink: String = infoHref

  def nextVersion: Long = latest.version.toLong + 1L

  def nextFileName(newTitle: String, userFileName: String): String =
  {
    prePadTo(number, 4, '0') +
      "-" +
      prePadTo(nextVersion.toString, 3, '0') +
      "-" +
      retitleFile(newTitle, userFileName).getOrElse(number)
  }

  def editingFileName(username: String): String =
  {
    prePadTo(number, 4, '0') +
      "-" +
      prePadTo(nextVersion.toString, 3, '0') +
      "#" + username +
      "-" +
      retitleFile(title, latest.filename).getOrElse(number)
  }

  def keyAndVersion(version: Long) = {
    number + "-" + version
  }

  def infoHref(): String = {
    "/" + number
  }

  def infoHref(version: Long): String = {
    "/" + keyAndVersion(version)
  }
  
  def info(): NodeSeq = <a href={ infoHref() }>{ fullTitle }</a>
  
  def info(version: Long): NodeSeq = <a href={ infoHref(version) }>{ fullTitle }</a>

  def downloadHref(): String = {
    infoHref() + "/download"
  }

  def downloadHref(version: Long): String = {
    infoHref(version) + "/download"
  }

  def approveHref(version: Long): String = {
    infoHref(version) + "/approve"
  }

  def requestApprovalHref(version: Long): String = {
    infoHref(version) + "/request-approval"
  }

  def submitHref(): String = {
    infoHref() + "/submit"
  }

  def editHref(): String = {
    infoHref() + "/edit"
  }

  def linkForEditing(): String =
  {
    infoLink + "/download/editing"
  }

  def fullTitle: String = number + ": " + title

  def contributors(): List[User] = {
    inTransaction{
      join(Document.dbTable, Revision.dbTable, User.dbTable)( (d,r,u) =>
        where(d.id === id)
          select(u)
          orderBy(u.email)
          on(d.id === r.documentId, r.authorId === u.id)
      ).distinct.toList
    }
  }
}

object Document extends Document {
  def forKey(key: String): Box[Document] = {
    inTransaction( Document.dbTable.where(d => d.number === key).headOption )
  }

  // todo make these private and use unapply-ers that also toUpperCase
  val ValidIdentifier: Regex = """^([0-9]{1,4}|[a-zA-Z][0-9]{3})(-[0-9]+)?$""".r
  val ValidDocumentFileName: Regex = """^([0-9]{1,4}|[a-zA-Z][0-9]{3})-([0-9]+)-(.+)$""".r
  val IdentifierAndExtension: Regex = """^([0-9]{1,4}|[a-zA-Z][0-9]{3})\..+$""".r
  val IdentifierAndFilename: Regex = """^([0-9]{1,4}|[a-zA-Z][0-9]{3})(-[0-9]+)?[\.-].+$""".r

  def sort(a: Document, b: Document): Boolean = {
    a.number.toLong < b.number.toLong
  }

  object DocumentRef {
    def unapply(ref: String): Option[(String, Long)] = {
      ref match {
        case ValidIdentifier(num, null) => Some(num.toUpperCase, Long.MaxValue)
        case ValidIdentifier(num, ver) => Some(num.toUpperCase, ver.substring(1).toLong)
        case ValidDocumentFileName(num, ver, _) => Some(num.toUpperCase, ver.toLong)
        case IdentifierAndFilename(num, null) => Some(num.toUpperCase, Long.MaxValue)
        case IdentifierAndFilename(num, ver) => Some(num.toUpperCase, ver.substring(1).toLong)
        case _ => None
      }
    }
  }

  object DocumentRevision {
    def unapply(ref: String): Option[(Document, Revision)] = {
      for {
        (key, version) <- DocumentRef.unapply(ref)
        d <- Document.forKey(key).toOption
        r <- Revision.forDocument(d, version)
      } yield (d, r)
    }
  }
}

object FilteredDocument
{
  lazy val searchLimit = 1000

  def search(request: String): List[(Document,Project,Revision,User)] = {
     (request match {
        case text if (text == null || text.isEmpty) => searchAll()
        case text => searchTerms(text)
     }).sortWith( (a,b) => a._3.date.getTime > b._3.date.getTime )
   }

  private def searchAll(): List[(Document,Project,Revision,User)] = {
     inTransaction(
        join(
           Document.dbTable,
           Project.dbTable,
           Revision.dbTable,
           User.dbTable,
           from(Revision.dbTable)(r2 => groupBy(r2.documentId) compute(max(r2.version)))
        ) ( (d, p, r, u, rMax) =>
           where(r.version === rMax.measures)
              select ((d, p, r, u))
              orderBy (r.date desc)
              on(d.projectId === p.id, d.id === r.documentId, r.authorId === u.id, rMax.key === d.id)
        ).
           page(0, searchLimit).
           toList
     )
  }

  private def searchTerms(request: String): List[(Document,Project,Revision,User)] = {
    // for each term
    // search
    request.split(" ").toList.
      map(_.trim).
      filter(_.size > 0).
      map(searchFor _). // List[Set[Long]]
      reduceOption(_ & _). // Option[Set[Long]]
      map(fill(_, searchLimit)).
      getOrElse(Nil)
  }

  private def searchFor(request: String): Set[Long] = {
    def searching(textSearch: String, numberSearch: String) = inTransaction {
       join(Document.dbTable, Project.dbTable, Revision.dbTable, User.dbTable)( (d,p,r,u) =>
          where((d.title like textSearch) or (u.name like textSearch) or (d.number === numberSearch) or (r.comment like textSearch) or (p.name like textSearch))
             select( d.id )
             on(d.projectId === p.id, d.id === r.documentId, r.authorId === u.id)
       ).toList.toSet
    }
    searching(formatSearch(request), prePadTo(request, 4, '0'))
  }

  def formatSearch(in: String): String = {
    def formatted(x: String): Option[String] = for {
      s <- Option(x)
      surrounded = "%" + s + "%"
    } yield surrounded.replaceAll("[ *]", "%").replaceAll("[%]+", "%")

    formatted(in).getOrElse("%")
  }

   def fill(ids: Set[Long], limit: Int): List[(Document, Project, Revision, User)] = ids.toSeq match {
      case Nil => {
         Nil
      }
      case xs => {
         inTransaction(
            join(
               Document.dbTable,
               Project.dbTable,
               Revision.dbTable,
               User.dbTable,
               from(Revision.dbTable)(r2 => where(r2.documentId in xs) groupBy(r2.documentId) compute(max(r2.version)))
            ) ( (d, p, r, u, rMax) =>
               where((d.id in xs) and (r.version === rMax.measures))
               select ((d, p, r, u))
               orderBy (r.date desc)
               on(d.projectId === p.id, d.id === r.documentId, r.authorId === u.id, rMax.key === d.id)
            ).
               page(0, limit).
               toList
         )
      }
   }
}
