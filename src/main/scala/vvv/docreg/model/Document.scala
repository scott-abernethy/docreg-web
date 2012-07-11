package vvv.docreg.model

import _root_.net.liftweb.util._
import _root_.net.liftweb.common._
import scala.xml._
import vvv.docreg.util.StringUtil.{prePadTo, retitleFile}
import util.matching.Regex
import vvv.docreg.db.{DbSchema, DbObject}
import org.squeryl.PrimitiveTypeMode._
import org.squeryl.dsl.OneToMany

class Document extends DbObject[Document] {
  def dbTable = DbSchema.documents
  var number: String = ""
  var projectId: Long = 0
  var title: String = ""
  var access: String = ""

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

  def infoLink: String = "/" + number

  def nextVersion: Long = latest.version.toLong + 1L

  def nextFileName(newTitle: String, userFileName: String): String =
  {
    prePadTo(number, 4, '0') +
      "-" +
      prePadTo(nextVersion.toString, 3, '0') +
      "-" +
      retitleFile(newTitle, userFileName).getOrElse("")
  }

  def editingFileName(username: String): String =
  {
    prePadTo(number, 4, '0') +
      "-" +
      prePadTo(nextVersion.toString, 3, '0') +
      "#" + username +
      "-" +
      retitleFile(title, latest.filename).getOrElse("")
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

  def info(): NodeSeq = <a href={ infoLink }>{ fullTitle }</a>

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

  val ValidIdentifier: Regex = """^([0-9]+)(-[0-9]+)?$""".r
  val ValidDocumentFileName: Regex = """^([0-9]+)-([0-9]+)-(.+)$""".r
  val IdentifierAndExtension: Regex = """^([0-9]+)\..+$""".r

  def sort(a: Document, b: Document): Boolean = {
    a.number.toLong < b.number.toLong
  }

  object DocumentRef {
    def unapply(ref: String): Option[(String, Long)] = {
      ref match {
        case ValidIdentifier(num, null) => Some(num, Long.MaxValue)
        case ValidIdentifier(num, ver) => Some(num, ver.substring(1).toLong)
        case ValidDocumentFileName(num, ver, _) => Some(num,ver.toLong)
        case IdentifierAndExtension(num) => Some(num, Long.MaxValue)
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
  import vvv.docreg.helper.ProjectSelection

  def search(request: String): List[(Document,Project,Revision,User)] = {
    val start = System.currentTimeMillis()
    val result = if (request == null || request.isEmpty) searchAll() else searchFor(request)
    val sorted = result.sortWith{ (a,b) =>
      a._3.date.getTime > b._3.date.getTime
    }
    //println("Search took: " + (System.currentTimeMillis() - start))
    sorted
  }

  private def searchAll() = {
//    val checked = ProjectSelection.projects.is.map(_.id)
//    if (ProjectSelection.showAll.is) {
      inTransaction{
        join(Document.dbTable, Project.dbTable, Revision.dbTable, User.dbTable)( (d,p,r,u) =>
          select( (d,p,r,u) )
            orderBy(d.id asc, r.version desc)
            on(d.projectId === p.id, d.id === r.documentId, r.authorId === u.id)
        ).toList
      }.groupBy(_._1).flatMap(x => x._2.headOption).toList
//    }
//    else if (checked.isEmpty) {
//      Nil
//    }
//    else {
//      inTransaction{
//        join(Document.dbTable, Project.dbTable, Revision.dbTable, User.dbTable)( (d,p,r,u) =>
//          where(p.id in checked)
//          select( (d,p,r,u) )
//          orderBy(d.id asc, r.version desc)
//          on(d.projectId === p.id, d.id === r.documentId, r.authorId === u.id)
//        ).toList
//      }.groupBy(_._1).flatMap(x => x._2.headOption).toList
//    }
  }

  private def searchFor(request: String) = {
    val searchString: String = formatSearch(request)
    val number: String = prePadTo(request, 4, '0')
//    val checked = ProjectSelection.projects.is.map(_.id)
//    if (ProjectSelection.showAll.is) {
        inTransaction{
        join(Document.dbTable, Project.dbTable, Revision.dbTable, User.dbTable)( (d,p,r,u) =>
          where((d.title like searchString) or (u.name like searchString) or (d.number === number))
            select( (d,p,r,u) )
            orderBy(d.id asc, r.version desc)
            on(d.projectId === p.id, d.id === r.documentId, r.authorId === u.id)
        ).toList
      }.groupBy(_._1).flatMap(x => x._2.headOption).toList
//    }
//    else if (checked.isEmpty) {
//      Nil
//    }
//    else {
//      inTransaction{
//        join(Document.dbTable, Project.dbTable, Revision.dbTable, User.dbTable)( (d,p,r,u) =>
//          where(
//            ((d.title like searchString) or (u.name like searchString) or (d.number === number)) and
//            (p.id in checked)
//          )
//          select( (d,p,r,u) )
//          orderBy(d.id asc, r.version desc)
//          on(d.projectId === p.id, d.id === r.documentId, r.authorId === u.id)
//        ).toList
//      }.groupBy(_._1).flatMap(x => x._2.headOption).toList
//    }
  }

  def formatSearch(in: String): String =
  {
    val out: Option[String] = for {
      s <- Option(in)
      surrounded = "%" + s + "%"
    } yield surrounded.replaceAll("[ *]", "%").replaceAll("[%]+", "%")

    out.getOrElse("%")
  }
}
