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
    infoHref() + "/approve"
  }

  def editHref(): String = {
    infoHref() + "/edit"
  }

  def linkForEditing(username: String): String =
  {
    infoLink + "/download/editing/" + username
  }

  def fullTitle: String = number + ": " + title

  def info(): NodeSeq = <a href={ infoLink }>{ fullTitle }</a>
}

object Document extends Document {
  def forKey(key: String): Box[Document] = {
    inTransaction( Document.dbTable.where(d => d.number === key).headOption )
  }

  val ValidIdentifier: Regex = """^([0-9]+)(-[0-9]+)?$""".r
  val ValidDocumentFileName: Regex = """^([0-9]+)-([0-9]+)-(.+)$""".r

  def sort(a: Document, b: Document): Boolean = {
    a.number.toLong < b.number.toLong
  }
}

object FilteredDocument
{
  import vvv.docreg.helper.ProjectSelection

  def search(request: String): List[(Document,Project,Revision,User)] = {
    // TODO project selection!
    val start = System.currentTimeMillis()
    val result = if (request == null || request.isEmpty) {
      inTransaction{
        join(Document.dbTable, Project.dbTable, Revision.dbTable, User.dbTable)( (d,p,r,u) =>
          select( (d,p,r,u) )
          orderBy(d.id asc, r.version desc)
          on(d.projectId === p.id, d.id === r.documentId, r.authorId === u.id)
        ).toList
      }.groupBy(_._1).flatMap(x => x._2.headOption).toList
    }
    else {
      val searchString: String = formatSearch(request)
      val number: String = prePadTo(request, 4, '0')
      inTransaction{
        join(Document.dbTable, Project.dbTable, Revision.dbTable, User.dbTable)( (d,p,r,u) =>
          where((d.title like searchString) or (u.name like searchString) or (d.number === number))
          select( (d,p,r,u) )
          orderBy(d.id asc, r.version desc)
          on(d.projectId === p.id, d.id === r.documentId, r.authorId === u.id)
        ).toList
      }.groupBy(_._1).flatMap(x => x._2.headOption).toList
    }
    val sorted = result.sortWith{ (a,b) =>
      a._3.date.getTime > b._3.date.getTime
    }
    println("Search took: " + (System.currentTimeMillis() - start))
    sorted
  }

//  def search(request: String): List[Document] =
//  {
//    if (request == null || request.isEmpty)
//    {
//      all()
//    }
//    else
//    {
//      val search: String = formatSearch(request)
//      (
//        searchLike(Document.title, search) :::
//        searchAuthor(search) :::
//        searchLike(Document.key, prePadTo(request, 4, '0'))
//      ).distinct
//    }
//  }

  def formatSearch(in: String): String =
  {
    val out: Option[String] = for {
      s <- Option(in)
      surrounded = "%" + s + "%"
    } yield surrounded.replaceAll("[ *]", "%").replaceAll("[%]+", "%")

    out.getOrElse("%")
  }

//  def all(): List[Document] =
//  {
//    if (ProjectSelection.showAll.is) {
//      Document.findAll(
//        OrderBy(Document.id, Descending),
//        PreCache(Document.project)
//      )
//    } else {
//      val checked = ProjectSelection.projects.is.toList
//      Document.findAll(
//        In(Document.project, Project.id, ByList(Project.id, checked.map( _.id.is))),
//        OrderBy(Document.id, Descending)
//      )
//    }
//  }
//
//  def searchLike(field: MappedField[String, Document], value: String): List[Document] =
//  {
//    if (ProjectSelection.showAll.is) {
//      Document.findAll(
//        Like(field, value),
//        OrderBy(Document.id, Descending),
//        PreCache(Document.project)
//      )
//    } else {
//      val checked = ProjectSelection.projects.is.toList
//      Document.findAll(
//        Like(field, value),
//        In(Document.project, Project.id, ByList(Project.id, checked.map( _.id.is))),
//        OrderBy(Document.id, Descending),
//        PreCache(Document.project)
//      )
//    }
//  }
//
//  def searchAuthor(value: String): List[Document] =
//  {
//    val users : List[User] = User.findAll(
//        Like(User.name, "%" + value + "%"),
//        OrderBy(User.name, Descending)
//    )
//    if (!ProjectSelection.showAll.is){
//      val touchedRevisions = Revision.findAll(
////      In(Revision.author, User.id, In(User.name, User.name, Like(User.name, "%" + value + "%"))),
//        In(Revision.author, User.id, ByList(User.id, users.map(_.id.is))),
//        In(Revision.document, Document.id, In(Document.project, Project.id, ByList(Project.id, ProjectSelection.projects.is.map(_.id.is).toSeq)))
//      )
//      touchedRevisions.flatMap(_.document.obj).distinct
//    }else{
//      val touchedRevisions = Revision.findAll(
//        In(Revision.author, User.id, ByList(User.id, users.map(_.id.is)))
//      )
//      touchedRevisions.flatMap(_.document.obj).distinct
//    }
//  }
}
