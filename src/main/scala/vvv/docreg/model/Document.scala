package vvv.docreg.model

import _root_.net.liftweb.mapper._
import _root_.net.liftweb.util._
import _root_.net.liftweb.common._
import scala.xml._
import vvv.docreg.util.StringUtil.{prePadTo, retitleFile}
import util.matching.Regex

class Document extends LongKeyedMapper[Document] with IdPK with ManyToMany {
  def getSingleton = Document

  object key extends MappedString(this, 20)
  object project extends MappedLongForeignKey(this, Project)
  object title extends MappedString(this, 200)
  object access extends MappedString(this, 128)
  object subscribers extends MappedManyToMany(Subscription, Subscription.document, Subscription.user, User)

  def revisions = Revision.forDocument(this)
  def revision(version: Long) = Revision.forDocument(this, version)

  def subscriber(user: User) = Subscription.forDocumentBy(this, user)

  def latest = if (revisions nonEmpty) revisions head else EmptyRevision
  def latest_?(version: Long): Boolean = {
    val r = latest
    r != null && r.version.is == version
  }
  def projectName: String = project.obj.map(_.name.is) openOr "?"
  def infoLink: String = "/" + key.is
  def nextVersion: Long = latest.version.toLong + 1L

  def nextFileName(userFileName: String): String =
  {
    prePadTo(key, 4, '0') +
      "-" +
      prePadTo(nextVersion.toString, 3, '0') +
      "-" +
      retitleFile(title, userFileName)
  }

  def editingFileName(username: String): String =
  {
    prePadTo(key, 4, '0') +
      "-" +
      prePadTo(nextVersion.toString, 3, '0') +
      "#" + username +
      "-" +
      retitleFile(title, latest.filename.is)
  }

  def linkForEditing(username: String): String =
  {
    infoLink + "/download/editing/" + username
  }

  def fullTitle: String = key.is + ": " + title.is

  def info(): NodeSeq = <a href={ infoLink }>{ fullTitle }</a>
}

object Document extends Document with LongKeyedMetaMapper[Document] {
  override def dbIndexes = UniqueIndex(key) :: super.dbIndexes

  override def fieldOrder = List(key, project, title)

  def forKey(key: String): Box[Document] = {
    val xs = findAll(By(Document.key, key))
    if (xs isEmpty) Empty else Full(xs head)
  }

  val ValidIdentifier: Regex = """^([0-9]+)(-[0-9]+)?$""".r
  val ValidDocumentFileName: Regex = """^([0-9]+)-([0-9]+)-(.+)$""".r

  def sort(a: Document, b: Document): Boolean = {
    a.key.is < b.key.is
  }
}

object FilteredDocument {
  import vvv.docreg.helper.ProjectSelection
  def search(request: String): List[Document] = searchLike(Document.title, "%" + request + "%")
  def searchLike(field: MappedField[String, Document], value: String): List[Document] = {
    if (ProjectSelection.showAll.is) {
      Document.findAll(
        Like(field, value),
        OrderBy(Document.id, Descending)
      )
    } else {
      val checked = ProjectSelection.projects.is.toList
      Document.findAll(
        Like(field, value),
        In(Document.project, Project.id, ByList(Project.id, checked.map( _.id.is))),
        OrderBy(Document.id, Descending)
      )
    }
  }
  def searchAuthor(value: String): List[Document] = {
    val users : List[User] = User.findAll(
        Like(User.name, "%" + value + "%"),
        OrderBy(User.name, Descending)
    )
    if (!ProjectSelection.showAll.is){
      val touchedRevisions = Revision.findAll(
//      In(Revision.author, User.id, In(User.name, User.name, Like(User.name, "%" + value + "%"))),
        In(Revision.author, User.id, ByList(User.id, users.map(_.id.is))),
        In(Revision.document, Document.id, In(Document.project, Project.id, ByList(Project.id, ProjectSelection.projects.is.map(_.id.is).toSeq)))
      )
      touchedRevisions.flatMap(_.document.obj).distinct
    }else{
      val touchedRevisions = Revision.findAll(
        In(Revision.author, User.id, ByList(User.id, users.map(_.id.is)))
      )
      touchedRevisions.flatMap(_.document.obj).distinct
    }
  }
}
