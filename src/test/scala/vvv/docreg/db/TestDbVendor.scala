package vvv.docreg.db

import vvv.docreg.model._
import java.util.Date
import org.squeryl.PrimitiveTypeMode._
import java.sql.Timestamp

trait TestDbVendor extends DbVendor {
  def initAndClean() {
    init()
    clear()
  }

  def createUsers = {
    val u = new User
    u.name = ("foo")
    u.email = ("foo@bar.com")
    u.username = ("aaa")
    val other = new User
    other.name = ("other")
    other.email = ("other@msn.com")
    other.username = ("bbb")
    (User.dbTable.insert(u), User.dbTable.insert(other))
  }
  
  def createProjects = {
    val p1 = new Project
    p1.name = ("p1")
    val p3 = new Project
    p3.name = ("p3")
    val p2 = new Project
    p2.name = ("p2")
    (Project.dbTable.insert(p1),Project.dbTable.insert(p2),Project.dbTable.insert(p3))
  }
  
  def createDocument(p: Project, author: User) =
  {
    var d: Document = new Document
    d.number = ("234")
    d.projectId = (p.id)
    d.title = ("The Nameless City")
    d.access = ("Forbidden")
    d = Document.dbTable.insert(d)
    val r1 = new Revision
    r1.documentId = (d.id)
    r1.version = (1)
    r1.filename = ("0234-001-foo.txt")
    r1.authorId = (author.id)
    r1.date = (new Timestamp(System.currentTimeMillis()))
    r1.comment = ("first")
    val r2 = new Revision
    r2.documentId = (d.id)
    r2.version = (2)
    r2.filename = ("0234-002-foo.txt")
    r2.authorId = (author.id)
    r2.date = (new Timestamp(System.currentTimeMillis()))
    r2.comment = ("second")
    val r3 = new Revision
    r3.documentId = (d.id)
    r3.version = (3)
    r3.filename = ("0234-003-foo.txt")
    r3.authorId = (author.id)
    r3.date = (new Timestamp(System.currentTimeMillis()))
    r3.comment = ("third")
    (d, Revision.dbTable.insert(r1), Revision.dbTable.insert(r2), Revision.dbTable.insert(r3))
  }
}

object TestDbVendor extends TestDbVendor