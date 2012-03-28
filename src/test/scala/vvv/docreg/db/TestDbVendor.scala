package vvv.docreg.db

import vvv.docreg.model._
import java.util.Date

trait TestDbVendor extends DbVendor {
  def initAndClean() {
    init()
    Project.bulkDelete_!!()
    User.bulkDelete_!!()
    UserProject.bulkDelete_!!()
    Document.bulkDelete_!!()
    Revision.bulkDelete_!!()
    UserLookup.bulkDelete_!!()
    Approval.bulkDelete_!!()
  }

  def createUsers = {
    val u = User.create.name("foo").email("foo@bar.com").username("aaa")
    u.save
    val other = User.create.name("other").email("other@msn.com").username("bbb")
    other.save
    (u, other)
  }
  
  def createProjects = {
    val p1 = Project.create.name("p1")
    p1.save
    val p3 = Project.create.name("p3")
    p3.save
    val p2 = Project.create.name("p2")
    p2.save
    (p1, p2, p3)
  }
  
  def createDocument(p: Project, author: User) =
  {
    val d: Document = Document.create.key("234").project(p).title("The Nameless City").access("Forbidden")
    d.save
    val r1 = Revision.create.document(d).version(1).filename("0234-001-foo.txt").author(author).date(new Date()).comment("first")
    r1.save
    val r2 = Revision.create.document(d).version(2).filename("0234-002-foo.txt").author(author).date(new Date()).comment("second")
    r2.save
    val r3 = Revision.create.document(d).version(3).filename("0234-003-foo.txt").author(author).date(new Date()).comment("third")
    r3.save
    (d, r1, r2, r3)
  }
}

object TestDbVendor extends TestDbVendor