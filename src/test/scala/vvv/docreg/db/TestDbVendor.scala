package vvv.docreg.db

import vvv.docreg.model._

trait TestDbVendor extends DbVendor {
  def initAndClean() {
    init()
    Project.bulkDelete_!!()
    User.bulkDelete_!!()
    UserProject.bulkDelete_!!()
  }

  def createUsers = {
    val u = User.create.name("foo").email("foo@bar.com")
    u.save
    val other = User.create.name("other").email("other@msn.com")
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
}

object TestDbVendor extends TestDbVendor