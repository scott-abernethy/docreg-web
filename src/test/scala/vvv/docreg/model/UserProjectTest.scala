package vvv.docreg.model

import org.specs._
import org.specs.runner.JUnit4
import org.specs.runner.ConsoleRunner
import org.specs.matcher._
import org.specs.specification._
import net.liftweb.mapper.By
import vvv.docreg.db.{TestDbVendor, DbVendor}
import org.squeryl.PrimitiveTypeMode._

object UserProjectTest extends Specification {

  "UserProject Model" should {
    "find none where no UserProject record exists for the user" >> {
      TestDbVendor.initAndClean
      transaction{
      val (p1, p2, p3) = TestDbVendor.createProjects
      val (u1, u2) = TestDbVendor.createUsers

      val up = new UserProject
      up.userId = u1.id
      up.projectId = p1.id
      UserProject.dbTable.insert(up)

      val x = UserProject.userSelected(u2)
      x must haveSize(0)
      }
    }

    "find some, based on the selected field value" >> {
      TestDbVendor.initAndClean
      transaction {
      val (p1, p2, p3) = TestDbVendor.createProjects
      val (u1, u2) = TestDbVendor.createUsers

      val up1 = new UserProject
      up1.userId = u1.id
      up1.projectId = p1.id
      up1.selected = true
      UserProject.dbTable.insert(up1)
      val up2 = new UserProject
      up2.userId = u1.id
      up2.projectId = p2.id
      up2.selected = false
      UserProject.dbTable.insert(up2)
      val up3 = new UserProject
      up3.userId = u1.id
      up3.projectId = p3.id
      up3.selected = true
      UserProject.dbTable.insert(up3)

      val x = UserProject.userSelected(u1)
      x must haveSize(2)
      x must haveSameElementsAs(p1 :: p3 :: Nil)
      }
    }

    "update existing user project, or create one" >> {
      TestDbVendor.initAndClean
      transaction {
        val (p1, p2, p3) = TestDbVendor.createProjects
      val (u1, u2) = TestDbVendor.createUsers

      UserProject.set(u1, p1, true)
      var found = UserProject.find(u1, p1)
      found must haveSize(1)
      found.map(_.userId) must beSome(u1.id)
      found.map(_.projectId) must beSome(p1.id)
      found.map(_.selected) must beSome(true)

      UserProject.set(u1, p1, true)
      found = UserProject.find(u1, p1)
      found must haveSize(1)
      found.map(_.selected) must beSome(true)

      UserProject.set(u1, p1, false)
      found = UserProject.find(u1, p1)
      found must haveSize(1)
      found.map(_.selected) must beSome(false)
      }
    }

    "provide all projects, sorted by name, marking selected" >> {
      TestDbVendor.initAndClean

      transaction {
      val (p1, p2, p3) = TestDbVendor.createProjects
      val (u, other) = TestDbVendor.createUsers

      UserProject.set(u,p2,true)
      UserProject.set(other,p3,true)
      UserProject.set(u,p1,false)

      val x = UserProject.listFor(Some(u))
      x must haveSize(2)
      x(0)._1 must be_==(p1)
      x(0)._2 must beFalse
      x(1)._1 must be_==(p2)
      x(1)._2 must beTrue
      }
    }
  }

  override def doBeforeSpec(actions: => Any) {
  }
}
