package vvv.docreg.model

import org.specs._
import org.specs.runner.JUnit4
import org.specs.runner.ConsoleRunner
import org.specs.matcher._
import org.specs.specification._
import net.liftweb.mapper.By
import vvv.docreg.db.{TestDbVendor, DbVendor}

class UserProjectTestSpecsAsTest extends JUnit4(UserProjectTestSpecs)
object UserProjectTestSpecsRunner extends ConsoleRunner(UserProjectTestSpecs)

object UserProjectTestSpecs extends Specification {

  "UserProject Model" should {
    "find none where no UserProject record exists for the user" >> {
      TestDbVendor.initAndClean

      val otherU = User.create.name("other").email("other@msn.com").username("aaa")
      otherU.save
      val u = User.create.name("foo").email("foo@bar.com").username("bbb")
      u.save
      val p = Project.create.name("p1")
      p.save
      val p2 = Project.create.name("p2")
      p2.save
      UserProject.create.user(otherU).project(p)
      val x = UserProject.userSelected(u)
      x must haveSize(0)
    }
    "find some, based on the selected field value" >> {
      TestDbVendor.initAndClean

      val u = User.create.name("foo").email("foo@bar.com").username("aaa")
      u.save
      val p1 = Project.create.name("p1")
      p1.save
      val p2 = Project.create.name("p2")
      p2.save
      val p3 = Project.create.name("p3")
      p3.save
      UserProject.create.user(u).project(p1).selected(true).save
      UserProject.create.user(u).project(p2).selected(false).save
      UserProject.create.user(u).project(p3).selected(true).save

      val x = UserProject.userSelected(u)
      x must haveSize(2)
      x must haveSameElementsAs(p1 :: p3 :: Nil)
    }
    "update existing user project, or create one" >> {
      TestDbVendor.initAndClean

      val u = User.create.name("foo").email("foo@bar.com").username("aaa")
      u.save
      val p1 = Project.create.name("p1")
      p1.save

      UserProject.set(u, p1, true)
      var found: Seq[UserProject] = UserProject.findAll(By(UserProject.user, u), By(UserProject.project, p1))
      found must haveSize(1)
      found(0).user.toOption must beSome(u)
      found(0).project.toOption must beSome(p1)
      found(0).selected must beTrue

      UserProject.set(u, p1, true)
      found = UserProject.findAll(By(UserProject.user, u), By(UserProject.project, p1))
      found must haveSize(1)
      found(0).selected must beTrue

      UserProject.set(u, p1, false)
      found = UserProject.findAll(By(UserProject.user, u), By(UserProject.project, p1))
      found must haveSize(1)
      found(0).selected must beFalse
    }
    "provide all projects, sorted by name, marking selected" >> {
      TestDbVendor.initAndClean

      val (p1, p2, p3) = TestDbVendor.createProjects
      val (u, other) = TestDbVendor.createUsers

      UserProject.create.user(u).project(p2).selected(true).save
      UserProject.create.user(other).project(p3).selected(true).save
      UserProject.create.user(u).project(p1).selected(false).save

      val x = UserProject.listFor(u)
      x must haveSize(3)
      x(0)._1 must be_==(p1)
      x(0)._2 must beFalse
      x(1)._1 must be_==(p2)
      x(1)._2 must beTrue
      x(2)._1 must be_==(p3)
      x(2)._2 must beFalse
    }
  }

  override def doBeforeSpec(actions: => Any) {
  }
}
