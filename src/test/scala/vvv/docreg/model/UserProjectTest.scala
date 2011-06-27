package vvv.docreg.model

import org.specs._
import org.specs.runner.JUnit4
import org.specs.runner.ConsoleRunner
import org.specs.matcher._
import org.specs.specification._
import vvv.docreg.db.DbVendor

class UserProjectTestSpecsAsTest extends JUnit4(UserProjectTestSpecs)
object UserProjectTestSpecsRunner extends ConsoleRunner(UserProjectTestSpecs)

object UserProjectTestSpecs extends Specification {

  "UserProject Model" should {
    "find none where no UserProject record exists for the user" >> {
      DbVendor.init
      Project.bulkDelete_!!()
      User.bulkDelete_!!()
      UserProject.bulkDelete_!!()
      val otherU = User.create.name("other").email("other@msn.com")
      otherU.save
      val u = User.create.name("foo").email("foo@bar.com")
      u.save
      val p = Project.create.name("p1")
      p.save
      UserProject.create.user(otherU).project(p)
      val x = UserProject.userSelected(u)
      x must haveSize(0)
    }
    "find some, based on the selected field value" >> {
      DbVendor.init
      Project.bulkDelete_!!()
      User.bulkDelete_!!()
      UserProject.bulkDelete_!!()
      val u = User.create.name("foo").email("foo@bar.com")
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
  }

  override def doBeforeSpec(actions: => Any) {
  }
}
