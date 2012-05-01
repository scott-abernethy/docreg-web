package vvv.docreg.model

import org.specs.Specification
import vvv.docreg.db.TestDbVendor
import org.squeryl.PrimitiveTypeMode._

class ProjectAuthorizationTest extends Specification {
  "ProjectAuthorization Model" should {
    "give authorization where granted record exists" >> {
      TestDbVendor.initAndClean()
      transaction{
        val (p1,p2,p3) = TestDbVendor.createProjects
        val (u1,u2) = TestDbVendor.createUsers
        ProjectAuthorization.grant(u1, p3)
        ProjectAuthorization.grant(u2, p1)
        ProjectAuthorization.authorizedFor_?(u1, p1) must beFalse
        ProjectAuthorization.authorizedFor_?(u1, p3) must beTrue
        ProjectAuthorization.authorizedFor_?(u2, p1) must beTrue
        ProjectAuthorization.authorizedFor_?(u2, p2) must beFalse
      }
    }

    "refuse authorization where no record exists" >> {
      TestDbVendor.initAndClean()
      transaction{
        val (p1,p2,p3) = TestDbVendor.createProjects
        val (u1,u2) = TestDbVendor.createUsers
        ProjectAuthorization.authorizedFor_?(u1, p1) must beFalse
        ProjectAuthorization.authorizedFor_?(u2, p3) must beFalse
      }
    }

    "refuse authorization where revoked record exists" >> {
      TestDbVendor.initAndClean()
      transaction{
        val (p1,p2,p3) = TestDbVendor.createProjects
        val (u1,u2) = TestDbVendor.createUsers
        ProjectAuthorization.grant(u1, p1)
        ProjectAuthorization.grant(u2, p2)
        ProjectAuthorization.authorizedFor_?(u1, p1) must beTrue
        ProjectAuthorization.revoke(u1, p1)
        ProjectAuthorization.authorizedFor_?(u1, p1) must beFalse
        ProjectAuthorization.authorizedFor_?(u2, p2) must beTrue
      }
    }
  }
}
