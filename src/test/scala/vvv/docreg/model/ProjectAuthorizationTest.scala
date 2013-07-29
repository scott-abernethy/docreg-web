/*
 * Copyright (c) 2013 Aviat Networks.
 * This file is part of DocReg+Web. Please refer to the NOTICE.txt file for license details.
 */

package vvv.docreg.model

import org.specs2.mutable._
import vvv.docreg.db.{TestDbScope}

class ProjectAuthorizationTest extends Specification {

  sequential

  "ProjectAuthorization Model" should {
    "give authorization where granted record exists" >> new TestDbScope {
      import org.squeryl.PrimitiveTypeMode._
      transaction{
        val (p1,p2,p3) = db.createProjects
        val (u1,u2) = db.createUsers
        ProjectAuthorization.grant(u1, p3)
        ProjectAuthorization.grant(u2, p1)
        ProjectAuthorization.authorizedFor_?(u1, p1) must beFalse
        ProjectAuthorization.authorizedFor_?(u1, p3) must beTrue
        ProjectAuthorization.authorizedFor_?(u2, p1) must beTrue
        ProjectAuthorization.authorizedFor_?(u2, p2) must beFalse
      }
    }

    "refuse authorization where no record exists" >> new TestDbScope {
      import org.squeryl.PrimitiveTypeMode._
      transaction{
        val (p1,p2,p3) = db.createProjects
        val (u1,u2) = db.createUsers
        ProjectAuthorization.authorizedFor_?(u1, p1) must beFalse
        ProjectAuthorization.authorizedFor_?(u2, p3) must beFalse
      }
    }

    "refuse authorization where revoked record exists" >> new TestDbScope {
      import org.squeryl.PrimitiveTypeMode._
      transaction{
        val (p1,p2,p3) = db.createProjects
        val (u1,u2) = db.createUsers
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
