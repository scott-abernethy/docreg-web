/*
 * Copyright (c) 2013 Scott Abernethy.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

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
