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

package vvv.docreg.backend

import org.specs.runner.{ConsoleRunner, JUnit4}
import org.specs.Specification
import net.liftweb.common.{Full, Empty, Failure}

class UserMigrationTestSpecs extends Specification {
  "UserMigration" should {
    "reject non emails" >> {
      val x = UserMigration
      x.migrateEmail("foo") must be_==(Failure("Email 'foo' is not valid"))
      x.migrateEmail("foo.bar@") must be_==(Failure("Email 'foo.bar@' is not valid"))
      x.migrateEmail("foo.bar@com") must be_==(Failure("Email 'foo.bar@com' is not valid"))
      x.migrateEmail("foo.bar@aviatnet.com@aviatnet.com") must be_==(Failure("Email 'foo.bar@aviatnet.com@aviatnet.com' is not valid"))
    }
    "highlight unmigrateable user emails" >> {
      val x = UserMigration
      x.migrateEmail("xbostjan.prevodnik@msn.com") must be_==(Failure("Migration for 'xbostjan.prevodnik@msn.com' is not supported"))
      x.migrateEmail("fred.foo@welly.govt.nz") must be_==(Failure("Migration for 'fred.foo@welly.govt.nz' is not supported"))
    }
    "skip migrations for valid aviatnet emails" >> {
      val x = UserMigration
      x.migrateEmail("xbostjan.prevodnik@aviatnet.com") must be_==(Empty)
      x.migrateEmail("j.crescencio.flores@aviatnet.com") must be_==(Empty)
      x.migrateEmail("greg.martin-sj@aviatnet.com") must be_==(Empty)
      x.migrateEmail("scott.abernethy@aviatnet.com") must be_==(Empty)
    }
    "fix old emails" >> {
      val x = UserMigration
      x.migrateEmail("steve_webb@stratexnet.com") must be_==(Full("steve.webb@aviatnet.com"))
      x.migrateEmail("andrew.bridger@hstx.com") must be_==(Full("andrew.bridger@aviatnet.com"))
      x.migrateEmail("scott.abernethy@hstx.com") must be_==(Full("scott.abernethy@aviatnet.com"))
      x.migrateEmail("scott.abernethy@stratexnet.com") must be_==(Full("scott.abernethy@aviatnet.com"))
      x.migrateEmail("scott_abernethy@aviatnet.com") must be_==(Full("scott.abernethy@aviatnet.com"))
    }
    "fix domain name user emails" >> {
      val x = UserMigration
      x.migrateEmail("dantliff@aviatnet.com") must be_==(Full("david.antliff@aviatnet.com"))
      x.migrateEmail("swwang@hstx.com") must be_==(Full("shiwen.wang@aviatnet.com"))
      x.migrateEmail("sreddy@hstx.com") must be_==(Full("srinivasa.reddy@aviatnet.com"))
      x.migrateEmail("bnishida@hstx.com") must be_==(Full("brad.nishida@aviatnet.com"))
      x.migrateEmail("gmcilroy@hstx.com") must be_==(Full("guy.mcilroy@aviatnet.com"))
      x.migrateEmail("rmatian@hstx.com") must be_==(Full("roland.matian@aviatnet.com"))
      x.migrateEmail("dlangdale-hunt@hstx.com") must be_==(Full("dean.langdale-hunt@aviatnet.com"))
      x.migrateEmail("svarin@hstx.com") must be_==(Full("stephane.varin@aviatnet.com"))
      x.migrateEmail("aelola@hstx.com") must be_==(Full("arthur.elola@aviatnet.com"))
      x.migrateEmail("nkrishnamurthy@aviatnet.com") must be_==(Full("narayana.krishnamurthy@aviatnet.com"))
      x.migrateEmail("dhunt@aviatnet.com") must be_==(Full("darran.hunt@aviatnet.com"))
      x.migrateEmail("rwidjajakusuma@aviatnet.com") must be_==(Full("ricardo.widjajakusuma@aviatnet.com"))
      x.migrateEmail("hbui@aviatnet.com") must be_==(Full("hoang.bui@aviatnet.com"))
    }
    "fix prashanths email" >> {
      val x = UserMigration
      x.migrateEmail("prashanth.shitikond@hstx.com") must be_==(Full("prashanth.sitikond@aviatnet.com"))
    }
//    "fix the greg martins" >> {
//      val x = UserMigration
//    }
    "fix the martim asprey" >> {
      val x = UserMigration
      x.migrateEmail("martim.asprey@aviatnet.com") must be_==(Full("martin.asprey@aviatnet.com"))
    }
  }
}