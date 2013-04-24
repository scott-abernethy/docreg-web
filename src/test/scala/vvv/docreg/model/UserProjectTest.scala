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

import org.specs._
import org.specs.runner.JUnit4
import org.specs.runner.ConsoleRunner
import org.specs.matcher._
import org.specs.specification._
import net.liftweb.mapper.By
import vvv.docreg.db.{TestDbVendor, DbVendor}
import org.squeryl.PrimitiveTypeMode._

class UserProjectTest extends Specification {

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

      val (p1, p2, p3) = transaction( TestDbVendor.createProjects )
      val (u, other) = transaction( TestDbVendor.createUsers )

      transaction {
      val d1_p1 = new Document
      d1_p1.number = "0001"
      d1_p1.projectId = p1.id
      d1_p1.title = "Foo"
      Document.dbTable.insert(d1_p1)
      val d2_p1 = new Document
      d2_p1.number = "0002"
      d2_p1.projectId = p1.id
      d2_p1.title = "Bar"
      Document.dbTable.insert(d2_p1)
      val d3_p2 = new Document
      d3_p2.number = "0003"
      d3_p2.projectId = p2.id
      d3_p2.title = "Red"
      Document.dbTable.insert(d3_p2)
      val d4_p3 = new Document
      d4_p3.number = "0004"
      d4_p3.projectId = p3.id
      d4_p3.title = "Night"
      Document.dbTable.insert(d4_p3)

      UserProject.set(u,p2,true)
      UserProject.set(other,p3,true)
      UserProject.set(u,p1,false)

      val x = UserProject.listFor(Some(u), true)
      x must haveSize(3)
      x(0)._1 must be_==(p1)
      x(0)._2 must beFalse
      x(1)._1 must be_==(p2)
      x(1)._2 must beTrue
      x(2)._1 must be_==(p3)
      x(2)._2 must beFalse

      val y = UserProject.listFor(Some(u), false)
      y must haveSize(2)
      y(0)._1 must be_==(p1)
      y(0)._2 must beFalse
      y(1)._1 must be_==(p2)
      y(1)._2 must beTrue
      }

      transaction {
         Document.dbTable.deleteWhere(_.projectId === p2.id)
      }

      transaction{
      val z = UserProject.listFor(Some(u), false)
      z must haveSize(1)
      z(0)._1 must be_==(p1)
      z(0)._2 must beFalse
         
      val z_ = UserProject.listFor(Some(u), true)
      z_ must haveSize(2)
      z_(0)._1 must be_==(p1)
      z_(0)._2 must beFalse
      z_(1)._1 must be_==(p3)
      z_(1)._2 must beFalse
      }
    }
  }

  override def doBeforeSpec(actions: => Any) {
  }
}
