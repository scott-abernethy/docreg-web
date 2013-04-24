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

import org.specs._
import org.specs.mock.Mockito
import vvv.docreg.model.{Document, User}

class SubmitEngineTest extends Specification with Mockito {
   "SubmitEngine" should {
      "create register request" >> {
         val u = new User
         u.name = "Me Who"
         u.email = "meat@domain.co"
         u.username = "meat@domain.co"
         val x = SubmitEngine.registerRequest("proj", "Every1", "Good Name", "foo", u, "localhost", "v2")
         x.project must beEqual("proj")
         x.access must beEqual("Every1")
         x.fileName must beEqual("Good Name")
         x.comment must beEqual("foo")
         x.author must beEqual("Me Who")
         x.userName must beEqual("meat")
         x.clientHost must beEqual("localhost")
         x.clientVersion must beEqual("v2")
      }
      "correct empty comment" >> {
         val u = new User
         u.name = "Me Who"
         u.email = "me@domain.co"
         val x = SubmitEngine.registerRequest("proj", "Every1", "Good Name", "", u, "localhost", "v2")
         x.comment must beEqual("[no description]")
      }
      "remove multiple space in document name" >> {
         val u = new User
         val x = SubmitEngine.registerRequest("proj", "all", "Good  Name     Again", "foo", u, "localhost", "v2")
         x.fileName must beEqual("Good Name Again")
      }
      "remove bad characters from document name" >> {
         val u = new User
         val x = SubmitEngine.registerRequest("proj", "all", """This name: has ba:dness \in it""", "foo", u, "localhost", "v2")
         x.fileName must beEqual("This name has ba dness in it")
      }
   }
}
