/*
 * Copyright (c) 2013 Aviat Networks.
 * This file is part of DocReg+Web. Please refer to the NOTICE.txt file for license details.
 */

package vvv.docreg.backend

import org.specs2.mutable._
import org.specs2.mock._
import vvv.docreg.model.{Document, User}

class SubmitEngineTest extends Specification with Mockito {
   "SubmitEngine" should {

      "create register request" >> {
         val u = new User
         u.name = "Me Who"
         u.email = "meat@domain.co"
         u.username = "meat@domain.co"
         val x = SubmitEngine.registerRequest("proj", "Every1", "Good Name", "foo", u, "localhost", "v2")
         x.project must beEqualTo("proj")
         x.access must beEqualTo("Every1")
         x.fileName must beEqualTo("Good Name")
         x.comment must beEqualTo("foo")
         x.author must beEqualTo("Me Who")
         x.userName must beEqualTo("meat")
         x.clientHost must beEqualTo("localhost")
         x.clientVersion must beEqualTo("v2")
      }

      "correct empty comment" >> {
         val u = new User
         u.name = "Me Who"
         u.email = "me@domain.co"
         val x = SubmitEngine.registerRequest("proj", "Every1", "Good Name", "", u, "localhost", "v2")
         x.comment must beEqualTo("[no description]")
      }

      "remove multiple space in document name" >> {
         val u = new User
         val x = SubmitEngine.registerRequest("proj", "all", "Good  Name     Again", "foo", u, "localhost", "v2")
         x.fileName must beEqualTo("Good Name Again")
      }

      "remove bad characters from document name" >> {
         val u = new User
         val x = SubmitEngine.registerRequest("proj", "all", """This*? "'name: has<> /ba|:dness \in it""", "foo", u, "localhost", "v2")
         x.fileName must beEqualTo("This name has ba dness in it")
      }

      "shorten names that are too long" >> {
         val u = new User

         SubmitEngine.registerRequest("proj", "all", """123456789a123456789b123456789c123456789d123456789e123456789f1234""", "foo", u, "localhost", "v2").
           fileName must beEqualTo("123456789a123456789b123456789c123456789d123456789e123456789f1234")

         SubmitEngine.registerRequest("proj", "all", """123456789a123456789b123456789c123456789d123456789e123456789f1234X""", "foo", u, "localhost", "v2").
           fileName must beEqualTo("123456789a123456789b123456789c123456789d123456789e123456789f1234")

         SubmitEngine.registerRequest("proj", "all", """123456789a123456789b123456789c123456789d123456789e123456789f1234.ext""", "foo", u, "localhost", "v2").
           fileName must beEqualTo("123456789a123456789b123456789c123456789d123456789e123456789f1234.ext")

         SubmitEngine.registerRequest("proj", "all", """123456789a123456789b123456789c123456789d123456789e123456789f1234G.ext""", "foo", u, "localhost", "v2").
           fileName must beEqualTo("123456789a123456789b123456789c123456789d123456789e123456789f1234.ext")

         SubmitEngine.registerRequest("proj", "all", """1234-456-123456789a123456789b123456789c123456789d123456789e123456789f1234G.ext""", "foo", u, "localhost", "v2").
           fileName must beEqualTo("1234-456-123456789a123456789b123456789c123456789d123456789e123456789f1234.ext")
      }
   }
}
