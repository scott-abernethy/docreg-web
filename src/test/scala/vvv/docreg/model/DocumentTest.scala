package vvv.docreg.model

import org.specs._
import java.util.Date
import vvv.docreg.db.TestDbVendor

object DocumentTest extends Specification {
  "Document Model" should {
    "next version is 1 if no revisions" >> {
      TestDbVendor.initAndClean()
      val p = Project.create.name("p")
      p.save
      val d = Document.create.key("999").project(p).title("hellow world").access("all")
      d.save

      d.nextVersion must be_==(1)
    }

    "create next version file name" in {
      TestDbVendor.initAndClean()
      val (u1, u2) = TestDbVendor.createUsers
      val p = Project.create.name("Cthulhu")
      p.save
      val d: Document = Document.create.key("234").project(p).title("The Nameless City").access("Forbidden")
      d.save
      val r4 = Revision.create.document(d).version(4).filename("foo.txt").author(u2).date(new Date()).comment("hmmm")
      r4.save

      d.nextFileName("Rainbow Fish", "youyoui.odt") must be_==("0234-005-Rainbow Fish.odt")
    }

    "check no file extension" in {
      TestDbVendor.initAndClean()
      val (u1, u2) = TestDbVendor.createUsers
      val p = Project.create.name("Cthulhu")
      p.save
      val d: Document = Document.create.key("234").project(p).title("The Nameless City").access("Forbidden")
      d.save
      val r4 = Revision.create.document(d).version(4).filename("foo.txt").author(u2).date(new Date()).comment("hmmm")
      r4.save

      d.nextFileName("The Nameless City", "youyoui") must be_==("0234-005-The Nameless City")
    }

    "check valid identifiers" >> {
      Document.ValidIdentifier.findFirstIn("") must beNone
      Document.ValidIdentifier.findFirstIn("index") must beNone
      Document.ValidIdentifier.findFirstIn("d/987") must beNone
      Document.ValidIdentifier.findFirstIn("user/1234") must beNone
      Document.ValidIdentifier.findFirstIn("user/1234/profile") must beNone

      def checkValidId(in: String, expectedKey: String, expectedVersion: String) = {
        in match {
          case Document.ValidIdentifier(key, version) => {
            key must be_==(expectedKey)
            if (expectedVersion == null) {
              version must beNull[String]
            }
            else {
              version must be_==(expectedVersion)
            }
          }
          case _ => {
            fail()
          }
        }
      }

      checkValidId("0", "0", null)
      checkValidId("1", "1", null)
      checkValidId("0002", "0002", null)
      checkValidId("987", "987", null)
      checkValidId("9999", "9999", null)
      checkValidId("12345", "12345", null)

      checkValidId("12-4", "12", "-4")
      checkValidId("9999-999", "9999", "-999")
    }

    "check valid document filename" >>
    {
      def checkValid(in: String, expectedKey: String, expectedVersion: String, expectedFileName: String)
      {
        in match {
          case Document.ValidDocumentFileName(key, version, fileName) =>
          {
            key must be_==(expectedKey)
            version must be_==(expectedVersion)
            expectedFileName must be_==(expectedFileName)
          }
          case _ =>
          {
            fail()
          }
        }
      }

      Document.ValidDocumentFileName.findFirstIn("6146-001") must beNone
      Document.ValidDocumentFileName.findFirstIn("6146-001-") must beNone
      Document.ValidDocumentFileName.findFirstIn("6146-New Document Test.txt") must beNone
      Document.ValidDocumentFileName.findFirstIn("New Document Test.txt") must beNone

      checkValid("6146-001-New Document Test 3.txt", "6146", "001", "New Document Test 3.txt")
    }
  }
}