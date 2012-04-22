package vvv.docreg.model

import org.specs._
import java.util.Date
import vvv.docreg.db.TestDbVendor
import net.liftweb.http.js.JsCmds._Noop
import vvv.docreg.util.T
import org.squeryl.PrimitiveTypeMode._

object DocumentTest extends Specification {
  "Document Model" should {
    "next version is 1 if no revisions" >> {
      TestDbVendor.initAndClean()
      transaction{
      val (p,_,_) = TestDbVendor.createProjects
      val d = new Document
      d.number = ("336")
      d.projectId = (p.id)
      d.title = ("Foo bar")
      d.access = ("Everyone")
      Document.dbTable.insert(d)

      d.nextVersion must be_==(1)
      }
    }

    "create next version file name" in {
      TestDbVendor.initAndClean()
      transaction{
      val (u1, u2) = TestDbVendor.createUsers
      val (p,_,_) = TestDbVendor.createProjects
      val d = new Document
      d.number = ("234")
      d.projectId = (p.id)
      d.title = ("The Nameless City")
      d.access = ("Forbidden")
      Document.dbTable.insert(d)

      val r4 = new Revision
      r4.documentId = (d.id)
      r4.version = (4)
      r4.filename = ("foo.txt")
      r4.authorId = (u2.id)
      r4.date = (T.now)
      r4.comment = ("hmmm")
      Revision.dbTable.insert(r4)

      d.nextFileName("Rainbow Fish", "youyoui.odt") must be_==("0234-005-Rainbow Fish.odt")
      }
    }

    "check no file extension" in {
      TestDbVendor.initAndClean()
      transaction{
      val (u1, u2) = TestDbVendor.createUsers
      val (p,_,_) = TestDbVendor.createProjects
      val d = new Document
      d.number = ("234")
      d.projectId = (p.id)
      d.title = ("The Nameless City")
      d.access = ("Forbidden")
      Document.dbTable.insert(d)

      val r4 = new Revision
      r4.documentId = (d.id)
      r4.version = (4)
      r4.filename = ("foo.txt")
      r4.authorId = (u2.id)
      r4.date = (T.now)
      r4.comment = ("hmmm")
      Revision.dbTable.insert(r4)

      d.nextFileName("The Nameless City", "youyoui") must be_==("0234-005-The Nameless City")
      }
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