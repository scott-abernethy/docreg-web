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
      val d = Document.create.key("999").project(p).title("hellow world").editor("me").access("all")
      d.save

      d.nextVersion must be_==(1)
    }

    "create next version file name" in {
      TestDbVendor.initAndClean()
      val (u1, u2) = TestDbVendor.createUsers
      val p = Project.create.name("Cthulhu")
      p.save
      val d: Document = Document.create.key("234").project(p).title("The Nameless City").editor("H P Lovecraft").access("Forbidden")
      d.save
      val r4 = Revision.create.document(d).version(4).filename("foo.txt").author(u2).date(new Date()).comment("hmmm")
      r4.save

      d.nextFileName("youyoui.odt") must be_==("0234-005-The Nameless City.odt")
    }

    "check valid identifiers" >> {
      Document.ValidIdentifier.findFirstIn("") must beNone
      Document.ValidIdentifier.findFirstIn("1") must beSome("1")
      Document.ValidIdentifier.findFirstIn("987") must beSome("987")
      Document.ValidIdentifier.findFirstIn("0002") must beSome("0002")
      Document.ValidIdentifier.findFirstIn("9999") must beSome("9999")
      Document.ValidIdentifier.findFirstIn("index") must beNone
      Document.ValidIdentifier.findFirstIn("d/987") must beNone
      Document.ValidIdentifier.findFirstIn("user/1234") must beNone
      Document.ValidIdentifier.findFirstIn("user/1234/profile") must beNone
    }
  }
}