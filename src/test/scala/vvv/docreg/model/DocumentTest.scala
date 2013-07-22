/*
* Copyright (c) 2013 Aviat Networks.
* This file is part of DocReg+Web. Please refer to the NOTICE.txt file for license details.
*/

package vvv.docreg.model

import org.specs2.mutable._
import vvv.docreg.db.{TestDbScope}
import vvv.docreg.util.T

class DocumentTest extends Specification {

  sequential

  "Document Model" should {
    "next version is 1 if no revisions" in new TestDbScope {
      import org.squeryl.PrimitiveTypeMode._
      transaction{
      val (p,_,_) = db.createProjects
      val d = new Document
      d.number = ("336")
      d.projectId = (p.id)
      d.title = ("Foo bar")
      d.access = ("Everyone")
      Document.dbTable.insert(d)

      d.nextVersion must be_==(1)
      }
      success
    }

    "create next version file name" in new TestDbScope {
      import org.squeryl.PrimitiveTypeMode._
      transaction{
      val (u1, u2) = db.createUsers
      val (p,_,_) = db.createProjects
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
      success
    }

    "check no file extension" in new TestDbScope {
      import org.squeryl.PrimitiveTypeMode._
      transaction{
      val (u1, u2) = db.createUsers
      val (p,_,_) = db.createProjects
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
      success
    }

    "check valid identifiers" in {
      Document.ValidIdentifier.findFirstIn("") must beNone
      Document.ValidIdentifier.findFirstIn("d/987") must beNone
      Document.ValidIdentifier.findFirstIn("user/1234") must beNone
      Document.ValidIdentifier.findFirstIn("user/signin") must beNone
      Document.ValidIdentifier.findFirstIn("12345") must beNone
      Document.ValidIdentifier.findFirstIn("AB12") must beNone

      def checkValidId(in: String, expectedKey: String, expectedVersion: String) = {
        import Document.ValidIdentifier
        in match {
          case ValidIdentifier(key, version) => {
            key must be_==(expectedKey)
            if (expectedVersion == null) {
              version must beNull[String]
            }
            else {
              version must be_==(expectedVersion)
            }
          }
          case _ => {
            failure
          }
        }
      }

      checkValidId("0", "0", null)
      checkValidId("1", "1", null)
      checkValidId("0002", "0002", null)
      checkValidId("987", "987", null)
      checkValidId("9999", "9999", null)
      checkValidId("1234", "1234", null)

      checkValidId("12-4", "12", "-4")
      checkValidId("9999-999", "9999", "-999")

      // NOTE likely implementation is with alpha only in most significant position e.g. "a001"
      
      // without version
      checkValidId("A897", "A897", null)
      checkValidId("j090", "j090", null)
      checkValidId("S007", "S007", null)
      
      // with version
      checkValidId("S007-1", "S007", "-1")
      checkValidId("P000-6", "P000", "-6")
      checkValidId("K140-6", "K140", "-6")
      checkValidId("w007-999", "w007", "-999")
      checkValidId("z999-999", "z999", "-999")
      checkValidId("z999-4", "z999", "-4")

      success
    }

    "check valid document filename" in
    {
      import Document.ValidDocumentFileName

      def checkValidFN(in: String, expectedKey: String, expectedVersion: String, expectedFileName: String)
      {
        in match {
          case ValidDocumentFileName(key, version, fileName) =>
          {
            key must be_==(expectedKey)
            version must be_==(expectedVersion)
            expectedFileName must be_==(expectedFileName)
          }
          case _ =>
          {
            failure
          }
        }
      }

      ValidDocumentFileName.findFirstIn("6146-001") must beNone
      ValidDocumentFileName.findFirstIn("R876-B09") must beNone
      ValidDocumentFileName.findFirstIn("6146-001-") must beNone
      ValidDocumentFileName.findFirstIn("6146-New Document Test.txt") must beNone
      ValidDocumentFileName.findFirstIn("New Document Test.txt") must beNone

      checkValidFN("6146-001-New Document Test 3.txt", "6146", "001", "New Document Test 3.txt")
      checkValidFN("a054-088-Freak.mov", "a054", "088", "Freak.mov")

      success
    }

    "check valid identifier and ext" in
    {
      import Document.IdentifierAndExtension

      def checkValidIAE(in: String, expectedKey: String)
      {
        in match {
          case IdentifierAndExtension(key) =>
          {
            key must be_==(expectedKey)
          }
          case _ =>
          {
            failure
          }
        }
      }
      IdentifierAndExtension.findFirstIn("6146.") must beNone
      IdentifierAndExtension.findFirstIn(".txt") must beNone
      IdentifierAndExtension.findFirstIn("my doco") must beNone
      IdentifierAndExtension.findFirstIn("1234 txt") must beNone
      IdentifierAndExtension.findFirstIn("roger.txt") must beNone
      IdentifierAndExtension.findFirstIn("e78.txt") must beNone

      checkValidIAE("987.foo", "987")
      checkValidIAE("0987.foo", "0987")
      checkValidIAE("1234.doc", "1234")
      checkValidIAE("I098.aoi", "I098")
      checkValidIAE("e780.in", "e780")

      success
    }

    "validate user access" in new TestDbScope {
      import org.squeryl.PrimitiveTypeMode._
      inTransaction {
        val (p1, p2, p3) = db.createProjects
        val (u1, u2) = db.createUsers

        val x = new Document
        x.projectId = p3.id
        x.title = "Foo"
        x.number = "1444"
        x.access = "Public"
        x.allows(u1) must beTrue

        x.access = "Secure"
        x.allows(u1) must beFalse

        ProjectAuthorization.grant(u2, p3)
        x.allows(u1) must beFalse

        ProjectAuthorization.grant(u1, p3)
        x.allows(u1) must beTrue

        ProjectAuthorization.revoke(u1, p3)
        x.allows(u1) must beFalse
      }
      success
    }
  }

  "DocumentRef extractor" should {
    import Document.DocumentRef

    "Fail to extract garbage" in {
      DocumentRef.unapply("garbage") must beNone
      DocumentRef.unapply("^&*2") must beNone
      DocumentRef.unapply("/root/readme.txt") must beNone
    }

    "Extract 1234 or 0066 like" in {
      DocumentRef.unapply("1234") must beSome(("1234", Long.MaxValue))
      DocumentRef.unapply("546") must beSome(("546", Long.MaxValue))
      DocumentRef.unapply("9") must beSome(("9", Long.MaxValue))
      DocumentRef.unapply("0066") must beSome(("0066", Long.MaxValue))
      DocumentRef.unapply("y789") must beSome(("Y789", Long.MaxValue))
      DocumentRef.unapply("P401") must beSome(("P401", Long.MaxValue))
      DocumentRef.unapply("Z") must beNone
      DocumentRef.unapply("QW") must beNone
      DocumentRef.unapply("lkj") must beNone
      DocumentRef.unapply("hYs3") must beNone
    }

    "Extract 2345-499 or 765-34 or 8-001 like" in {
      DocumentRef.unapply("2345-499") must beSome(("2345", 499l))
      DocumentRef.unapply("765-34") must beSome(("765", 34l))
      DocumentRef.unapply("8-001") must beSome(("8", 1l))
      DocumentRef.unapply("7-2") must beSome(("7", 2l))
      DocumentRef.unapply("0066-002") must beSome(("0066", 2l))
      DocumentRef.unapply("2R45-499") must beNone
      DocumentRef.unapply("W145-499") must beSome(("W145", 499l))
      DocumentRef.unapply("b65-34") must beNone
      DocumentRef.unapply("b065-34") must beSome(("B065", 34l))
      DocumentRef.unapply("Q-001") must beNone
      DocumentRef.unapply("x-2") must beNone
      DocumentRef.unapply("A066-102") must beSome(("A066", 102l))
    }

    "Extract 1234.txt or 1234-876.txt like" in {
      DocumentRef.unapply("1234.txt") must beSome(("1234", Long.MaxValue))
      DocumentRef.unapply("1234-876.txt") must beSome(("1234", 876l))
      DocumentRef.unapply("U098.txt") must beSome(("U098", Long.MaxValue))
      DocumentRef.unapply("w769-876.txt") must beSome(("W769", 876l))
    }

    "Extract 1234-My title.zip or 0666-666-Snap.jpg" in {
      DocumentRef.unapply("1234-My title.zip") must beSome(("1234", Long.MaxValue))
      DocumentRef.unapply("0666-666-Snap.jpg") must beSome(("0666", 666l))
      DocumentRef.unapply("p707-My title.zip") must beSome(("P707", Long.MaxValue))
      DocumentRef.unapply("v001-666-Snap.jpg") must beSome(("V001", 666l))
    }
  }

  "DocumentRevision extractor" should {
    import Document.DocumentRevision

    "Not extract garbage input" in new TestDbScope {
      DocumentRevision.unapply("X-Y") must beNone
      DocumentRevision.unapply("XXXX-YYY") must beNone
      DocumentRevision.unapply("123KKK-REV") must beNone
      DocumentRevision.unapply("garbage") must beNone
    }

    "Load correct document" in new TestDbScope {
      import org.squeryl.PrimitiveTypeMode._
      inTransaction{
        val (p1, p2, p3) = db.createProjects
        val (u1, u2) = db.createUsers

        val d1 = new Document
        d1.number = ("0234")
        d1.projectId = (p1.id)
        d1.title = ("The Nameless City")
        d1.access = ("Forbidden")
        Document.dbTable.insert(d1)

        val r1 = new Revision
        r1.documentId = (d1.id)
        r1.version = (1)
        r1.filename = ("foo.txt")
        r1.authorId = (u1.id)
        r1.date = (T.now)
        r1.comment = ("ok ok ok now")
        Revision.dbTable.insert(r1)

        val r4 = new Revision
        r4.documentId = (d1.id)
        r4.version = (4)
        r4.filename = ("foo.txt")
        r4.authorId = (u2.id)
        r4.date = (T.now)
        r4.comment = ("hmmm")
        Revision.dbTable.insert(r4)

        val d2 = new Document
        d2.number = ("G029")
        d2.projectId = (p1.id)
        d2.title = ("Startide Rising")
        d2.access = ("Public")
        Document.dbTable.insert(d2)
        
        val r55 = new Revision
        r55.documentId = (d2.id)
        r55.version = (55)
        r55.filename = ("qux.txt")
        r55.authorId = (u2.id)
        r55.date = (T.now)
        r55.comment = ("yuup!")
        Revision.dbTable.insert(r55)

        DocumentRevision.unapply("0233") must beNone
        DocumentRevision.unapply("234") must beNone
        DocumentRevision.unapply("0234") must beSome((d1, r4))
        DocumentRevision.unapply("0234-004") must beSome((d1, r4))
        DocumentRevision.unapply("0234-4") must beSome((d1, r4))
        DocumentRevision.unapply("0234-001") must beSome((d1, r1))
        DocumentRevision.unapply("0234-001-Some garbage") must beSome((d1, r1))
        DocumentRevision.unapply("0234-001-Some garbage.txt") must beSome((d1, r1))
        DocumentRevision.unapply("0234.txt") must beSome((d1, r4))
        DocumentRevision.unapply("0234txt") must beNone
        DocumentRevision.unapply("0234-1") must beSome((d1, r1))
        DocumentRevision.unapply("0234-009") must beNone
        DocumentRevision.unapply("0234-9") must beNone
        DocumentRevision.unapply("0234-XYZ") must beSome(d1, r4)
        DocumentRevision.unapply("0234-XYZ.jpg") must beSome(d1, r4)
        DocumentRevision.unapply("0234-") must beNone
        
        DocumentRevision.unapply("G29") must beNone
        DocumentRevision.unapply("G029") must beSome((d2, r55))
        DocumentRevision.unapply("G029-055") must beSome((d2, r55))
        DocumentRevision.unapply("G029-055-Cruft") must beSome((d2, r55))
        DocumentRevision.unapply("G029-055-Crufty.stuff") must beSome((d2, r55))
        DocumentRevision.unapply("G029-056") must beNone
        
      }
      success
    }
  }
}