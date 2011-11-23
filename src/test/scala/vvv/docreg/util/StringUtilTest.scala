package vvv.docreg.util

import org.specs._

object StringUtilTest extends Specification {
  "StringUtil" should {
    "parse valid email" >> {
      StringUtil.ValidEmail.findFirstIn("scott.abernethy@aviatnet.com") must beSome("scott.abernethy@aviatnet.com")
      StringUtil.ValidEmail.findFirstIn("sabernethy@GNET.global.vpn") must beSome("sabernethy@GNET.global.vpn")
    }

    "convert email to name" >> {
      StringUtil.nameFromEmail("foo@bar.com") must be equalTo("Foo")
      StringUtil.nameFromEmail("scott.abernethy@aviatnet.com") must be equalTo("Scott Abernethy")
      StringUtil.nameFromEmail("SCOTT-andrew+abErnethy@Gmail.com") must be equalTo("Scott Andrew Abernethy")
      StringUtil.nameFromEmail("david.36.smith@bt.com") must be equalTo("David Smith")
      StringUtil.nameFromEmail("Sunil.Kumar2@Aviatnet.com") must be equalTo("Sunil Kumar")
      StringUtil.nameFromEmail("") must be equalTo("")
      StringUtil.nameFromEmail("the quick Brown fox") must be equalTo("")
      StringUtil.nameFromEmail("yogie@") must be equalTo("Yogie")
      StringUtil.nameFromEmail("@that doesn't work") must be equalTo("")
    }
    "prePad" >> {
      StringUtil.prePadTo("45", 4, '0') must be_==("0045")
      StringUtil.prePadTo("1234", 4, '0') must be_==("1234")
      StringUtil.prePadTo("5", 3, '0') must be_==("005")
      StringUtil.prePadTo("578", 2, '0') must be_==("578")
    }
    "parse file extensions" >> {
      StringUtil.fileExtension("foo.bar.baz") must beSome("baz")
      StringUtil.fileExtension("123-887 wer sfaf.ZIP") must beSome("ZIP")
      StringUtil.fileExtension("/usr/share/foo/goat.sh") must beSome("sh")
      StringUtil.fileExtension("asdf sfdsf") must beNone
      StringUtil.fileExtension("/asdf/sf/sf sdf - sdf") must beNone
    }
  }
}
