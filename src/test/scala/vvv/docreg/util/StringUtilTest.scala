/*
 * Copyright (c) 2013 Scott Abernethy.
 * This file is part of DocReg+Web. Please refer to the NOTICE.txt file for license details.
 */

package vvv.docreg.util

import org.specs._

class StringUtilTest extends Specification {
  "StringUtil" should {
    "parse valid email" >> {
      StringUtil.ValidEmail.findFirstIn("scott.abernethy@aviatnet.com") must beSome("scott.abernethy@aviatnet.com")
      StringUtil.ValidEmail.findFirstIn("sabernethy@GNET.global.vpn") must beSome("sabernethy@GNET.global.vpn")
    }

    "parse valid domain username" >> {
      StringUtil.DomainUsername.findFirstIn("GNET\\sabernethy") must beSome("GNET\\sabernethy")
      StringUtil.DomainUsername.findFirstIn("GNET/sabernethy") must beSome("GNET/sabernethy")
      StringUtil.DomainUsername.findFirstIn("sabernethy") must beNone
      StringUtil.DomainUsername.findFirstIn("sabernethy@GNET") must beNone
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
      StringUtil.fileExtension("/usr/share/foo/goat.") must beNone
      StringUtil.fileExtension("asdf sfdsf") must beNone
      StringUtil.fileExtension("/asdf/sf/sf sdf - sdf") must beNone
    }
    "create document file name from title plus user uploaded file extension" >>
    {
      StringUtil.retitleFile("Harold", "money.png") must beSome("Harold.png")
      StringUtil.retitleFile("Harold.txt", "money.bmp") must beSome("Harold.txt.bmp")
      StringUtil.retitleFile("Gone with the", "wind") must beSome("Gone with the")
      StringUtil.retitleFile("Gone with the", "wind.") must beSome("Gone with the")
      StringUtil.retitleFile("Gone with the", "/usr/tmp/wind") must beSome("Gone with the")
      StringUtil.retitleFile("Gone with the", "/usr/tmp/northerly.wind") must beSome("Gone with the.wind")
      StringUtil.retitleFile("", "foo.bar") must beNone
      StringUtil.retitleFile(null, "foo.bar") must beNone
      StringUtil.retitleFile("baz", "") must beSome("baz")
      StringUtil.retitleFile("baz", null) must beSome("baz")
    }

    "pluralise" >>
    {
      StringUtil.pluralise(0, "foo") must be_==("0 foos")
      StringUtil.pluralise(1, "foo") must be_==("1 foo")
      StringUtil.pluralise(2, "foo") must be_==("2 foos")
      StringUtil.pluralise(1000, "foo") must be_==("1000 foos")

      StringUtil.pluralise(1000, "foo", "es") must be_==("1000 fooes")
    }
  }
}
