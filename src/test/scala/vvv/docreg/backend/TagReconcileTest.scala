/*
 * Copyright (c) 2013 Aviat Networks.
 * This file is part of DocReg+Web. Please refer to the NOTICE.txt file for license details.
 */

package vvv.docreg.backend

import org.specs2.mutable._

class TagReconcileTest extends Specification {
  "TagReconcile" should {
    "clean up tag strings" >> {
      val x = new TagReconcile{}
      x.cleanTag("#") must beNone
      x.cleanTag("#G") must beSome("#G")
      x.cleanTag(" #foo ") must beSome("#foo")
      x.cleanTag(" rubbish....  #myTag-1 fudsf") must beSome("#myTag-1")
      x.cleanTag("#bus-last") must beSome("#bus-last")
      x.cleanTag("#bus-33") must beSome("#bus-33")
      x.cleanTag("#thecakeisalie. ") must beSome("#thecakeisalie")
      x.cleanTag(" #FF,") must beSome("#FF")
      x.cleanTag("#GoAT)") must beSome("#GoAT")
      x.cleanTag("#yeP\"yo\"") must beSome("#yeP")
      x.cleanTag("#'s") must beNone
      x.cleanTag("#ttocs's") must beSome("#ttocs")
      x.cleanTag("") must beNone
      x.cleanTag(" mytag ") must beNone
      x.cleanTag(" mytag #red #blue #green ") must beSome("#red")
    }
    "a number is not a hashtag" >> {
      val x = new TagReconcile{}
      x.cleanTag("#4") must beNone
      x.cleanTag("#345-346") must beNone
      x.cleanTag("#561. ") must beNone
      x.cleanTag(" #17,") must beNone
      x.cleanTag("#2553)") must beNone
      x.cleanTag("#51\"") must beNone
    }
  }
}
