/*
 * Copyright (c) 2013 Scott Abernethy.
 * This file is part of DocReg+Web. Please refer to the NOTICE.txt file for license details.
 */

package vvv.docreg.snippet

import org.specs2.mutable._
import java.util.Calendar
import vvv.docreg.model.Revision
import java.sql.Timestamp

class HistoryTest extends Specification
{
  "MonthHistory" should
  {
    "have date range back 30 days" >>
    {
      val d = Calendar.getInstance

      d.set(2012, Calendar.JANUARY, 5, 11, 49, 58)
      val r1 = new Revision
      r1.date = new Timestamp(d.getTimeInMillis)

      d.set(2012, Calendar.JANUARY, 2, 11, 49, 58)
      val r2 = new Revision
      r2.date = new Timestamp(d.getTimeInMillis)
      val r3 = new Revision
      r3.date = new Timestamp(d.getTimeInMillis)

      d.set(2011, Calendar.DECEMBER, 27, 3, 4, 5)
      val r4 = new Revision
      r4.date = new Timestamp(d.getTimeInMillis)

      d.set(2011, Calendar.DECEMBER, 1, 3, 4, 5)
      val r5 = new Revision
      r5.date = new Timestamp(d.getTimeInMillis)

      val revisions = List(r1, r2, r3, r4, r5)

      val now = Calendar.getInstance
      now.set(2012, Calendar.JANUARY, 5, 11, 49, 59)
      val x = new MonthHistory(){
        override def load() = Nil
      }.analyse(now, revisions)
      x must haveSize(30)
      x(29) must be_==(Sample(0, 1, "5"))
      x(28) must be_==(Sample(-1, 0, "4"))
      x(26) must be_==(Sample(-3, 2, "2"))
      x(20) must be_==(Sample(-9, 1, "27"))
      x(19) must be_==(Sample(-10, 0, "26"))
      x(0) must be_==(Sample(-29, 0, "7"))
    }
  }
}