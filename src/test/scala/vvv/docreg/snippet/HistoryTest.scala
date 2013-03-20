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

package vvv.docreg.snippet

import org.specs.Specification
import java.util.Calendar
import vvv.docreg.model.Revision
import java.sql.Timestamp

object HistoryTest extends Specification
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