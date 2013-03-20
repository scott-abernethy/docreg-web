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

package vvv.docreg.util

import java.text._
import java.util.Date
import java.util.TimeZone
import vvv.docreg.model.User
import net.liftweb.common.Full

object DatePresentation {
  // TODO date formats are not synchronized, so may play up if accessed concurrently
  private val dateTimeF = new SimpleDateFormat("MMM dd, yyyy h:mm a")
  dateTimeF.setTimeZone(TimeZone.getDefault)
  private val dayF = new SimpleDateFormat("MMM dd, yyyy")
  dayF.setTimeZone(TimeZone.getDefault)
  private val timeF = new SimpleDateFormat("h:mm a")
  timeF.setTimeZone(TimeZone.getDefault)
  private val dateF = new SimpleDateFormat("MMM dd")
  dateF.setTimeZone(TimeZone.getDefault)

  def now = new Date

  def short(date: Date): String =
  {
    if (date == null) return "---"
    if (formatDay(now) == formatDay(date)) {
      formatTime(date)
    } else {
      formatDate(date)
    }
  }

  def formatDateTime(date: Date): String =
  {
    if (date == null) return "---"
    User.loggedInUser.is.foreach(u =>
      dateTimeF.setTimeZone(u.getTimeZone)
    )
    dateTimeF.format(date)
  }

  def formatDay(date: Date): String =
  {
    if (date == null) return "---"
    User.loggedInUser.is.foreach(u =>
      dayF.setTimeZone(u.getTimeZone)
    )
    dayF.format(date)
  }

  def formatTime(date: Date): String =
  {
    if (date == null) return "---"
    User.loggedInUser.is.foreach(u =>
      timeF.setTimeZone(u.getTimeZone)
    )
    timeF.format(date)
  }

  def formatDate(date: Date): String =
  {
    if (date == null) return "---"
    User.loggedInUser.is.foreach(u =>
      dateF.setTimeZone(u.getTimeZone)
    )
    dateF.format(date)
  }


}
