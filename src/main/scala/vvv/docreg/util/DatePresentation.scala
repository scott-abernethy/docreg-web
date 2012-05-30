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
  private val dateF = new SimpleDateFormat("MMM dd '('E')'")
  dateF.setTimeZone(TimeZone.getDefault)

  def now = new Date

  def short(date: Date): String =
  {
    if (date == null) return "---"
    if (formatDay(now) == formatDay(date)) {
      "Today " + formatTime(date)
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
