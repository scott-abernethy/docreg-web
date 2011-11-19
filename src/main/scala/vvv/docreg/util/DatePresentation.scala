package vvv.docreg.util

import java.text._
import java.util.Date
import java.util.TimeZone

object DatePresentation {
  val dateTimeF = new SimpleDateFormat("d MMM yyyy, h:mm a")
  dateTimeF.setTimeZone(TimeZone.getDefault)
  val dayF = new SimpleDateFormat("MMM dd, yyyy")
  dayF.setTimeZone(TimeZone.getDefault)
  val timeF = new SimpleDateFormat("h:mm a")
  timeF.setTimeZone(TimeZone.getDefault)
  val dateF = new SimpleDateFormat("d MMM '('E')'")
  dateF.setTimeZone(TimeZone.getDefault)

  def now = new Date

  def short(date: Date) = {
    if (dayF.format(now) == dayF.format(date)) {
      "Today " + timeF.format(date)
    } else {
      dateF.format(date) 
    }
  }
}
