/*
 * Copyright (c) 2013 Scott Abernethy.
 * This file is part of DocReg+Web. Please refer to the NOTICE.txt file for license details.
 */

package vvv.docreg.util

import java.util.Date
import vvv.docreg.util.StringUtil._

class Ago(private val date: Date) {
  def now = new Date
  override def toString = {
    val mins = (now.getTime - date.getTime) / (1000 * 60)
    if (mins < 60) pluralise(mins, "min") else pluralise (mins / 60, "hour")
  }
}
