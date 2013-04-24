/*
 * Copyright (c) 2013 Scott Abernethy.
 * This file is part of DocReg+Web. Please refer to the NOTICE.txt file for license details.
 */

package vvv.docreg.util

import java.sql.Timestamp
import java.util.Date

object T {
  def at(date: Date) = {
    new Timestamp(date.getTime)
  }

  def ago(duration: Long) = {
    new Timestamp(System.currentTimeMillis() - duration)
  }

  def now() = {
    new Timestamp(System.currentTimeMillis())
  }
}
