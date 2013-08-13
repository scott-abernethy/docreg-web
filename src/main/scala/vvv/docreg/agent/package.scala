/*
 * Copyright (c) 2013 Aviat Networks.
 * This file is part of DocReg+Web. Please refer to the NOTICE.txt file for license details.
 */

package vvv.docreg

import java.util.{Date,TimeZone}
import java.text._

package object agent {

  val documentNumberFormat = new DecimalFormat("0000")

  def agentFormat = {
    val x = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss 'Z'");
    x.setTimeZone(TimeZone.getTimeZone("UTC"))
    x
  }

  def parseAgentDate(dateString: String): Date = {
    if ("".equals(dateString)) {
      return null
    }
    try {
      // Date formats are not synchronized, so create this every time.
      agentFormat.parse(dateString);
    }
    catch {
      case x: Exception => {
        println("Failed to parse '" + dateString + "' - " + x);
        null;
      }
    }
  }

  def formatAgentDate(date: Date): String = {
    agentFormat.format(date)
  }

}
