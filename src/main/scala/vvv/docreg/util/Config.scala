/*
 * Copyright (c) 2013 Scott Abernethy.
 * This file is part of DocReg+Web. Please refer to the NOTICE.txt file for license details.
 */

package vvv.docreg.util

import net.liftweb.util.Props
import org.streum.configrity.Configuration

object Config {
  lazy val is = Props.get("conf") map(Configuration.load(_)) getOrElse (Configuration())
}
