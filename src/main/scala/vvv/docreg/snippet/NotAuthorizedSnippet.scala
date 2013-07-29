/*
 * Copyright (c) 2013 Aviat Networks.
 * This file is part of DocReg+Web. Please refer to the NOTICE.txt file for license details.
 */

package vvv.docreg.snippet

import net.liftweb.common.Loggable
import net.liftweb.util.PassThru
import net.liftweb.http.RequestVar
import net.liftweb.util.Helpers._
import net.liftweb.http.S

class NotAuthorizedSnippet extends Loggable {

  val username = S.param("u") openOr "???"
  
  def render = {
    ".x-username *" #> username
  }

}
