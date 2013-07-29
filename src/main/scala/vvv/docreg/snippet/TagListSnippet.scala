/*
 * Copyright (c) 2013 Aviat Networks.
 * This file is part of DocReg+Web. Please refer to the NOTICE.txt file for license details.
 */

package vvv.docreg.snippet

import net.liftweb.common.Loggable
import scala.xml.Unparsed
import net.liftweb.util.Helpers._
import vvv.docreg.model._
import net.liftweb.sitemap.Menu
import net.liftweb.common.Full
import net.liftweb.http.S

class TagListSnippet extends Loggable {
  
  def list() = {
    val items = Tag.names()
    ".t-count *" #> items.size &
    ".t-item *" #> items.map { item =>
      <a href={ Tag.url(item) }>{ item }</a> ++ Unparsed("&nbsp; ")
    }
  }

}
