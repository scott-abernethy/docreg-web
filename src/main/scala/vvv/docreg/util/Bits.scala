/*
 * Copyright (c) 2013 Scott Abernethy.
 * This file is part of DocReg+Web. Please refer to the NOTICE.txt file for license details.
 */

package vvv.docreg.util

import xml.NodeSeq

object Bits {
  def restrictedNotice(count: Long): NodeSeq = {
    <p><span class="badge badge-warning">{ count }</span> { "Restricted document" + (if (count != 1L) "s" else "") + " not shown" }</p>
  }
}
