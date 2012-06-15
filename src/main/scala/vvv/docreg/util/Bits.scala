package vvv.docreg.util

import xml.NodeSeq

object Bits {
  def restrictedNotice(count: Long): NodeSeq = {
    <p><span class="badge badge-warning">{ count }</span> { "Restricted document" + (if (count != 1L) "s" else "") + " not shown" }</p>
  }
}
