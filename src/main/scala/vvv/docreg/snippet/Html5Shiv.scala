package vvv.docreg.snippet

import scala.xml.Unparsed

/** HTML5 shiv/shim, for IE6-8 support of HTML5 elements */
object Html5Shiv {
  def render = Unparsed("""<!--[if lt IE 9]>
  <script src="http://html5shim.googlecode.com/svn/trunk/html5.js">
  </script><![endif]-->""")
}
