/*
 * Copyright (c) 2013 Scott Abernethy.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package code

import java.io.File

import scala.xml.XML

import org.specs.Specification
import org.specs.runner.JUnit4

import net.liftweb.common.Full
import net.liftweb.util.PCDataXmlParser

class XmlSourceSpecs extends Specification {

  "XML Sources" should {
    "be well-formed" in {
      /**
       * Tests to make sure the project's XML files are well-formed.
       *
       * Finds every *.html and *.xml file in src/main/webapp (and its
       * subdirectories) and tests to make sure they are well-formed.
       */
      var failed: List[File] = Nil
      
      def handledXml(file: String) =
	file.endsWith(".xml")
      
      def handledXHtml(file: String) =
	file.endsWith(".html") || file.endsWith(".htm") || file.endsWith(".xhtml")
      
      def wellFormed(file: File) {
	if (file.isDirectory)
	  for (f <- file.listFiles) wellFormed(f)
        
	if (file.isFile && handledXml(file.getName)) {
	  try {
	    XML.loadFile(file)
	  } catch {
	    case e: org.xml.sax.SAXParseException => failed = file :: failed
	  }
	}
	if (file.isFile && handledXHtml(file.getName)) {
	  PCDataXmlParser(new java.io.FileInputStream(file.getAbsolutePath)) match {
	    case Full(_) => // file is ok
	      case _ => failed = file :: failed
	  }
	}
      }
      
      wellFormed(new File("src/main/webapp"))
      
      val numFails = failed.size
      if (numFails > 0) {
	val fileStr = if (numFails == 1) "file" else "files"
	val msg = "Malformed XML in " + numFails + " " + fileStr + ": " + failed.mkString(", ")
	fail(msg)
      }
      
      numFails must_== 0
    }
  }
}
