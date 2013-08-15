package vvv.docreg.directory.faux

import vvv.docreg.backend.DirectoryComponent
import vvv.docreg.backend.Directory
import net.liftweb.common.Full
import vvv.docreg.backend.UserAttributes
import net.liftweb.common.Box
import net.liftweb.common.Empty
import vvv.docreg.backend.DirectoryConfig
import vvv.docreg.agent.faux.FauxData
import net.liftweb.json._

trait FauxDirectoryComponentImpl extends DirectoryComponent with FauxData {

  case class FauxUser(dn0: String, username0: String, email0: String, displayName0: String, memberOf0: List[String]) extends UserAttributes {
    def userName(): Option[String] = Some(username0)
    def email(): Option[String] = Some(email0)
    def displayName(): Option[String] = Some(displayName0)
    def dn(): Option[String] = Some(dn0)
    def department(): Option[String] = Some("Engineering")
    def description(): Option[String] = Some("Senior Doer of Stuff")
    def location(): Option[String] = Some("Wellington")
    def memberOf(): List[String] = memberOf0
  }
  
  lazy val users = userParser(loadData().openOr(JNothing))
  
  lazy val usersByMail = users.map(u => (u.email0, u)).toMap
  lazy val usersByUsername = users.map(u => (u.username0.takeWhile(_ != '@'), u)).toMap
  lazy val usersByDn = users.map(u => (u.dn0, u)).toMap
      
  val directory = new Directory {
    def findFromMail(mailUserName: String): Box[UserAttributes] = 
      usersByMail.get(mailUserName)
    def findFromUserName(userName: String): Box[UserAttributes] =
      usersByUsername.get(userName)
    def findFromPartialName(partialName: String): Box[UserAttributes] =
      Empty
    def findAttributes(dn: String): Box[UserAttributes] = 
      usersByDn.get(dn)
    def groupMembers(dn: String): Box[List[String]] = Full(List())
    def login(dn: String, password: String): Boolean = 
      usersByDn.get(dn).isDefined
  }
  
  def userParser(json: JValue): List[FauxUser] = {
    import vvv.docreg.util.JValueWithFilter._
    for {
      JObject(user) <- json \ "users"
      JField("dn", JString(dn)) <- user
      JField("name", JString(name)) <- user
      JField("username", JString(username)) <- user
      JField("email", JString(email)) <- user
      JField("groups", JArray(groups)) <- user
    }
    yield FauxUser(dn, username, email, name, groups.collect{case JString(str) => str}.toList)
  }
}
