package vvv.docreg.backend

import net.liftweb.ldap._
import net.liftweb.util.ControlHelpers._
import javax.naming.directory._
import net.liftweb.common.{Full, Failure, Empty, Box}
import vvv.docreg.model.User
import vvv.docreg.util.StringUtil.ValidEmail
/*
https://wiki.shibboleth.net/confluence/display/SHIB2/IdPADConfigIssues
http://download.oracle.com/javase/jndi/tutorial/ldap/referral/jndi.html
*/

/*
userPrincipalName: sabernethy@GNET.global.vpn    (must be unique)
mailNickname: sabernethy
mail Scott.Abernethy@Aviatnet.com
name SAbernethy
sAMAccountName: sabernethy
sn: Abernethy
givenName: Scott
displayName: Scott Abernethy
distinguishedName: CN=SAbernethy,OU=NZ,OU=People,OU=APAC,DC=GNET,DC=global,DC=vpn
 */

case class UserAttributes(userName: String, mail: String, displayName: String)

trait Directory {
  def findFromMail(mailUserName: String): Box[UserAttributes]
  def findFromUserName(userName: String): Box[UserAttributes]
  def findFromPartialName(partialName: String): Box[UserAttributes]
}

trait DirectoryComponent {
  val directory: Directory
}

class DirectoryImpl extends LDAPVendor with Directory {
  configure(
    Map(
      "ldap.url" -> "ldap://dcgnetnz1.gnet.global.vpn:3268",
      "ldap.userName" -> "gnet\\DocRegSystem",
      "ldap.password" -> "***REMOVED***",
      "ldap.base" -> DirectoryConfig.ldapBase,
      "lift-ldap.testLookup" -> DirectoryConfig.testLookup,
      "lift-ldap.retryInterval" -> "3000",
      "lift-ldap.maxRetries" -> "5"
    )
  )

  def searchIt(filter: String): List[String] = {
    val sc = new SearchControls()
    sc.setSearchScope(SearchControls.SUBTREE_SCOPE)
    // todo restrict search to attributes we care about?
    searchControls.doWith(sc) {
      search(filter)
    }
  }

  def findFromPartialName(partialName: String): Box[UserAttributes] = {
    find("displayName=" + partialName.replaceAll(" ", "*") + "")
  }

  def findFromMail(mail: String): Box[UserAttributes] = {
    find("mail=" + mail)
  }

  def findFromUserName(userName: String): Box[UserAttributes] =
  {
    val u = userName match
    {
      case ValidEmail(name, domain) =>
        name.replaceAll("\\.", "")
      case other =>
        other
    }
    find("userPrincipalName=" + u + User.domain)
  }
  
  def find(filter: String): Box[UserAttributes] = {
    tryo(searchIt("(&(objectCategory=person)(" + filter + "))")) match {
      case Full(Nil) => Empty
      case Full(dn :: Nil) => findFromDn(dn)
      case Full(dn :: more) => Failure("Multiple users found " + (dn :: more))
      case _ => Failure("Exception encoutered during search for '" + filter + "'")
    }
  }

  def findFromDn(dn: String): Box[UserAttributes] = {
    def extractValue(attr: Attributes, key: String): Option[String] = Option(attr).map(_.get(key)).filter(_ != null).map(_.get()).filter(_ != null).map(_.toString)
    tryo(attributesFromDn(dn + "," + DirectoryConfig.ldapBase)) match {
      case Full(attr) if (attr != null) => {
        Box(
          for {
            userPrincipalName <- extractValue(attr, "userPrincipalName")
            mail <- extractValue(attr, "mail")
            displayName <- extractValue(attr, "displayName")
          } yield UserAttributes(userPrincipalName, mail, displayName)
        )
      }
      case _ => {
        Empty
      }
    }
  }
}

trait DirectoryComponentImpl extends DirectoryComponent {
  val directory = new DirectoryImpl
}

object DirectoryConfig {
  val ldapBase = "DC=GNET,DC=global,DC=vpn"
  val testLookup = "CN=DocRegUser,OU=DocReg,OU=New Zealand,OU=Groups,OU=APAC,DC=GNET,DC=global,DC=vpn"
}
