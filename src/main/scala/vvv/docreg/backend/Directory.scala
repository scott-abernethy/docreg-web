package vvv.docreg.backend

import net.liftweb.ldap._
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
  val ldapBase = "DC=GNET,DC=global,DC=vpn"
  configure(
    Map(
      "ldap.url" -> "ldap://dcgnetnz1.gnet.global.vpn:3268",
      "ldap.userName" -> "gnet\\***REMOVED***",
      "ldap.password" -> "***REMOVED***",
      "ldap.base" -> ldapBase
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
    searchIt("(&(objectCategory=person)(" + filter + "))") match {
      case Nil => Empty
      case dn :: Nil => findFromDn(dn)
      case dn :: more => Failure("Multiple users found " + (dn :: more))
    }
  }

  def findFromDn(dn: String): Box[UserAttributes] = {
    def extractValue(attr: Attributes, key: String): Option[String] = Option(attr).map(_.get(key)).filter(_ != null).map(_.get()).filter(_ != null).map(_.toString)
    attributesFromDn(dn + "," + ldapBase) match {
      case null => Empty
      case attr => 
        Box(
          for {
            userPrincipalName <- extractValue(attr, "userPrincipalName")
            mail <- extractValue(attr, "mail")
            displayName <- extractValue(attr, "displayName")
          } yield UserAttributes(userPrincipalName, mail, displayName)
        )
    }
  }
}

trait DirectoryComponentImpl extends DirectoryComponent {
  val directory = new DirectoryImpl
}
