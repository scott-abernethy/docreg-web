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
description: Software Lead Designer
title: Software Lead Designer
displayName: Scott Abernethy
distinguishedName: CN=SAbernethy,OU=NZ,OU=People,OU=APAC,DC=GNET,DC=global,DC=vpn
mobile: +64 21 442 473
department: Software Engineering
co: New Zealand
cn: SAbernethy
l: Lower Hutt
st: Wellington
c: NZ
physicalDeliveryOfficeName: New Zealand
telephoneNumber: +64 4 5778 913
proxyAddresses: sip:Scott.Abernethy@Aviatnet.com, smtp:Scott.Abernethy@Aviatnetworks.com, smtp:Scott_Abernethy@stratexnet.com, smtp:Scott.Abernethy@HSTX.co.nz, X400:c=NZ;a= ;p=DMCWave;o=WENZ;s=Abernethy;g=Scott;, smtp:Scott.Abernethy@HarrisStratex.com, x400:c=NZ;a= ;p=DMCWave;o=WENZ;s=sabernethy;, SMTP:Scott.Abernethy@Aviatnet.com, smtp:Scott.Abernethy@HSTX.com
objectClass: top, person, organizationalPerson, user
objectCategory: CN=Person,CN=Schema,CN=Configuration,DC=global,DC=vpn
memberOf: CN=DocRegProject,OU=OpenKM,OU=New Zealand,OU=Groups,OU=APAC,DC=GNET,DC=global,DC=vpn, CN=STXN Provision Development Team,OU=Distribution List,OU=Messaging,OU=APAC,DC=HSTX,DC=global,DC=vpn, ...
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
    tryo(searchIt("(&(objectCategory=person)(" + filter + "))")) match {
      case Full(Nil) => Empty
      case Full(dn :: Nil) => findFromDn(dn)
      case Full(dn :: more) => Failure("Multiple users found " + (dn :: more))
      case _ => Failure("Exception encoutered during search for '" + filter + "'")
    }
  }

  def findFromDn(dn: String): Box[UserAttributes] = {
    def extractValue(attr: Attributes, key: String): Option[String] = Option(attr).map(_.get(key)).filter(_ != null).map(_.get()).filter(_ != null).map(_.toString)
    tryo(attributesFromDn(dn + "," + ldapBase)) match {
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
