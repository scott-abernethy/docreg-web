/*
 * Copyright (c) 2013 Scott Abernethy.
 * This file is part of DocReg+Web. Please refer to the NOTICE.txt file for license details.
 */

package vvv.docreg.backend

import net.liftweb.ldap._
import net.liftweb.util.ControlHelpers._
import net.liftweb.common.{Full, Failure, Empty, Box}
import vvv.docreg.model.User
import vvv.docreg.util.StringUtil.ValidEmail
import vvv.docreg.util.Config

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

trait Directory {
  def findFromMail(mailUserName: String): Box[UserAttributes]
  def findFromUserName(userName: String): Box[UserAttributes]
  def findFromPartialName(partialName: String): Box[UserAttributes]
  def findAttributes(dn: String): Box[UserAttributes]
  def groupMembers(dn: String): Box[List[String]]
  def login(dn: String, password: String): Boolean
}

trait DirectoryComponent {
  val directory: Directory
}

/*
LDAP port 389?
Microsoft's AD LDAP is on port 3268 (and Global Catalog server)
 */

class DirectoryImpl extends LDAPVendor with Directory {
  configure(
    Map(
      "ldap.url" -> (Config.is.get[String]("ldap.url") getOrElse "ldap://dcgnetnz1.gnet.global.vpn:3268"),
      "ldap.userName" -> (Config.is.get[String]("ldap.user") getOrElse "gnet\\sabernethy"),
      "ldap.password" -> (Config.is.get[String]("ldap.password") getOrElse "notsecureenuf"),
      "ldap.base" -> DirectoryConfig.ldapBase,
      "lift-ldap.testLookup" -> DirectoryConfig.testLookup,
      "lift-ldap.retryInterval" -> "1000",
      "lift-ldap.maxRetries" -> "10"
    )
  )

  def searchIt(filter: String): List[String] = {
    val sc = new javax.naming.directory.SearchControls()
    sc.setSearchScope(javax.naming.directory.SearchControls.SUBTREE_SCOPE)
    // todo restrict search to attributes we care about?
    searchControls.doWith(sc) {
      search(filter)
    }
  }

  def partialNameFilter(partialName: String) = {
    "displayName=" + partialName.replaceAll(" ", "*") + ""
  }

  def mailFilter(mail: String) = {
    "mail=" + mail
  }

  def userNameFilter(userName: String) = {
    val u = userName match
    {
      case ValidEmail(name, domain) =>
        name.replaceAll("\\.", "")
      case other =>
        other
    }
    "userPrincipalName=" + u + User.domain
  }

  def findFromPartialName(partialName: String): Box[UserAttributes] = {
    find(partialNameFilter(partialName))
  }

  def findFromMail(mail: String): Box[UserAttributes] = {
    find(mailFilter(mail))
  }

  def findFromUserName(userName: String): Box[UserAttributes] = {
    find(userNameFilter(userName))
  }

  def find(filter: String): Box[UserAttributes] = {
    dn(filter).flatMap(findAttributes(_))
  }

  def dn(filter: String): Box[String] = {
    tryo(searchIt("(&(objectCategory=person)(" + filter + "))")) match {
      case Full(Nil) => Empty
      case Full(dn :: Nil) => Full(dn)
      case Full(dn :: more) => Failure("Multiple users found " + (dn :: more))
      case _ => Failure("Exception encoutered during search for '" + filter + "'")
    }
  }

  def findAttributes(dn: String): Box[UserAttributes] = {
    tryo(attributesFromDn(dn + "," + DirectoryConfig.ldapBase)) match {
      case Full(attrs) if (attrs != null) => Full(new NamingUserAttributes(dn,attrs))
      case _ => Empty
    }
  }

  def groupMembers(dn: String): Box[List[String]] = {
    tryo(attributesFromDn(dn + "," + DirectoryConfig.ldapBase)) match {
      case Full(attrs) if (attrs != null) =>
        Full(new GroupAttributes(attrs).members().map(_.replace(","+DirectoryConfig.ldapBase, "")))
      case f @ Failure(_, _, _) => f
      case _ => Empty
    }
  }

  def login(dn: String, password: String): Boolean = {
    if (!password.isEmpty) {
      bindUser(dn, password)
    }
    else {
      false
    }
  }
}

trait UserAttributes {
  def userName(): Option[String]

  def email(): Option[String]

  def displayName(): Option[String]

  def dn(): Option[String]

  def department(): Option[String]

  def description(): Option[String]

  def location(): Option[String]

  def memberOf(): List[String]

  def debug() = {}
}

class AttributesWrapper(val attrs: javax.naming.directory.Attributes) {
  def extractValue(key: String): Option[String] = {
    Option( attrs.get(key) ).flatMap( x => Option(x.get()) ).map(_.toString)
  }

  def extractList(key: String): List[String] = {
    val listAttr = attrs.get(key)
    if (listAttr != null) {
      tryo(lameEnumToListOfString(listAttr.getAll)).getOrElse(List.empty[Any]).map(_.toString())
    }
    else {
      List.empty
    }
  }

  def lameEnumToListOfString(enum: javax.naming.NamingEnumeration[_]): List[String] = {
    if (!enum.hasMore) {
      Nil
    }
    else {
      enum.next().toString :: lameEnumToListOfString(enum)
    }
  }

  def lameEnumToList[T](enum: javax.naming.NamingEnumeration[T]): List[T] = {
    if (!enum.hasMore) {
      Nil
    }
    else {
      enum.next() :: lameEnumToList(enum)
    }
  }
}

class NamingUserAttributes(val foundDn: String, attrs: javax.naming.directory.Attributes) extends AttributesWrapper(attrs) with UserAttributes {

  def dn() = Some(foundDn)

  def userName() = extractValue("userPrincipalName")

  def email() = extractValue("mail")

  def displayName() = extractValue("displayName")

  def department() = extractValue("department")

  def description() = extractValue("title")

  def location() = extractValue("physicalDeliveryOfficeName")

  def memberOf(): List[String] = {
    val memberOf = attrs.get("memberOf")
    if (memberOf != null) {
      tryo(lameEnumToListOfString(memberOf.getAll)).getOrElse(List.empty[Any]).map(_.toString())
    }
    else {
      List.empty
    }
  }

  override def debug() {
    lameEnumToList(attrs.getAll).foreach(x => println(x.getID + " -> " + lameEnumToListOfString(x.getAll)))
  }
}

class GroupAttributes(attrs: javax.naming.directory.Attributes) extends AttributesWrapper(attrs) {
  def members() = extractList("member")
}

trait DirectoryComponentImpl extends DirectoryComponent {
  val directory = new DirectoryImpl
}

object DirectoryConfig {
  val ldapBase = "DC=GNET,DC=global,DC=vpn"
  val testLookup = "CN=DocRegUser,OU=DocReg,OU=New Zealand,OU=Groups,OU=APAC,DC=GNET,DC=global,DC=vpn"
  val docRegUser = """^CN=DocRegUser,OU=DocReg,OU=New Zealand,OU=Groups,OU=APAC,DC=GNET,DC=global,DC=vpn$""".r
  val docRegProject = """^CN=DocRegProject(.*),OU=DocReg,OU=New Zealand,OU=Groups,OU=APAC,DC=GNET,DC=global,DC=vpn$""".r

  def userGroup(): String = {
    "CN=DocRegUser,OU=DocReg,OU=New Zealand,OU=Groups,OU=APAC"
  }

  def projectAuthorizationGroup(project: String): String = {
    "CN=DocRegProject%s,OU=DocReg,OU=New Zealand,OU=Groups,OU=APAC".format(project)
  }
}

object DirectoryTest {

  def stress(users: List[(String,String)]) {
    val d = new DirectoryImpl
    var success: Long = 0
    var failure: Long = 0
    for (i <- List.range(1, 100000)) {
      for (u <- users) {
        if (testLogin(d, u._1, u._2)) {
          success = success + 1
        }
        else {
          failure = failure + 1
        }
      }
      if (i % 10 == 0) {
        println("[" + failure + "] of " + (success+failure))
      }
    }
  }

  def testLogin(d: DirectoryImpl, userName: String, password: String): Boolean = {
    d.dn(d.userNameFilter(userName)) match {
      case Full(dn) => d.bindUser(dn, password)
      case _ => false
    }
  }

}

/*
groups have
member - list
objectClass - list (top, group)
whenChanged
*/