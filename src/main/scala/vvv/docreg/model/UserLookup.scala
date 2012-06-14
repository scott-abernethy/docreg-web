package vvv.docreg.model

import net.liftweb.common.Failure._
import net.liftweb.common._
import vvv.docreg.db.{DbSchema, DbObject}
import org.squeryl.PrimitiveTypeMode._
import vvv.docreg.backend.{DirectoryConfig, UserAttributes, Directory}

trait UserLookupProvider
{
  def lookup(usernameOption: Option[String], emailOption: Option[String], nameOption: Option[String], why: String): Box[User]
}

class UserLookup extends DbObject[UserLookup] {
  def dbTable = DbSchema.userLookups
  var username: Option[String] = None
  var email: Option[String] = None
  var name: Option[String] = None
  var userId: Long = 0
}

object SystemUserAttributes extends UserAttributes {
  def userName() = Some("system.docreg")

  def email() = Some("system@docreg.x")

  def displayName() = Some("[System]")

  def dn() = None

  def department() = None

  def description() = None

  def location() = None

  def memberOf() = Nil
}

object UnknownUserAttributes extends UserAttributes {
  def userName() = Some("unknown.docreg")

  def email() = Some("unknown@docreg.x")

  def displayName() = Some("[Unknown]")

  def dn() = None

  def department() = None

  def description() = None

  def location() = None

  def memberOf() = Nil
}

object UserLookup extends UserLookup with Loggable {
  lazy val unknownUser = fromAttributes(UnknownUserAttributes, false)
  lazy val systemUser = fromAttributes(SystemUserAttributes, false)

  def installDefaults() {
    install(Some("smite"), None, Some("System"), SystemUserAttributes)
  }

  def install(usernameOption: Option[String], emailOption: Option[String], nameOption: Option[String], userAttributes: UserAttributes) {
    fromAttributes(userAttributes, false) match {
      case Full(user) => createLookup(usernameOption, emailOption, nameOption, Some(user))
      case _ => logger.error("Failed to install lookup")
    }
  }

  def lookup(usernameOption: Option[String], emailOption: Option[String], nameOption: Option[String], directory: Directory, why: String): Box[User] = {
    if (usernameOption.isEmpty && emailOption.isEmpty && nameOption.isEmpty) return Failure("Invalid input")
    
    val existing = from(UserLookup.dbTable)(l =>
      where(
        l.username === usernameOption and
        l.email === emailOption and
        l.name === nameOption
      )
      select(l)
    )

    val found = for {
      l <- existing.headOption
      u <- User.lookup(l.userId)
    } yield u
    found match {
      case Some(u) =>
        //logger.debug((usernameOption :: emailOption :: nameOption :: Nil) + " *** " + (record.user.map(_.username.is) :: record.user.map(_.email.is) :: record.user.map(_.name.is) :: Nil))
        Full(u)
      case _ =>
        val x = directoryLookup(usernameOption, emailOption, nameOption, directory) match {
          case Full(user) => Full(user)
          case _ => unknownUser
        }
        createLookup(usernameOption, emailOption, nameOption, x)
        logger.debug("UserLookup created for " + x + " from " + List(usernameOption, emailOption, nameOption) + " due to " + why)
        x
    }
  }

  def createLookup(usernameOption: Option[String], emailOption: Option[String], nameOption: Option[String], userOption: Option[User]) {
    userOption.foreach { i =>
      try {
        val x = new UserLookup
        x.username = usernameOption
        x.email = emailOption
        x.name = nameOption
        x.userId = i.id
        dbTable.insertOrUpdate(x)
        //logger.debug((usernameOption :: emailOption :: nameOption :: Nil) + " ??? " + (i.username.is :: i.email.is :: i.name.is :: Nil))
      } catch {
        case _ =>
      }
    }
  }
  
  def directoryLookup(usernameOption: Option[String], emailOption: Option[String], nameOption: Option[String], directory: Directory): Box[User] = {
    (
      // try name as a username
      lookupUser(nameOption, x => directory.findFromUserName(x.replaceAll(" ", "")))
    ) orElse (
      // try username
      lookupUser(usernameOption, x => directory.findFromUserName(x))
    ) orElse (
      // try email
      lookupUser(emailOption, x => directory.findFromMail(x))
    ) orElse (
      // try name
      lookupUser(nameOption, x => directory.findFromPartialName(x))
    )
  }

  protected def lookupUser(inputOption: Option[String], lookup: (String) => Box[UserAttributes]): Box[User] = {
    for {
      input <- inputOption
      foundUserAttributes <- lookup(input)
      user <- fromAttributes(foundUserAttributes, true)
    } yield user
  }

  def lookupUser(username: String, directory: Directory): Box[User] = {
    lookupUser(Some(username), x => directory.findFromUserName(x))
  }
  
  protected def fromAttributes(attributes: UserAttributes, active: Boolean): Box[User] = {
    attributes.userName() match {
      case Some(userName) => {
        from(User.dbTable)(u => where(u.username === userName) select(u)).headOption match {
          case Some(existing) =>
            updateUserAttributes(existing, attributes, active)
            User.dbTable.update(existing)
            parseUserAuthorizations(existing, attributes)
            Full(existing)
          case _ =>
            var created = new User
            created.username = userName
            updateUserAttributes(created, attributes, active)
            created.localServer = "boromir"
            created.timeZone = "US/Pacific"
            created = User.dbTable.insert(created)
            parseUserAuthorizations(created, attributes)
            Full( created )
        }
      }
      case _ => Failure("No username")
    }
  }



  def updateUserAttributes(user: User, attributes: UserAttributes, active: Boolean) {
    user.dn = attributes.dn() getOrElse "?"
    user.name = attributes.displayName() getOrElse "?"
    user.email = attributes.email() getOrElse "?"
    user.description = attributes.description() getOrElse ""
    user.department = attributes.department() getOrElse ""
    user.location = attributes.location() getOrElse ""
    user.active = active && parseUserAccess(attributes)
  }

  // TODO UserLookup and the directory should be actors

  def parseUserAccess(attributes: UserAttributes): Boolean = {
    attributes.memberOf().exists(DirectoryConfig.docRegUser.findFirstIn(_).isDefined)
  }

  def parseUserAuthorizations(user: User, attributes: UserAttributes) {
    var existing: Set[Long] = ProjectAuthorization.authorizedProjectsFor(user).map(_.id).toSet
    attributes.memberOf().foreach{
      _ match {
        case DirectoryConfig.docRegProject(projectName) => {
          Project.forName(projectName) match {
            case Some(p) if (existing.contains(p.id)) => {
              existing = existing - p.id
            }
            case Some(p) => {
              ProjectAuthorization.grant(user, p)
            }
            case _ => {}
          }
        }
        case _ => {}
      }
    }
    for (pid <- existing) ProjectAuthorization.revoke(user.id, pid)
  }
}
