package vvv.docreg.model

import net.liftweb.mapper._
import net.liftweb.common.Failure._
import vvv.docreg.backend.{UserAttributes, Directory}
import net.liftweb.common._

trait UserLookupProvider
{
  def lookup(usernameOption: Option[String], emailOption: Option[String], nameOption: Option[String], why: String): Box[User]
}

class UserLookup extends LongKeyedMapper[UserLookup] with IdPK {
  def getSingleton = UserLookup
  object username extends MappedString(this, 64)
  object email extends MappedString(this, 64)
  object name extends MappedString(this, 64)
  object user extends MappedLongForeignKey(this, User)
}

object UserLookup extends UserLookup with LongKeyedMetaMapper[UserLookup] with Loggable {
  override def dbIndexes = UniqueIndex(username, email, name) :: super.dbIndexes

  val unknownUserAttributes = UserAttributes("unknown.docreg", "unknown@docreg.x", "[Unknown]")
  val systemUserAttributes = UserAttributes("system.docreg", "system@docreg.x", "[System]")
  lazy val unknownUser = fromAttributes(unknownUserAttributes, false)

  def installDefaults() {
    install(Some("smite"), None, Some("System"), systemUserAttributes)
  }

  def install(usernameOption: Option[String], emailOption: Option[String], nameOption: Option[String], userAttributes: UserAttributes) {
    fromAttributes(userAttributes, false) match {
      case Full(user) => createLookup(usernameOption, emailOption, nameOption, Some(user))
      case _ => logger.error("Failed to install lookup")
    }
  }

  def lookup(usernameOption: Option[String], emailOption: Option[String], nameOption: Option[String], directory: Directory, why: String): Box[User] = {
    if (usernameOption.isEmpty && emailOption.isEmpty && nameOption.isEmpty) return Failure("Invalid input")
    
    val existing = UserLookup.find(
      By(UserLookup.username, usernameOption.getOrElse("")),
      By(UserLookup.email, emailOption.getOrElse("")),
      By(UserLookup.name, nameOption.getOrElse(""))
    )
    existing match {
      case Full(record) =>
        //logger.debug((usernameOption :: emailOption :: nameOption :: Nil) + " *** " + (record.user.map(_.username.is) :: record.user.map(_.email.is) :: record.user.map(_.name.is) :: Nil))
        record.user
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
        UserLookup.create.username(usernameOption.getOrElse("")).email(emailOption.getOrElse("")).name(nameOption.getOrElse("")).user(i).save
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

  def lookupUser(inputOption: Option[String], lookup: (String) => Box[UserAttributes]): Option[User] = {
    for {
      input <- inputOption
      foundUserAttributes <- lookup(input)
      user <- fromAttributes(foundUserAttributes, true)
    } yield user
  }

  // todo move to User?
  def fromAttributes(attributes: UserAttributes, active: Boolean): Box[User] = {
    // todo put back to find via username when other email dependant user creation tasks have gone.
    User.find(
      By(User.email, attributes.mail)
    ) match {
      case Full(existing) =>
        Full(existing)
      case _ =>
        val created = User.create.name(attributes.displayName).email(attributes.mail).username(attributes.userName).active(active).localServer("boromir").timeZone("US/Pacific")
        created.save
        Full(created)
    }
  }
}
