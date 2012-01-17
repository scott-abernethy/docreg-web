package vvv.docreg.db

import net.liftweb.db.StandardDBVendor
import net.liftweb.util.Props
import net.liftweb.http.LiftRules
import net.liftweb.mapper._
import vvv.docreg.model._
import com.mchange.v2.c3p0.ComboPooledDataSource
import net.liftweb.common.{Failure, Empty, Full, Box}
import java.sql.{DriverManager, Connection}

trait DbVendor {
  lazy val driver = Props.get("db.driver") openOr "org.h2.Driver"
  lazy val url = Props.get("db.url") openOr "jdbc:h2:mem:random"
  lazy val user = Props.get("db.user")
  lazy val password = Props.get("db.password")

  def init() {
    if (!DB.jndiJdbcConnAvailable_?) {
      val vendor = new StandardDBVendor(driver, url, user, password)
      LiftRules.unloadHooks.append(vendor.closeAllConnections_! _)
      DB.defineConnectionManager(DefaultConnectionIdentifier, vendor)
    }

    // Use Lift's Mapper ORM to populate the database
    // you don't need to use Mapper to use Lift... use
    // any ORM you want
    Schemifier.schemify(true, Schemifier.infoF _, User, Project, Document, Revision, Approval, Subscription, UserProject, UserLookup, Pending)
  }
}

object DbVendor extends DbVendor