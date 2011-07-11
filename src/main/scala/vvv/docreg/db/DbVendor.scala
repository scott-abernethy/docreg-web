package vvv.docreg.db

import net.liftweb.db.StandardDBVendor
import net.liftweb.util.Props
import net.liftweb.http.LiftRules
import net.liftweb.mapper._
import vvv.docreg.model._
import java.sql.Connection
import com.mchange.v2.c3p0.ComboPooledDataSource
import net.liftweb.common.{Failure, Empty, Full, Box}

trait DbVendor {
  def init() {
    val cm = new PooledConnectionManager
    DB.defineConnectionManager(DefaultConnectionIdentifier, cm)
    LiftRules.unloadHooks.append(cm.close _)

    // Use Lift's Mapper ORM to populate the database
    // you don't need to use Mapper to use Lift... use
    // any ORM you want
    Schemifier.schemify(true, Schemifier.infoF _, User, Project, Document, Revision, Approval, Subscription, UserProject)
  }
}

object DbVendor extends DbVendor

class PooledConnectionManager extends ConnectionManager {
  lazy val driver = Props.get("db.driver") openOr "org.h2.Driver"
  lazy val url = Props.get("db.url") openOr "jdbc:h2:mem:random"
  lazy val user = Props.get("db.user") openOr ""
  lazy val password = Props.get("db.password") openOr ""
  lazy val pool = {
    Class.forName(driver)
    val pool = new ComboPooledDataSource
    // Setup connection pooling with c3p0
    pool.setDriverClass(driver)
    pool.setJdbcUrl(url)
    pool.setUser(user)
    pool.setPassword(password)
    pool.setMinPoolSize(3)
    pool.setAcquireIncrement(1)
    pool.setMaxPoolSize(10)
    pool
  }

  def newConnection(name: ConnectionIdentifier): Box[Connection] = {
    try {
      Full(pool.getConnection())
    } catch {
      case e => Failure("Failed to get connection", Full(e), Empty)
    }
  }

  def releaseConnection(conn: Connection) {
    try {
      conn.close()
    } catch {
      case e =>
    }
  }

  def close() {
    pool.close()
  }
}

