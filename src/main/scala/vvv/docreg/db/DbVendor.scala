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
  def init() {
//    val cm = new PooledConnectionManager
    val cm = LiftWikiConnectionVendor
    DB.defineConnectionManager(DefaultConnectionIdentifier, cm)
    LiftRules.unloadHooks.append(cm.close _)

    // Use Lift's Mapper ORM to populate the database
    // you don't need to use Mapper to use Lift... use
    // any ORM you want
    Schemifier.schemify(true, Schemifier.infoF _, User, Project, Document, Revision, Approval, Subscription, UserProject, UserLookup)
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

// Stolen from http://www.assembla.com/spaces/liftweb/wiki/Mapper
object LiftWikiConnectionVendor extends ConnectionManager {
  private var pool: List[Connection] = Nil
  private var poolSize = 0
  private val maxPoolSize = 4
  lazy val driver = Props.get("db.driver") openOr "org.h2.Driver"
  lazy val url = Props.get("db.url") openOr "jdbc:h2:mem:random"

  private def createOne: Box[Connection] = try {
    Class.forName(driver)

    val dm = (Props.get("db.user"), Props.get("db.password")) match {
      case (Full(user), Full(pwd)) =>
        DriverManager.getConnection(url, user, pwd)
      case _ => DriverManager.getConnection(url)
    }

    Full(dm)
  } catch {
    case e: Exception => e.printStackTrace; Empty
  }

  def newConnection(name: ConnectionIdentifier): Box[Connection] =
    synchronized {
      pool match {
        case Nil if poolSize < maxPoolSize =>
          val ret = createOne
          poolSize = poolSize + 1
          ret.foreach(c => pool = c :: pool)
          ret

        case Nil => wait(1000L); newConnection(name)
        case x :: xs => try {
          x.setAutoCommit(false)
          Full(x)
        } catch {
          case e => try {
            pool = xs
            poolSize = poolSize - 1
            x.close
            newConnection(name)
          } catch {
            case e => newConnection(name)
          }
        }
      }
    }

  def releaseConnection(conn: Connection): Unit = synchronized {
    pool = conn :: pool
    notify
  }

  def close() {
    // ?
  }

}

