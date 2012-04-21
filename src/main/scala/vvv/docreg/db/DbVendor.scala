package vvv.docreg.db

import net.liftweb.http.LiftRules
import net.liftweb.mapper._
import vvv.docreg.model._
import net.liftweb.common.{Failure, Empty, Full, Box}
import net.liftweb.util.Props
import org.squeryl._
import internals.DatabaseAdapter
import org.squeryl.PrimitiveTypeMode._
import com.mchange.v2.c3p0.ComboPooledDataSource

trait DbVendor {
  lazy val driver = Props.get("db.driver") openOr "org.h2.Driver"
  lazy val adapter = Props.get("db.adapter") openOr "org.squeryl.adapters.H2Adapter"
  lazy val url = Props.get("db.url") openOr "jdbc:h2:mem:random"
  lazy val user = Props.get("db.user") openOr ""
  lazy val password = Props.get("db.password") openOr ""

  lazy val pool = {
    // Connection pooling with c3p0
    val pool = new ComboPooledDataSource
    pool.setDriverClass(driver)
    pool.setJdbcUrl(url)
    pool.setUser(user)
    pool.setPassword(password)
    pool.setMinPoolSize(3)
    pool.setAcquireIncrement(1)
    pool.setMaxPoolSize(10)

    // Work around MySQL connection timeouts.
    pool.setMaxIdleTimeExcessConnections(30 * 60) // 30 mins
    pool.setMaxConnectionAge(4 * 60 * 60) // 4 hours
    pool.setIdleConnectionTestPeriod(1 * 60 * 60) // 1 hour

    pool
  }

  def init() {
    Class.forName(driver)
    val adapterInstance: DatabaseAdapter = Class.forName(adapter).newInstance.asInstanceOf[DatabaseAdapter]
    SessionFactory.concreteFactory = Some(() => Session.create(pool.getConnection, adapterInstance))
  }

  def clear() {
    transaction {
      DbSchema.drop
      DbSchema.create
    }
  }

  def close() {
    pool.close()
  }

  def describe() {
    transaction {
      DbSchema.printDdl
    }
  }
}

object DbVendor extends DbVendor