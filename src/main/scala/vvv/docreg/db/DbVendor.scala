/*
 * Copyright (c) 2013 Aviat Networks.
 * This file is part of DocReg+Web. Please refer to the NOTICE.txt file for license details.
 */

package vvv.docreg.db

import net.liftweb.http.LiftRules
import net.liftweb.mapper._
import vvv.docreg.model._
import net.liftweb.common._
import org.squeryl._
import org.squeryl.internals.DatabaseAdapter
import org.squeryl.PrimitiveTypeMode._
import com.mchange.v2.c3p0.ComboPooledDataSource
import scala.Some
import net.liftweb.util.Props

class DbVendor(maxPoolSize: Int = 10) extends Loggable {
  lazy val driver = Props.get("db.driver").openOr("org.h2.Driver")
  lazy val adapter = Props.get("db.adapter").openOr("org.squeryl.adapters.H2Adapter")
  lazy val url = Props.get("db.url").openOr("jdbc:h2:mem:;MODE=MySQL")
  lazy val user = Props.get("db.user").openOr("")
  lazy val password = Props.get("db.password").openOr("")

  lazy val pool = {
    // Connection pooling with c3p0
    val pool = new ComboPooledDataSource
    pool.setDriverClass(driver)
    pool.setJdbcUrl(url)
    pool.setUser(user)
    pool.setPassword(password)
    pool.setMinPoolSize(1)
    pool.setAcquireIncrement(1)
    pool.setMaxPoolSize(maxPoolSize)

    // Work around MySQL connection timeouts.
    pool.setMaxIdleTime(1 * 60 * 60) // 1 hour
    pool.setMaxConnectionAge(6 * 60 * 60) // 6 hours
    pool.setIdleConnectionTestPeriod(1 * 60 * 60) // 1 hour
    pool.setPreferredTestQuery("SELECT 1")
    pool.setAcquireRetryAttempts(30)
    pool.setAcquireRetryDelay(1000) // 1 second (this setting is in millis)

    pool
  }

  def init() {
    Class.forName(driver)
    val adapterInstance: DatabaseAdapter = Class.forName(adapter).newInstance.asInstanceOf[DatabaseAdapter]
    SessionFactory.concreteFactory = Some(() => Session.create(pool.getConnection, adapterInstance))
    logger.info("Database init on " + url)
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
