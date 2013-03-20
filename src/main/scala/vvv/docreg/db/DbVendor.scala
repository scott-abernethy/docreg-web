/*
 * Copyright (c) 2013 Scott Abernethy.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package vvv.docreg.db

import net.liftweb.http.LiftRules
import net.liftweb.mapper._
import vvv.docreg.model._
import net.liftweb.common._
import org.squeryl._
import internals.DatabaseAdapter
import org.squeryl.PrimitiveTypeMode._
import com.mchange.v2.c3p0.ComboPooledDataSource
import org.streum.configrity.Configuration
import scala.Some

class DbVendor(config: Configuration) extends Loggable {
  lazy val driver = config.get[String]("db.driver") getOrElse "org.h2.Driver"
  lazy val adapter = config.get[String]("db.adapter") getOrElse "org.squeryl.adapters.H2Adapter"
  lazy val url = config.get[String]("db.url") getOrElse "jdbc:h2:mem:test;DB_CLOSE_DELAY=-1"
  lazy val user = config.get[String]("db.user") getOrElse ""
  lazy val password = config.get[String]("db.password") getOrElse ""

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