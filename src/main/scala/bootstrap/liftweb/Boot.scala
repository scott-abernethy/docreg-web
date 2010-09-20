package bootstrap.liftweb

import net.liftweb._
import util._
import Helpers._

import common._
import http._
import sitemap._
import Loc._
import mapper._

import _root_.vvv.docreg.model._
import _root_.vvv.docreg.backend._
import _root_.vvv.docreg.util._


/**
 * A class that's instantiated early and run.  It allows the application
 * to modify lift's environment
 */
class Boot {
  def boot {
    if (!DB.jndiJdbcConnAvailable_?) {
      val vendor = 
	new StandardDBVendor(Props.get("db.driver") openOr "org.h2.Driver",
			     Props.get("db.url") openOr 
			     "jdbc:h2:lift_proto.db;AUTO_SERVER=TRUE",
			     Props.get("db.user"), Props.get("db.password"))

      LiftRules.unloadHooks.append(vendor.closeAllConnections_! _)

      DB.defineConnectionManager(DefaultConnectionIdentifier, vendor)
    }

    // Use Lift's Mapper ORM to populate the database
    // you don't need to use Mapper to use Lift... use
    // any ORM you want
    Schemifier.schemify(true, Schemifier.infoF _, User, Project, Document, Revision)

    // where to search snippet
    LiftRules.addToPackages("vvv.docreg")

    // Build SiteMap
    val entries = List(
      Menu.i("Home") / "index", // the simple way to declare a menu
      Nil) :::
    // the User management menu items
    User.sitemap

    // set the sitemap.  Note if you don't want access control for
    // each page, just comment this line out.
    //LiftRules.setSiteMap(SiteMap(entries:_*))

    // Force the request to be UTF-8
    LiftRules.early.append(_.setCharacterEncoding("UTF-8"))

    // What is the function to test if a user is logged in?
    LiftRules.loggedInTest = Full(() => User.loggedIn_?)

    // Make a transaction span the whole HTTP request
    S.addAround(DB.buildLoanWrapper)

    LiftRules.dispatch.append {
      case Req("doc" :: key :: "download" :: Nil, _, GetRequest) => 
        () => Download.download(key)
      case Req("doc" :: key :: version :: "download" :: Nil, _, GetRequest) => 
        () => Download.download(key, version)
    }

    LiftRules.statelessRewrite.append {
      case RewriteRequest(ParsePath(List("doc", key, "info"), _, _, _), _, _) =>
        RewriteResponse("doc" :: "info" :: Nil, Map("key" -> key))
    }

    DocumentServer start

    val backend = new Backend
    backend.start
    backend ! Connect()
  }
}
