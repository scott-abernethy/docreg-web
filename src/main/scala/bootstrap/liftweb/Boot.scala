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
import _root_.vvv.docreg.util._
import vvv.docreg.backend._
import vvv.docreg.db.DbVendor

/**
 * A class that's instantiated early and run.  It allows the application
 * to modify lift's environment
 */
class Boot {
  def boot {
    DbVendor.init()

    // where to search snippet
    LiftRules.addToPackages("vvv.docreg")

    val loggedIn = If(() => User.loggedIn_?, () => RedirectResponse("/user/signin"))

    // Build SiteMap
    val entries: List[ConvertableToMenu] = List(
      Menu.i("Home") / "index" >> loggedIn,
      Menu.i("Login") / "user" / "signin",
      Menu.i("Logout") / "user" / "signout" >> loggedIn,
      Menu.i("Register") / "user" / "register",
      Menu.i("Profile") / "user" / "profile" >> loggedIn,
      Menu.i("Info") / "doc" / "info" >> loggedIn,
      Menu.i("Approve") / "doc" / "approve" >> loggedIn,
      Menu.i("Request Approval") / "doc" / "request-approval" >> loggedIn,
      Menu.i("Submit") / "doc" / "submit" >> loggedIn
    )

    // set the sitemap.  Note if you don't want access control for
    // each page, just comment this line out.
    LiftRules.setSiteMapFunc(() => SiteMap(entries:_*))

    // Use jQuery 1.4
    LiftRules.jsArtifacts = net.liftweb.http.js.jquery.JQuery14Artifacts

    // Force the request to be UTF-8
    LiftRules.early.append(_.setCharacterEncoding("UTF-8"))

    // What is the function to test if a user is logged in?
    LiftRules.loggedInTest = Full(() => User.loggedIn_?)

    // Use HTML5 for rendering
//    LiftRules.htmlProperties.default.set((r: Req) =>
//      new Html5Properties(r.userAgent))    

    // Make a transaction span the whole HTTP request
    S.addAround(DB.buildLoanWrapper)

    LiftRules.ajaxStart = Full( () => LiftRules.jsArtifacts.show("loading").cmd )
    LiftRules.ajaxEnd = Full( () => LiftRules.jsArtifacts.hide("loading").cmd )

    LiftRules.dispatch.append {
      case Req("d" :: key :: "download" :: Nil, _, GetRequest) => 
        () => Download.download(key)
      case Req("d" :: key :: "v" :: version :: "download" :: Nil, _, GetRequest) => 
        () => Download.download(key, version)
    }

    LiftRules.statelessRewrite.append {
      case RewriteRequest(ParsePath("d" :: key :: Nil, _, _, _), _, _) =>
        RewriteResponse("doc" :: "info" :: Nil, Map("key" -> key))
      case RewriteRequest(ParsePath("d" :: key :: "v" :: version :: Nil, _, _, _), _, _) =>
        RewriteResponse("doc" :: "info" :: Nil, Map("key" -> key, "version" -> version))
      case RewriteRequest(ParsePath("d" :: key :: "v" :: version :: "approve" :: Nil, _, _, _), _, _) =>
        RewriteResponse("doc" :: "approve" :: Nil, Map("key" -> key, "version" -> version))
      case RewriteRequest(ParsePath("d" :: key :: "v" :: version :: "submit" :: Nil, _, _, _), _, _) =>
        RewriteResponse("doc" :: "submit" :: Nil, Map("key" -> key, "version" -> version))
      case RewriteRequest(ParsePath("d" :: key :: "v" :: version :: "request-approval" :: Nil, _, _, _), _, _) =>
        RewriteResponse("doc" :: "request-approval" :: Nil, Map("key" -> key, "version" -> version))
      case RewriteRequest(ParsePath("user" :: user :: "profile" :: Nil, _, _, _), _, _) =>
        RewriteResponse("user" :: "profile" :: Nil, Map("user" -> user))
    }

    val env = new Environment with BackendComponentImpl with DocumentServerComponentImpl with AgentComponentImpl {
      def start() {
        documentServer.start()
        backend.start()
        backend ! Connect()
      }
    }
    env.start()
    Environment.env = env
  }
}
