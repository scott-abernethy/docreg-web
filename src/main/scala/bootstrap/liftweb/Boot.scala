package bootstrap.liftweb

import net.liftweb._
import common.Full
import http.Html5Properties
import http.ParsePath
import http.RewriteRequest
import http.RewriteResponse._
import net.liftweb.util._
import Helpers._

import common._
import http._
import sitemap._
import Loc._

import _root_.vvv.docreg.model._
import _root_.vvv.docreg.util._
import sitemap.Loc.If
import vvv.docreg.backend._
import vvv.docreg.db.DbVendor
import net.liftweb.widgets.flot._
import scala.actors.Actor
import org.squeryl.PrimitiveTypeMode._
import scala._
import vvv.docreg.model.Document.{DocumentRef, DocumentRevision}
import scala.Some

/**
 * A class that's instantiated early and run.  It allows the application
 * to modify lift's environment
 */
class Boot
{
  def boot
  {
    val db = new DbVendor(Config.is)
    db.init()
    //db.describe()

    // where to search snippet
    LiftRules.addToPackages("vvv.docreg")

    def loginRedirect = {
      User.requestUri(Some(S.uri))
      RedirectResponse("/user/signin")
    }
    val loggedIn = If(() => User.loggedIn_?, loginRedirect _)

    // Build SiteMap
    val entries: List[ConvertableToMenu] = List(
      Menu.i("Stream") / "index" >> loggedIn,
      Menu.i("Help") / "help",
      Menu.i("Search") / "search" >> loggedIn,
      Menu.i("Login") / "user" / "signin",
      Menu.i("Logout") / "user" / "signout" >> loggedIn,
      Menu.i("History") / "doc" / "history" >> loggedIn,
      Menu.i("Profile") / "user" / "profile" >> loggedIn,
      Menu.i("Preferences") / "user" / "preferences" >> loggedIn,
      Menu.i("Document Info") / "doc" / "info" >> loggedIn,
      Menu.i("Approve Document") / "doc" / "approve" >> loggedIn,
      Menu.i("Request Approval") / "doc" / "request-approval" >> loggedIn,
      Menu.i("Edit Document") / "doc" / "edit" >> loggedIn,
      Menu.i("Create Document") / "doc" / "create" >> loggedIn,
      Menu.i("Submit Document") / "doc" / "submit" >> loggedIn,
      Menu.i("Restricted Document") / "doc" / "restricted" >> loggedIn,
      Menu.i("Invalid Document") / "doc" / "invalid" >> loggedIn,
      Menu.i("Project Information") / "project" / "info" >> loggedIn,
      Menu.i("User Lookup Admin") / "admin" / "user-lookup" >> loggedIn,
      Menu.i("User Lookup Change Admin") / "admin" / "user-lookup-change" >> loggedIn,
      Menu.i("Authorized Users") / "admin" / "authorized-list" >> loggedIn
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
    Flot.init

    // Use HTML5 for rendering
    LiftRules.htmlProperties.default.set((r: Req) =>
      new Html5Properties(r.userAgent))

    // Make a transaction span the whole HTTP request
    //S.addAround(DB.buildLoanWrapper)
    S.addAround(new LoanWrapper{
      override def apply[T](f: => T): T = {
        inTransaction(f)
      }
    })

    LiftRules.ajaxStart = Full( () => LiftRules.jsArtifacts.show("loading").cmd )
    LiftRules.ajaxEnd = Full( () => LiftRules.jsArtifacts.hide("loading").cmd )

    def uploadViaDisk(fieldName: String, contentType: String, fileName: String, stream: java.io.InputStream): FileParamHolder = OnDiskFileParamHolder(fieldName, contentType, fileName, stream)

    val maxSize = 500 * 1024 * 1024
    LiftRules.maxMimeSize = maxSize
    LiftRules.maxMimeFileSize = maxSize
    LiftRules.handleMimeFile = uploadViaDisk

    def docIdParams(key: String, version: String): Map[String, String] = {
      if (version == null) {
        Map("key" -> key)
      }
      else {
        Map("key" -> key, "version" -> version.substring(1))
      }
    }

    // Rewrite ... (stateless)
    LiftRules.statelessRewrite.append {
      case RewriteRequest(ParsePath("docreg" :: something :: document :: Nil, suffix, _, _), _, _) =>
        RewriteResponse(document :: Nil)
    }
    // Rewrite ... (stateful, so S is valid here)
    LiftRules.statefulRewrite.append {
      case RewriteRequest(ParsePath((ref @ Document.DocumentRef(number, version)) :: action, s, a, es), _, _) => {
        inTransaction {
          ref match {
            case DocumentRevision(document, revision) => {
              (User.loggedInUser.is.filter(document.allows(_)).isDefined, action) match {
                case (false, _) => {
                  // This rewrite will occur for cases where user is not logged in.
                  RewriteResponse(ParsePath("doc" :: "restricted" :: Nil, s, a, es), Map("number" -> document.number, "version" -> revision.version.toString), true)
                }
                case (_, Nil) => {
                  RewriteResponse(ParsePath("doc" :: "info" :: Nil, s, a, es), Map("key" -> document.number, "version" -> revision.version.toString), true)
                }
                case (_, "download" :: tail) => {
                  RewriteResponse(ParsePath("doc" :: "download" :: tail ::: document.number :: revision.version.toString :: Nil, s, a, es), Map.empty, true)
                }
                case (_, "log" :: Nil) => {
                  RewriteResponse(ParsePath("doc" :: "log" :: document.number:: Nil, s, a, es), Map.empty, true)
                }
                case (_, x) => {
                  RewriteResponse(ParsePath("doc" :: x, s, a, es), Map("key" -> document.number, "version" -> revision.version.toString), true)
                }
              }
            }
            case _ => {
              RewriteResponse(ParsePath("doc" :: "invalid" :: Nil, s, a, es), Map("number" -> number, "version" -> version.toString), true)
            }
          }
        }
      }
      case RewriteRequest(ParsePath(Document.ValidIdentifier(key, version) :: action :: Nil, _, _, _), _, _) if (action != "download" && action != "log") => {
        RewriteResponse("doc" :: action :: Nil, docIdParams(key, version))
      }
      case RewriteRequest(ParsePath("user" :: x :: Nil, suffix, absolute, endSlash), _, _) if (x != "signin" && x != "signout" && x != "profile") => {
        RewriteResponse(ParsePath("user" :: "profile" :: Nil, suffix, absolute, endSlash), Map("user" -> x), true)
      }
      case RewriteRequest(ParsePath("user" :: user :: action :: Nil, suffix, absolute, endSlash), _, _) => {
        RewriteResponse(ParsePath("user" :: action :: Nil, suffix, absolute, endSlash), Map("user" -> user), true)
      }
      case RewriteRequest(ParsePath("project" :: key :: Nil, suffix, absolute, endSlash), _, _) => {
        RewriteResponse(ParsePath("project" :: "info" :: Nil, suffix, absolute, endSlash), Map("key" -> key), true)
      }
    }

    // ... then dispatch ... (S is valid here)
    LiftRules.dispatch.append(DownloadService)

    LiftRules.noticesAutoFadeOut.default.set( (notices: NoticeType.Value) => {
            notices match {
              case NoticeType.Notice => Full((10 seconds, 2 seconds))
              case _ => Empty
            }
         }
        )

    val env = new EnvironmentImpl{}
    env.start()
    Environment.env = env
    LiftRules.unloadHooks.append(() => {
      env.exit()
      db.close()
    })
  }
}
