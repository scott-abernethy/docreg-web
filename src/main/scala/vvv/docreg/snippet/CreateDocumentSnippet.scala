package vvv.docreg.snippet

import xml.{Text, NodeSeq}
import vvv.docreg.model.{User, Project}
import net.liftweb.common.Full
import net.liftweb.util.Helpers._
import vvv.docreg.backend.Create
import vvv.docreg.util.{StringUtil, Environment}
import net.liftweb.http._

class CreateDocumentSnippet
{
  private val projects = Project.findAll()
  private object name extends RequestVar("")
  private object nameError extends RequestVar[Option[String]](None)
  private object project extends RequestVar(projects.headOption.map(_.name.is).getOrElse("???"))
  private object file extends RequestVar[Option[FileParamHolder]](None)
  private object fileError extends RequestVar[Option[String]](None)
  private object comment extends RequestVar("")

  def render =
  {
    val projectList = projects.map(i => (i.name.is, i.name.is))
    ".submission-project" #> SHtml.select(projectList, Option(project.is), project(_)) &
    ".submission-name" #> SHtml.text(name.is, name(_)) &
    "#name-group [class+]" #> nameError.is.map(_ => "error").getOrElse("") &
    ".submission-version *" #> "1" &
    ".submission-file" #> SHtml.fileUpload(ul => file(Some(ul))) &
    "#file-group [class+]" #> fileError.is.map(_ => "error").getOrElse("") &
    ".submission-by *" #> Text(User.loggedInUser map (_.displayName) openOr "?") &
    ".submission-comment" #> SHtml.textarea(comment.is, comment(_), "class" -> "input-xlarge") &
    ".submission-submit" #> SHtml.submit("Submit", () => processCreate(), "class" -> "btn primary") &
    ".submission-cancel" #> SHtml.submit("Cancel", () => S.redirectTo("/"), "class" -> "btn")
  }

  private def processCreate()
  {
    // todo refactor with processSubmit
    file.is match {
      case Some(f: OnDiskFileParamHolder) if f.mimeType == null =>
      {
        fileError(Some("No file uploaded!"))
      }
      case Some(f: OnDiskFileParamHolder) if name.is == "" =>
      {
        nameError(Some("Name must be entered!"))
        fileError(Some("Please re-select file"))
      }
      case Some(f: OnDiskFileParamHolder) =>
      {
        User.loggedInUser.is match {
          case Full(user) =>
            println("send " + f.localFile + " as " + f.fileName + " new")

            Environment.env.backend ! Create(project.is, f.localFile, StringUtil.retitleFile(name.is, f.fileName).getOrElse(f.fileName), comment.is, user)
            S.notice(<div class="alert-message info"><p>Document created, waiting for system to update...</p></div>)
            S.redirectTo("/")
          case _ =>
            S.error(<div class="alert-message error"><p>Unable to submit, no user logged in!</p></div>)
            S.redirectTo("/")
        }
      }
      case _ =>
      {
        fileError(Some("No file selected!"))
      }
    }
  }
}
