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

package vvv.docreg.snippet

import xml.{Text, NodeSeq}
import vvv.docreg.model.{User, Project}
import net.liftweb.util.Helpers._
import vvv.docreg.backend.Create
import vvv.docreg.util.{StringUtil, Environment}
import net.liftweb.http._
import net.liftweb.common.{Loggable, Full}

class CreateDocumentSnippet extends Loggable
{
  private val projects = Project.findAllUsed()
  private object name extends RequestVar("")
  private object nameError extends RequestVar[Option[String]](None)
  private object project extends RequestVar(projects.headOption.map(_.name).getOrElse("???"))
  private object file extends RequestVar[Option[FileParamHolder]](None)
  private object fileError extends RequestVar[Option[String]](None)
  private object comment extends RequestVar("")

  def render =
  {
    val projectList = projects.map(i => (i.name, i.name))
    ".submission-project" #> SHtml.select(projectList, Option(project.is), project(_)) &
    ".submission-name" #> SHtml.text(name.is, name(_), "maxlength" -> "110") &
    "#name-group [class+]" #> nameError.is.map(_ => "error").getOrElse("") &
    ".submission-version *" #> "1" &
    ".submission-file" #> SHtml.fileUpload(ul => file(Some(ul))) &
    "#file-group [class+]" #> fileError.is.map(_ => "error").getOrElse("") &
    ".submission-by *" #> Text(User.loggedInUser map (_.displayName) openOr "?") &
    ".submission-comment" #> SHtml.textarea(comment.is, comment(_), "class" -> "input-xlarge", "maxlength" -> "512") &
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
            logger.debug("Upload " + f.localFile + " as " + f.fileName + " new")

            // Note: OnDiskFileParamHolder will delete the local file on finalize, so pass the local file in a wrapper such that it is maintained til needed.
            Environment.env.backend ! Create(project.is, () => f.localFile, StringUtil.retitleFile(name.is, f.fileName).getOrElse(f.fileName), comment.is, user)
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
