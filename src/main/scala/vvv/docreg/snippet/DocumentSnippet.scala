/*
 * Copyright (c) 2013 Scott Abernethy.
 * This file is part of DocReg+Web. Please refer to the NOTICE.txt file for license details.
 */

package vvv.docreg.snippet

import vvv.docreg.backend._
import vvv.docreg.model.ApprovalState._
import net.liftweb._
import http._
import mapper._
import util._
import common._
import Helpers._
import js.JE.JsRaw
import js.jquery.JqJE._
import js._
import js.jquery._
import scala.xml.{NodeSeq, Text, Elem}
import net.liftweb.http.js.JsCmds._
import java.util.Date
import vvv.docreg.util.{Environment, StringUtil, TemplateParse}
import vvv.docreg.model._
import java.sql.Timestamp
import org.apache.tika.Tika

trait DocumentRequest
{

}

class DocumentSnippet extends DocumentRequest with Loggable {
  val backend = Environment.env.backend
  val key = S.param("key") openOr ""
  val version = S.param("version") openOr "latest"
  val document: Box[Document] = try {
    Document.forKey(key)
  } catch {
    case e:NumberFormatException => null
  }
  val revision: Box[Revision] = document match {
    case Full(d) =>
      if (version == "latest") {
        Full(d.latest)
      } else {
        Revision.forDocument(d, version.toLong)
      }
    case _ => Empty
  }
  val user: Box[User] = User.loggedInUser.is
  val pageUserId: Long = user.map(_.id).getOrElse(-1L)
  val editor: List[Pending] = {
    document match {
      case Full(d) => Pending.forAction(d, PendingAction.editing)
      case _ => Nil
    }
  }
  val userPendings: List[Pending] = {
    (document, user) match {
      case (Full(d), Full(u)) => Pending.forUserDocument(u, d)
      case _ => Nil
    }
  }
  val userIsActingAsEditor: Boolean = {
    userPendings.exists(p =>
      p.action == PendingAction.editing ||
      p.action == PendingAction.editRequest
    ) &&
    !userPendings.exists(p =>
      p.action == PendingAction.editCancel
    )
  }

  def editing_?(): Boolean =
  {
    editor != Nil
  }

  def header(in: NodeSeq): NodeSeq = {
    (document, revision) match
    {
      case (Full(d), Full(r)) =>
      {
        if (editor != Nil) S.notice("edit-message", <div class="alert alert-warning"><strong>Under edit!</strong> This document is currently being edited.</div>)
        if (!d.latest_?(r.version)) S.warning("out-of-date-message", <div class="alert alert-info"><strong>Out of date!</strong> The link you followed refers to a version that is not the most recent version of the document. The Download, Approve, and Request Approval options will take action on that out-of-date version. You may want to <a href={d.infoHref()}>see the most recent version</a> instead.</div>)
        ".doc-title" #> <a href={d.infoLink}>{d.fullTitle}</a> &
        ".doc-access" #> d.accessIcon() &
        ".doc-project" #> d.project().map(_.infoLink).getOrElse(<span>?</span>) apply(in)
      }
      case (Full(d), _) =>
      {
        <div class="alert alert-error">
            <strong>Invalid Version</strong>{" '" + version + "' for document '" + key + "'. "}<a href={d.infoLink}>Get the latest version instead.</a>
        </div>
      }
      case _ =>
      {
        <div class="alert alert-error">
          <strong>Invalid Document</strong>{" '" + key + "'"}
        </div>
      }
    }
  }

  def info(in: NodeSeq): NodeSeq =
  {
    (document, revision) match
    {
      case (Full(d), Full(r)) =>
      {
        import org.squeryl.PrimitiveTypeMode._
        val rs = inTransaction{
          join(Revision.dbTable, User.dbTable, Approval.dbTable.leftOuter, User.dbTable.leftOuter)( (r,u,a,u2) =>
            where(r.documentId === d.id)
              select( (r, u, a, u2 ))
              orderBy(r.version desc)
              on(r.authorId === u.id, r.id === a.map(_.revisionId), a.map(_.userId) === u2.map(_.id))
          ).toList
        }.foldRight(List.empty[(Revision,User,List[(Option[Approval],Option[User])])]){ (x, out) =>
          out match {
            case head :: tail if (head._1 == x._1) => (head._1, head._2, (x._3, x._4) :: head._3) :: tail
            case list => (x._1, x._2, (x._3, x._4) :: Nil) :: list
          }
        }
      val out = bind("doc", in,
        "key" -> d.key,
        "revised" -> r.dateAsDT(),
        "link" -> ((in: NodeSeq) => <a href={d.downloadHref(r.version)}>{in}</a>),
        "version" -> r.version
      )
      (
        ".doc-title" #> <a href={d.infoLink}>{d.fullTitle}</a> &
        ".doc-number" #> d.keyAndVersion(r.version) &
        ".doc-version" #> r.version &
        ".doc-filename" #> <a href={d.downloadHref(r.version)} title="Download">{r.filename}</a> &
        ".doc-mediatype" #> tryo(new Tika().detect(r.filename)).openOr("?") &
        ".doc-next" #> d.nextVersion &
        ".doc-project" #> d.project.map(_.infoLink).getOrElse(<span>?</span>) &
        docActions(d, r) &
        ".doc-pending" #> {
          editor.headOption match {
            case Some(editPending) => {
              ".doc-editor" #> editPending.user().map(_.profileLabel(pageUserId)).getOrElse(<span>???</span>) &
              ".edit-requested" #> editPending.dateOnlyWithHint &
              ".doc-next" #> d.nextVersion
            }
           case _ => {
              "tr" #> NodeSeq.Empty
            }
          }
        } &
        ".doc-revision" #> rs.map { x =>
          val r = x._1
          val u = x._2
          val as = x._3.collect{case (Some(a),b) => (a,b)}
          ".rev-number" #> r.version &
          ".rev-download" #> <a href={d.downloadHref(r.version)}>Download</a> &
          ".rev-approve" #> <a href={d.approveHref(r.version)}>Approve</a> &
          ".rev-author" #> u.knownOption.map(_.profileLabel(pageUserId)).getOrElse(Text(r.rawAuthor)) &
          ".rev-comment" #> r.comment &
          ".rev-date" #> r.dateOnlyWithHint &
          ".rev-approval" #> as.map { y =>
            val a = y._1
            val u2 = y._2
            ".approval-by" #> (u2.flatMap(_.knownOption).map(_.profileLabel(pageUserId)).getOrElse(Text(a.rawUser))) &
            ".approval-state" #> <span class={ApprovalState.style(a.state)}>{a.state}</span> &
            ".approval-comment" #> <span>{ if (a.comment == "No Comment") "" else a.comment }</span> &
            ".approval-date" #> a.dateOnlyWithHint
          }
        } &
        ".doc-subscriber" #> Subscription.watchersFor(d).filter(_.knownOption.isDefined).map { u =>
          ".subscriber-info" #> u.profileLabel(user.map(_.id).getOrElse(-1L))
        } &
        ".contributor" #> d.contributors().filter(_.knownOption.isDefined).map { u =>
          "li *" #> u.profileLabel(user.map(_.id).getOrElse(-1L))
        }
      ).apply(in)
      }
      case  _ => NodeSeq.Empty
    }
  }

  def edit(in: NodeSeq): NodeSeq =
  {
    (document, revision) match
    {
      case (Full(d), Full(r)) =>
      {
    (
      ClearClearable &
      ".doc-title" #> <a href={d.infoLink}>{d.fullTitle}</a> &
      ".edit-download [href]" #> d.linkForEditing() &
      ".edit-submit-file [href]" #> (d.submitHref()) &
      ".edit-back [href]" #> (d.infoHref())
    ).apply(in)
      }
      case _ => NodeSeq.Empty
    }
  }

  private def docActions(d: Document, r: Revision): CssBindFunc = {
    ".doc-link [href]" #> d.downloadHref(r.version) &
    ".doc-edit" #> edit(d) &
    ".doc-submit" #> submit(d) &
    ".doc-approve [href]" #> (d.approveHref(r.version)) &
    ".doc-request-approval [href]" #> (d.requestApprovalHref(r.version)) &
    ".doc-watch" #> watch(d) &
    ".doc-fav" #> favourite(d)
  }

  private def edit(d: Document): NodeSeq = {
    user match {
      case Full(u) if (userIsActingAsEditor) => {
        SHtml.a(() => { processUnedit(d, u) }, <span>Cancel Edit</span>, "class" -> "btn")
      }
      case Full(u) => {
        SHtml.a(() => { processEdit(d, u) }, <i class="icon-edit icon-white"></i><span> Edit</span>, "class" -> (if (editing_?()) "btn btn-danger" else "btn btn-warning"))
      }
      case _ => {
        NodeSeq.Empty
      }
    }
  }

  private def submit(d: Document): NodeSeq = {
    user match {
      case Full(u) if (userIsActingAsEditor) => {
        <a class="btn btn-success" href={d.submitHref()}><i class="icon-share icon-white"></i> Submit</a>
      }
      case _ => {
        NodeSeq.Empty
      }
    }
  }

  private def processEdit(d: Document, u: vvv.docreg.model.User): JsCmd = {
    Pending.editRequest(u, d)
    // TODO remove explicit call, make backend listen for pendings
    backend ! Edit(d, u)
    S.redirectTo(d.editHref())
  }

  private def processUnedit(d: Document, u: vvv.docreg.model.User): JsCmd = {
    Pending.editCancel(u, d)
    // TODO remove explicit call, make backend listen for pendings
    backend ! Unedit(d, u)
    JsCmds.RedirectTo(d.infoLink)
  }

  private def watch(d: Document) = {
    User.loggedInUser.is match {
      case Full(u) if (u.watching_?(d))  =>
      {
        SHtml.a(() => processWatch(d, u, false), <span><i class="icon-eye-open"></i> Watching</span> , "class" -> "btn active")
      }
      case Full(u) =>
      {
        SHtml.a(() => processWatch(d, u, true), <span><i class="icon-eye-close"></i> Watch</span> , "class" -> "btn")
      }
      case _ =>
      {
        NodeSeq.Empty
      }
    }
  }

  private def processWatch(d: Document, u: vvv.docreg.model.User, checked: Boolean) = {
    if (checked) {
      Subscription.addNotification(d, u)
    }
    else {
      Subscription.removeNotification(d, u)
    }
    // Could return faster
    val msg = Subscription.optionsFor(d, u) match {
      case Some(options) => SubscribeRequested(d, u, options)
      case None => UnsubscribeRequested(d, u)
    }
    backend ! msg
    S.redirectTo(d.infoLink)
  }

  private def favourite(d: Document) = {
    User.loggedInUser.is match {
      case Full(u) if (u.bookmarked_?(d))  =>
      {
        SHtml.a(() => processFavourite(d, u, false), <span><i class="icon-star"></i> Favourite</span> , "class" -> "btn active")
      }
      case Full(u) =>
      {
        SHtml.a(() => processFavourite(d, u, true), <span><i class="icon-star-empty"></i> Favourite</span> , "class" -> "btn")
      }
      case _ =>
      {
        NodeSeq.Empty
      }
    }
  }

  private def processFavourite(d: Document, u: vvv.docreg.model.User, checked: Boolean) = {
    if (checked) {
      Subscription.addBookmark(d, u)
    }
    else {
      Subscription.removeBookmark(d, u)
    }
    // Could return faster
    val msg = Subscription.optionsFor(d, u) match {
      case Some(options) => SubscribeRequested(d, u, options)
      case None => UnsubscribeRequested(d, u)
    }
    backend ! msg
    S.redirectTo(d.infoLink)
  }

  def approve(in: NodeSeq): NodeSeq =
  {
    (document, revision) match
    {
      case (Full(d), Full(r)) =>
      {
    val states = List(ApprovalState.approved, ApprovalState.notApproved) map (state => (state.toString, state.toString))
    var comment = ""
    var state = ApprovalState.approved
    (
      ".doc-title" #> <a href={d.infoLink}>{d.fullTitle}</a> &
      ".approval-version *" #> r.version &
      ".approval-by *" #> Text(User.loggedInUser map (_.displayName) openOr "?") &
      ".approval-state" #> SHtml.select(states, Full(state.toString), (selected) => (state = ApprovalState parse selected)) &
      ".approval-comment" #> SHtml.textarea(comment, comment = _, "class" -> "input-xlarge", "maxlength" -> "128") &
      ".approval-submit" #> SHtml.submit("Submit", () => processApprove(d, r, state, comment), "class" -> "btn primary") &
      ".approval-cancel" #> SHtml.submit("Cancel", () => S.redirectTo(d.infoHref()), "class" -> "btn")
    ).apply(in)
      }
      case _ => NodeSeq.Empty
    }
  }

  def processApprove(d: Document, r: Revision, state: ApprovalState, comment: String): JsCmd = {
    val user = User.loggedInUser.is openOr null
    val approval = new Approval
    approval.revisionId = r.id
    approval.userId = user.id
    approval.state = state
    approval.comment = if (comment.trim == "") "No Comment" else comment
    approval.date = new Timestamp(System.currentTimeMillis())
    Approval.dbTable.insert(approval)
    backend ! ApprovalApproved(d, r, user, state, comment, user)
    S.notice("Document " + state)
    S.redirectTo(d.infoHref())
  }

  lazy val approverPartial = Templates("doc" :: "request-approval" :: Nil) match {
    case Full(in) => in \\ "div" filter (x => (x \ "@class").text.contains("approval-approver"))
    case _ => NodeSeq.Empty
  }
  lazy val docActionsPartial = Templates("doc" :: "info" :: Nil) match {
    case Full(in) => ("#doc-actions ^*" #> "ignored").apply(in)
    case _ => NodeSeq.Empty
  }

  def requestApproval(in: NodeSeq): NodeSeq =
  {
    (document, revision) match
    {
      case (Full(d), Full(r)) =>
      {
      ("#doc:title *" #> <a href={d.infoLink}>{d.fullTitle}</a> &
       "#doc:version *" #> r.version &
        ".approver:add" #> SHtml.ajaxButton(<span><i class="icon-plus"></i> Add</span>, () => {JqJsCmds.AppendHtml("approverList", approver(approverPartial))}, "class" -> "btn") &
       ".approval-approver" #> approver &
       "#submit" #> SHtml.submit("Submit", () => (processRequestApproval(d, r)), "class" -> "btn primary") &
       "#cancel" #> SHtml.submit("Cancel", () => S.redirectTo(d.infoHref()), "class" -> "btn") &
       ClearClearable) apply in
      }
      case _ => NodeSeq.Empty
    }
  }

  var approverCount: Int = 0
  object selected extends RequestVar[List[String]](Nil)

  def approver = {
    approverCount = approverCount + 1
    val id = "approver" + approverCount
    var approver = ""
    ".approval-approver [id]" #> id &
    ".approver:email" #> (SHtml.text(approver, s => selected(s :: selected.is), "maxlength" -> "64") % ("style" -> "width: 250px")) &
    ".approver:remove" #> SHtml.ajaxButton(<i class="icon-minus icon-white"></i>, () => {JsCmds.Replace(id, NodeSeq.Empty)}, "class" -> "btn btn-warning")
  }

  def processRequestApproval(d: Document, r: Revision) = {
    selected.is.map(s => UserLookup.lookup(None, Some(s), None, Environment.env.directory, "process request approval on " + r) openOr null).filterNot(_ == null) match {
      case Nil =>
        S.warning(<div class="alert-message error"><p>Approval request with no users!</p></div>)
      case xs  =>
        backend ! ApprovalRequested(d, r, xs, User.loggedInUser.is.getOrElse(null))
        S.notice("Approval requested for " + (xs.map(_.displayName).reduceRight((a, b) => a + "; " + b)))
    }
    S.redirectTo(d.infoHref())
  }

  private val projects = Project.findAllUsed()

  private object name extends RequestVar(document.map(_.title).getOrElse("???"))
  private object nameError extends RequestVar[Option[String]](None)
  private object project extends RequestVar(document.flatMap(_.project()).map(_.name).getOrElse(projects.headOption.map(_.name).getOrElse("???")))
  private object file extends RequestVar[Option[FileParamHolder]](None)
  private object fileError extends RequestVar[Option[String]](None)
  private object comment extends RequestVar("")

  def submit(in: NodeSeq) =
  {
    (document, revision) match
    {
      case (Full(d), Full(r)) =>
      {
    val projectList = projects.map(i => (i.name, i.name))
    (
//      ".doc-title" #> <a href={d.infoLink}>{r.fullTitle}</a> &
      ".submission-project" #> SHtml.select(projectList, Option(project.is), project(_)) &
      ".submission-name" #> SHtml.text(name.is, name(_), "maxlength" -> "110") &
      "#name-group [class+]" #> nameError.is.map(_ => "error").getOrElse("") &
      ".submission-version *" #> d.nextVersion &
      ".submission-file" #> SHtml.fileUpload(ul => file(Some(ul))) &
      "#file-group [class+]" #> fileError.is.map(_ => "error").getOrElse("") &
      ".submission-by *" #> Text(User.loggedInUser map (_.displayName) openOr "?") &
      ".submission-comment" #> SHtml.textarea(comment.is, comment(_), "class" -> "input-xlarge", "maxlength" -> "512") &
      ".submission-submit" #> SHtml.submit("Submit", () => processSubmit(d, project.is, name.is, comment.is, file.is), "class" -> "btn primary") &
      ".submission-cancel" #> SHtml.submit("Cancel", () => S.redirectTo("/"), "class" -> "btn")
    ).apply(in)
      }
      case _ => NodeSeq.Empty
    }
  }

  private def processSubmit(d: Document, projectName : String, name: String, comment: String, file: Option[FileParamHolder]) {
    file match {
      case Some(f: OnDiskFileParamHolder) if f.mimeType == null =>
      {
        fileError(Some("No file uploaded!"))
      }
      case Some(f: OnDiskFileParamHolder) if name == "" =>
      {
        nameError(Some("Name must be entered!"))
        fileError(Some("Please re-select file"))
      }
      case Some(f: OnDiskFileParamHolder) =>
      {
        User.loggedInUser.is match {
          case Full(user) =>
            logger.debug("Upload " + f.localFile + " as " + f.fileName + " to " + d.key + " ")

            // Note: OnDiskFileParamHolder will delete the local file on finalize, so pass the local file in a wrapper such that it is maintained til needed.
            Environment.env.backend ! Submit(d, projectName, () => f.localFile, d.nextFileName(name, f.fileName), comment, user)
            S.notice(<div class="alert-message info"><p>Document submitted, waiting for system to update...</p></div>)
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

