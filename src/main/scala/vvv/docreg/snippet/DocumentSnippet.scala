package vvv.docreg.snippet

import vvv.docreg.backend._
import vvv.docreg.model._
import vvv.docreg.model.ApprovalState._
import net.liftweb._
import mapper._
import util._
import common._
import Helpers._
import http._
import js.JE.JsRaw
import js.jquery.JqJE._
import js._
import js.jquery._
import scala.xml.{NodeSeq, Text, Elem}
import net.liftweb.http.js.JsCmds._
import java.util.Date
import vvv.docreg.util.{StringUtil, Environment, TemplateParse}

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

  def forRequest(in: NodeSeq, op: (NodeSeq, Document, Revision) => NodeSeq): NodeSeq = {
    document match {
      case Full(d) => revision match {
        case Full(r) =>
          if (editor != Nil) S.notice(<div class="alert-message info"><p>Document is currently being edited</p></div>)
          if (!d.latest_?(r.version.is)) S.warning(<p>Not the most recent version of this document</p>)
          op(in, d, r)
        case _ => 
          <div class="alert-message error"><p><strong>Invalid</strong>{" version '" + version + "' for document '" + key + "'"}</p></div>
      }
      case _ =>
        <div class="alert-message error"><p><strong>Invalid</strong>{" document '" + key + "'"}</p></div>
    }
  }

  def header(in: NodeSeq): NodeSeq = {
    forRequest(in, (in, d, r) => {
      ".doc-title" #> <a href={d.infoLink}>{r.fullTitle}</a> &
      ".doc-project" #> d.project.map(_.infoLink).getOrElse(<span>?</span>) apply in
    })
  }

  def info(in: NodeSeq): NodeSeq = forRequest(in, (in, d, r) => {
      val out = bind("doc", in,
        "key" -> d.key,
        "revised" -> r.date,
        "link" -> ((in: NodeSeq) => <a href={r.link}>{in}</a>),
        "version" -> r.version,
        "subscriber" -> subscribers(d)
      )
      (
        ".doc-title" #> <a href={d.infoLink}>{r.fullTitle}</a> &
        ".doc-number" #> r.number &
        ".doc-version" #> r.version &
        ".doc-next" #> d.nextVersion &
        ".doc-project" #> d.project.map(_.infoLink).getOrElse(<span>?</span>) &
        docActions(d, r) &
        ".doc-subscribe" #> subscribe(d) &
        ".doc-pending" #> {
          editor.headOption match {
            case Some(editPending) => {
              ".doc-editor" #> editPending.user.obj.map(_.profileLink()).getOrElse(<span>???</span>) &
              ".edit-requested" #> editPending.date &
              ".doc-next" #> d.nextVersion
            }
            case _ => {
              "tr" #> NodeSeq.Empty
            }
          }
        } &
        ".doc-revision" #> d.revisions.map { r =>User
          ".rev-link" #> <a href={r.info}>{r.version.asHtml}</a> &
          ".rev-author" #> r.authorLink &
          ".rev-comment" #> r.comment.is &
          ".rev-date" #> r.date & // TODO date only, hover for time
          ".rev-approval" #> Approval.forRevision(r).map { a =>
            ".approval-by" #> (a.by.obj.map(_.profileLink) openOr Text("?")) &
            ".approval-state" #> <span class={ApprovalState.style(a.state.is)}>{a.state}</span> &
            ".approval-comment" #> <span>{ if (a.comment.is == "No Comment") "" else a.comment }</span> &
            ".approval-date" #> a.date
          }
        } &
        ".doc-subscriber" #> d.subscribers.toList.sortWith(User.sort).map { u =>
          ".subscriber-info" #> u.profileLink
        }
      ).apply(in)
    })

  def edit(in: NodeSeq): NodeSeq = forRequest(in, (in, d, r) => {
    (
      ClearClearable &
      ".doc-title" #> <a href={d.infoLink}>{r.fullTitle}</a> &
      ".edit-download [href]" #> d.linkForEditing(user.map(_.shortUsername).getOrElse("unknown")) &
      ".edit-submit-file [href]" #> (d.latest.info + "/submit") &
      ".edit-back [href]" #> (d.latest.info)
    ).apply(in)
    })

  private def docActions(d: Document, r: Revision): CssBindFunc = {
    ".doc-link [href]" #> r.link &
    ".doc-edit" #> edit(d) &
    ".doc-submit" #> submit(d) &
    ".doc-approve [href]" #> (r.info + "/approve") &
    ".doc-request-approval [href]" #> (r.info + "/request-approval")
  }

  private def subscribers(d: Document): NodeSeq =
  {
    val users: List[User] = d.subscribers.toList
    users match
    {
      case Nil =>
      {
        <li>No subscribers</li>
      }
      case list =>
      {
        for (u <- list.sortWith(User.sort))
        yield <li><a href={ u.profileLink }>{ u.displayName }</a></li>
      }
    }
  }

//  private def revisions(xhtml: NodeSeq, d: Document, revisionInRequest: Revision): NodeSeq = {
//    d.revisions flatMap { r =>
//      bind("rev", xhtml,
//        AttrBindParam("download_attr", "/d/" + d.key + "/v/" + r.version + "/download", "href"),
//        AttrBindParam("approve_attr", "/d/" + d.key + "/v/" + r.version + "/approve", "href"),
//        AttrBindParam("request-approval_attr", "/d/" + d.key + "/v/" + r.version + "/request-approval", "href"),
//        "version" -> r.version,
//        "author" -> r.authorLink,
//        "date" -> r.date,
//        "approvals" -> ((in: NodeSeq) => approvals(in, r)),
//        "link" -> <a href={r.link}>{r.version.asHtml}</a>,
//        "comment" -> r.comment)
//    }
//  }

  private def edit(d: Document): NodeSeq = {
    user match {
      case Full(u) if (userIsActingAsEditor) => {
        SHtml.a(() => { processUnedit(d, u) }, <span> Cancel Edit</span>, "class" -> "btn")
      }
      case Full(u) => {
        SHtml.a(() => { processEdit(d, u) }, <i class="icon-edit icon-white"></i><span> Edit</span>, "class" -> "btn btn-warning")
      }
      case _ => {
        NodeSeq.Empty
      }
    }
  }

  private def submit(d: Document): NodeSeq = {
    user match {
      case Full(u) if (userIsActingAsEditor) => {
        <a class="btn btn-success" href={d.latest.info + "/submit"}><i class="icon-share icon-white"></i> Submit</a>
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
    S.redirectTo(d.latest.info + "/edit")
  }

  private def processUnedit(d: Document, u: vvv.docreg.model.User): JsCmd = {
    Pending.editCancel(u, d)
    // TODO remove explicit call, make backend listen for pendings
    backend ! Unedit(d, u)
    JsCmds.RedirectTo(d.infoLink)
  }

  private def subscribe(d: Document) = {
    User.loggedInUser.is match {
      case Full(u) =>
        SHtml.a(() => { processSubscribe(d, u)
                        (JsCmds.SetHtml("subscribers", subscribers(d)) &
                         JsCmds.SetHtml("subscribe", Text( if (u.subscribed_?(d)) "Unsubscribe" else "Subscribe" ))) },
                                   <span id="subscribe">{ if (u.subscribed_?(d)) "Unsubscribe" else "Subscribe" }</span> , "class" -> "btn")
      case _ =>
        NodeSeq.Empty
    }
  }

  private def processSubscribe(d: Document, u: vvv.docreg.model.User) = {
    if (!u.subscribed_?(d)) {
      Subscription.subscribe(d, u)
      backend ! SubscribeRequested(d, u)
      S.notice("Subscribe request sent")
    }
    else {
      Subscription.unsubscribe(d, u)
      backend ! UnsubscribeRequested(d, u)
      S.notice("Unsubscribe request sent")
    }
  }

  def approve(in: NodeSeq): NodeSeq = forRequest(in, (in, d, r) => {
    val states = List(ApprovalState.approved, ApprovalState.notApproved) map (state => (state.toString, state.toString))
    var comment = ""
    var state = ApprovalState.approved
    (
      ".doc-title" #> <a href={d.infoLink}>{r.fullTitle}</a> &
      ".approval-version *" #> r.version &
      ".approval-by *" #> Text(User.loggedInUser map (_.displayName) openOr "?") &
      ".approval-state" #> SHtml.select(states, Full(state.toString), (selected) => (state = ApprovalState parse selected)) &
      ".approval-comment" #> SHtml.textarea(comment, comment = _, "class" -> "input-xlarge") &
      ".approval-submit" #> SHtml.submit("Submit", () => processApprove(d, r, state, comment), "class" -> "btn primary") &
      ".approval-cancel" #> SHtml.submit("Cancel", () => S.redirectTo(r.info), "class" -> "btn")
    ).apply(in)
  })
  
  def processApprove(d: Document, r: Revision, state: ApprovalState, comment: String): JsCmd = {
    val user = User.loggedInUser.is openOr null
    val approval = Approval.create
    approval.revision(r)
    approval.by(user)
    approval.state(state)
    approval.comment(if (comment.trim == "") "No Comment" else comment)
    approval.date(new java.util.Date)
    approval.save
    backend ! ApprovalApproved(d, r, user, state, comment)
    S.notice("Document " + state)
    S.redirectTo(r.info)
  }

  lazy val approverPartial = Templates("doc" :: "request-approval" :: Nil) match {
    case Full(in) => in \\ "div" filter (x => (x \ "@class").text.contains("approval-approver"))
    case _ => NodeSeq.Empty
  }
  lazy val docActionsPartial = Templates("doc" :: "info" :: Nil) match {
    case Full(in) => ("#doc-actions ^*" #> "ignored").apply(in)
    case _ => NodeSeq.Empty
  }

  def requestApproval(in: NodeSeq): NodeSeq = forRequest(in, (in, d, r) => {
      ("#doc:title *" #> <a href={d.infoLink}>{r.fullTitle}</a> &
       "#doc:version *" #> r.version &
        ".approver:add" #> SHtml.ajaxButton(<span><i class="icon-plus"></i> Add</span>, () => {JqJsCmds.AppendHtml("approverList", approver(approverPartial))}, "class" -> "btn") &
       ".approval-approver" #> approver &
       "#submit" #> SHtml.submit("Submit", () => (processRequestApproval(d, r)), "class" -> "btn primary") &
       "#cancel" #> SHtml.submit("Cancel", () => S.redirectTo(r.info), "class" -> "btn") &
       ClearClearable) apply in
  })

  var approverCount: Int = 0
  object selected extends RequestVar[List[String]](Nil)

  def approver = {
    approverCount = approverCount + 1
    val id = "approver" + approverCount
    var approver = ""
    ".approval-approver [id]" #> id &
    ".approver:email" #> (SHtml.text(approver, s => selected(s :: selected.is)) % ("style" -> "width: 250px")) &
    ".approver:remove" #> SHtml.ajaxButton(<i class="icon-minus icon-white"></i>, () => {JsCmds.Replace(id, NodeSeq.Empty)}, "class" -> "btn btn-warning")
  }

  def processRequestApproval(d: Document, r: Revision) = {
    selected.is.map(s => UserLookup.lookup(None, Some(s), None, Environment.env.directory, "process request approval on " + r) openOr null).filterNot(_ == null) match {
      case Nil =>
        S.warning(<div class="alert-message error"><p>Approval request with no users!</p></div>)
      case xs  =>
        backend ! ApprovalRequested(d, r, xs)
        S.notice("Approval requested for " + (xs.map(_.displayName).reduceRight((a, b) => a + "; " + b)))
    }
    S.redirectTo(r.info)
  }

  private val projects = Project.findAll()

  private object name extends RequestVar(document.map(_.title.is).getOrElse("???"))
  private object nameError extends RequestVar[Option[String]](None)
  private object project extends RequestVar(document.map(_.projectName).getOrElse(projects.headOption.map(_.name.is).getOrElse("???")))
  private object file extends RequestVar[Option[FileParamHolder]](None)
  private object fileError extends RequestVar[Option[String]](None)
  private object comment extends RequestVar("")

  def submit(in: NodeSeq) = forRequest(in, (in, d, r) => {
    val projectList = projects.map(i => (i.name.is, i.name.is))
    (
//      ".doc-title" #> <a href={d.infoLink}>{r.fullTitle}</a> &
      ".submission-project" #> SHtml.select(projectList, Option(project.is), project(_)) &
      ".submission-name" #> SHtml.text(name.is, name(_)) &
      "#name-group [class+]" #> nameError.is.map(_ => "error").getOrElse("") &
      ".submission-version *" #> d.nextVersion &
      ".submission-file" #> SHtml.fileUpload(ul => file(Some(ul))) &
      "#file-group [class+]" #> fileError.is.map(_ => "error").getOrElse("") &
      ".submission-by *" #> Text(User.loggedInUser map (_.displayName) openOr "?") &
      ".submission-comment" #> SHtml.textarea(comment.is, comment(_), "class" -> "input-xlarge") &
      ".submission-submit" #> SHtml.submit("Submit", () => processSubmit(d, project.is, name.is, comment.is, file.is), "class" -> "btn primary") &
      ".submission-cancel" #> SHtml.submit("Cancel", () => S.redirectTo("/"), "class" -> "btn")
    ).apply(in)
  })

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
            logger.info("send " + f.localFile + " as " + f.fileName + " to " + d.key + " ")

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

