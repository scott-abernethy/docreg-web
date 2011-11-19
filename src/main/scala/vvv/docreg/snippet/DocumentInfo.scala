package vvv.docreg.snippet

import vvv.docreg.backend._
import vvv.docreg.model._
import vvv.docreg.model.ApprovalState._
import net.liftweb._
import util._
import common._
import Helpers._
import http._
import js._
import js.jquery._
import JE._
import scala.xml.{NodeSeq, Text, Elem}
import vvv.docreg.util.{Environment, TemplateParse}


class DocumentSnippet extends Loggable {
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

  def forRequest(in: NodeSeq, op: (NodeSeq, Document, Revision) => NodeSeq): NodeSeq = {
    document match {
      case Full(d) => revision match {
        case Full(r) =>
          if (d.editor.is != null) S.notice("Document is currently being editted")
          if (!d.latest_?(r.version.is)) S.warning("Not the most recent version of this document")
          op(in, d, r)
        case _ => 
          S.error("Invalid version '" + version + "' for document '" + key + "'")
          NodeSeq.Empty
      }
      case _ => 
        S.error("Invalid document '" + key + "'")
        NodeSeq.Empty
    }
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
        ".doc-title" #> <a href={r.info}>{r.fullTitle}</a> &
        ".doc-number" #> r.number &
        ".doc-next" #> d.nextVersion &
        ".doc-project" #> d.projectName &
        ".doc-link [href]" #> r.link &
        ".doc-subscribe" #> subscribe(d) &
        ".doc-edit" #> edit(d, User.loggedInUser.is) &
        ".doc-submit" #> submit(d, User.loggedInUser.is) &
        ".doc-approve [href]" #> ("/d/" + d.key + "/v/" + r.version + "/approve") &
        ".doc-request-approval [href]" #> ("/d/" + d.key + "/v/" + r.version + "/request-approval") &
        ".doc-editing" #> (if (d.editor.is == null) "tr" #> NodeSeq.Empty else (".doc-editor" #> d.editor & ".doc-next" #> d.nextVersion)) &
        ".doc-revision" #> d.revisions.map { r =>
          ".rev-link" #> <a href={r.link}>{r.version.asHtml}</a> &
          ".rev-author" #> r.authorLink &
          ".rev-comment" #> r.comment &
          ".rev-date" #> r.date & // TODO date only, hover for time
          ".rev-approval" #> Approval.forRevision(r).map { a =>
            ".approval-by" #> (a.by.obj.map(_.profileLink) openOr Text("?")) &
            ".approval-state" #> <span class={ApprovalState.style(a.state.is)}>{a.state}</span> &
            ".approval-comment" #> <span>{ if (a.comment.is == "No Comment") "" else a.comment }</span> &
            ".approval-date" #> a.date
          }
        } &
        ".doc-subscriber" #> d.subscribers.toList.sortWith(User.sort).map { s =>
          ".subscriber-info" #> <a href={ s.profileLink }>{ s.displayName }</a>
        }
      ).apply(in)
    })

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

  private def edit(d: Document, u: Box[User]): NodeSeq = {
    u match {
      case Full(user) if (user.shortUsername() == d.editor.is) => {
        SHtml.a(() => { processUnedit(d, user); JsCmds._Noop }, <span>Cancel Edit</span>, "class" -> "btn danger")
      }
      case Full(user) => {
        SHtml.a(() => { processEdit(d, user); JsCmds.RedirectTo(d.latest.link) }, <span>Edit</span>, "class" -> "btn danger")
      }
      case _ => {
        NodeSeq.Empty
      }
    }
  }

  private def submit(d: Document, u: Box[User]): NodeSeq = {
    u match {
      case Full(user) if (user.shortUsername() == d.editor.is) => {
        <a class="btn success" href={d.latest.info + "/submit"}>Submit...</a>
      }
      case _ => {
        NodeSeq.Empty
      }
    }
  }

  private def processEdit(d: Document, u: vvv.docreg.model.User) = {
    backend ! Edit(d, u)
    S.notice("Edit request sent")
  }

  private def processUnedit(d: Document, u: vvv.docreg.model.User) = {
    backend ! Unedit(d, u)
    S.notice("Cancel edit request sent")
  }

  private def processSubmit(d: Document, u: vvv.docreg.model.User) = {
    S.notice("Document submitted")
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
      ".doc-title" #> <a href={r.info}>{r.fullTitle}</a> &
      ".approval-version" #> r.version &
      ".approval-by" #> Text(User.loggedInUser map (_.displayName) openOr "?") &
      ".approval-state" #> SHtml.select(states, Full(state.toString), (selected) => (state = ApprovalState parse selected)) &
      ".approval-comment" #> SHtml.textarea(comment, comment = _) &
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
    case Full(in) => in \\ "tr" filter (x => (x \ "@class").text.contains("approval:approver"))
    case _ => NodeSeq.Empty
  }

  def requestApproval(in: NodeSeq): NodeSeq = forRequest(in, (in, d, r) => {
      ("#doc:title *" #> <a href={r.info}>{r.fullTitle}</a> &
       "#doc:version" #> r.version &
       ".approval:approver" #> approver &
       ".approver:add" #> SHtml.ajaxButton("Add", () => {JqJsCmds.AppendHtml("addTo", approver(approverPartial))}, "class" -> "btn") &
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
    ".approval:approver [id]" #> id &
    ".approver:email" #> (SHtml.text(approver, s => selected(s :: selected.is)) % ("style" -> "width: 250px")) &
    ".approver:remove" #> SHtml.ajaxButton("Remove", () => {JsCmds.Replace(id, NodeSeq.Empty)}, "class" -> "btn danger")
  }

  def processRequestApproval(d: Document, r: Revision) = {
    selected.is.map(s => UserLookup.lookup(None, Some(s), None, Environment.env.directory) openOr null).filterNot(_ == null) match {
      case Nil =>
        S.warning("Approval request with no users!")     
      case xs  =>
        backend ! ApprovalRequested(d, r, xs)
        S.notice("Approval requested for " + (xs.map(_.displayName).reduceRight((a, b) => a + "; " + b)))
    }
    S.redirectTo(r.info)
  }

  def submit(in: NodeSeq) = forRequest(in, (in, d, r) => {
    var comment = ""
    var file: Option[FileParamHolder] = None
    (
      ".doc-title" #> <a href={r.info}>{r.fullTitle}</a> &
      ".submission-version" #> d.nextVersion &
      ".submission-file" #> SHtml.fileUpload(ul => file = Some(ul)) &
      ".submission-by" #> Text(User.loggedInUser map (_.displayName) openOr "?") &
      ".submission-comment" #> SHtml.textarea(comment, comment = _) &
      ".submission-submit" #> SHtml.submit("Submit", () => processSubmit(d, comment, file), "class" -> "btn primary") &
      ".submission-cancel" #> SHtml.submit("Cancel", () => S.redirectTo(r.info), "class" -> "btn")
    ).apply(in)
  })

  private object submitFile extends RequestVar[Box[FileParamHolder]](Empty)

//   private def submissionForm(in: NodeSeq, d: Document, r: Revision): NodeSeq = {
//     var comment = ""
//     var file: Option[FileParamHolder] = None
//      bind("submission", in,
//      "version" -> d.nextVersion,
//      "by" -> Text(User.loggedInUser map (_.displayName) openOr "?"),
//      "file" -> SHtml.fileUpload(ul => file = Some(ul)),
//      "comment" -> SHtml.textarea(comment, comment = _) % ("class" -> "smalltextarea"),
//      "submit" -> SHtml.submit("Submit", () => processSubmit(d, comment, file)),
//      "cancel" -> SHtml.submit("Cancel", () => S.redirectTo(r.info))
//    )
//  }

  private def processSubmit(d: Document, comment: String, file: Option[FileParamHolder]) {
    file match {
      case Some(f: OnDiskFileParamHolder) if f.mimeType == null =>
        S.error("No file uploaded!")
      case Some(f: OnDiskFileParamHolder) =>
        S.notice("File uploaded")
        User.loggedInUser.is match {
          case Full(user) =>
            println("send " + f.localFile + " as " + f.fileName + " to " + d.key + " ")
            Environment.env.backend ! Submit(d, f.localFile, f.fileName, comment, user)
            S.notice("Submit processing...")
          case _ =>
            S.error("Unable to submit, no user logged in!")
        }
      case _ => 
        S.error("Failed to upload file!")
    }
  }
}

