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
          // TODO warning if document is being editted!
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
      bind("doc", in,
        "key" -> d.key,
        "title" -> <a href={r.info}>{r.fullTitle}</a>,
        "author" -> r.author,
        "revised" -> r.date,
        "link" -> ((in: NodeSeq) => <a href={r.link}>{in}</a>),
        "version" -> r.version,
        "project" -> d.projectName,
        "editor" -> (if (d.editor.is == null) Text("-") else <span class="highlight">{d.editor.asHtml}</span>),
        "submissions" -> submissionLinks(d),
        "subscribers" -> subscribers(d),
        "revision" -> ((in: NodeSeq) => revisions(in, d, r)),
        "approve" -> ((in: NodeSeq) => <a href={"/d/" + d.key + "/v/" + r.version + "/approve"}>{in}</a>),
        "request-approval" -> ((in: NodeSeq) => <a href={"/d/" + d.key + "/v/" + r.version + "/request-approval"}>{in}</a>),
        "subscribe" -> subscribe(d)
      )
    })

  private def submissionLinks(d: Document) = {
    User.loggedInUser.is match {
      case Full(u) =>
        processSubmissionLinks(d, u)
      case _ =>
        NodeSeq.Empty
    }
  }

  private def processSubmissionLinks(d: Document, u: vvv.docreg.model.User): NodeSeq = {
    if (d.editor.is == null)//If no one is editing, current user has the option to edit
      <li>{ SHtml.a(() => { processEdit(d, u); JsCmds.RedirectTo(d.latest.link) }, <span>Edit</span>)  }</li>
    else {
      if (d.editor.is == u.displayName) {  //If current user is editing, they can either submit changes or cancel
        <li><a href={d.latest.info + "/submit"}>Submit...</a></li> ++
        <li>{ SHtml.a(() => { processUnedit(d, u); JsCmds._Noop }, <span>Cancel Edit</span>) }</li>
      }
      else { //otherwise, someone else is editing, so current user has no options
        NodeSeq.Empty
      }
    }
  }

  // TODO this method is only here to make testing the submission sysem easier.
  // Delete when done and change submissionLinks method to call processSubmissionLinks
  private def processSubmissionLinksAll(d: Document, u: vvv.docreg.model.User): NodeSeq = {
      <li>{ SHtml.a(() => { processEdit(d, u); JsCmds.RedirectTo(d.latest.link) }, <span>Edit</span>)  }</li> ++
      <li><a href={d.latest.info + "/submit"}>Submit...</a></li> ++
      <li>{ SHtml.a(() => { processUnedit(d, u); JsCmds._Noop }, <span>Cancel Edit</span>) }</li>
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
                                   <span id="subscribe">{ if (u.subscribed_?(d)) "Unsubscribe" else "Subscribe" }</span> )
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

  private def subscribers(d: Document): NodeSeq = {
    val subscribers = Subscription.forDocument(d).map(_.user.obj.openOr(null)).filterNot(_ == null)
    if (subscribers.isEmpty)
      <span>-</span>
    else {
      //sort subscribers by last name before creating list of links
      val sortedSubscribers = subscribers.sortWith((a, b) => a.displayName.split(" ").last < b.displayName.split(" ").last)
                                          //must treat this bit as a NodeSeq otherwise the reduceRight doesn't work
      <span>{ sortedSubscribers.map(s => <a href={ s.profileLink }>{ s.displayName }</a>: NodeSeq).reduceRight((a, b) => a ++ <span>&#44;&nbsp;</span> ++ b) }</span>
    }
  }

  private def revisions(xhtml: NodeSeq, d: Document, revisionInRequest: Revision): NodeSeq = {
    d.revisions flatMap { r =>
      bind("rev", xhtml,
        AttrBindParam("download_attr", "/d/" + d.key + "/v/" + r.version + "/download", "href"),
        AttrBindParam("approve_attr", "/d/" + d.key + "/v/" + r.version + "/approve", "href"),
        AttrBindParam("request-approval_attr", "/d/" + d.key + "/v/" + r.version + "/request-approval", "href"),
        "version" -> r.version,
        "author" -> r.author,
        "date" -> r.date,
        "approvals" -> ((in: NodeSeq) => approvals(in, r)),
        "link" -> <a href={r.link}>{r.version.asHtml}</a>,
        "comment" -> r.comment)
    }
  }

  private def approvals(xhtml: NodeSeq, r: Revision): NodeSeq = {
    Approval.forRevision(r) flatMap {a =>
      bind("approval", xhtml,
        "by" -> (a.by.obj.map (o => <a href={o.profileLink}>{o.displayName}</a>) openOr Text("?")),
        "state" -> <span style={ApprovalState.style(a.state.is)}>{a.state}</span>,
        "comment" -> <span>{ if (a.comment.is == "No Comment") "" else a.comment }</span>,
        "date" -> a.date)
    }
  }

  def approve(in: NodeSeq): NodeSeq = forRequest(in, (in, d, r) => {
      bind("doc", in,
        "key" -> d.key,
        "title" -> <a href={r.info}>{r.fullTitle}</a>,
        "author" -> r.author,
        "revised" -> r.date,
        "link" -> ((in: NodeSeq) => <a href={r.link}>{in}</a>),
        "version" -> r.version,
        "project" -> d.projectName,
        "edit" -> (if (d.editor.is == null) Text("-") else <span class="highlight">{d.editor.asHtml}</span>),
        "approval" -> ((in: NodeSeq) => approvalForm(in, d, r)))
  })

  private def approvalForm(in: NodeSeq, d: Document, r: Revision): NodeSeq = {
    val states = List(ApprovalState.approved, ApprovalState.notApproved) map (state => (state.toString, state.toString))
    var comment = ""
    var state = ApprovalState.approved
    bind("approval", in,
      "version" -> r.version,
      "by" -> Text(User.loggedInUser map (_.displayName) openOr "?"),
      "state" -> SHtml.select(states, Full(state.toString), (selected) => (state = ApprovalState parse selected)),
      "comment" -> SHtml.textarea(comment, comment = _) % ("class" -> "smalltextarea"),
      "submit" -> SHtml.submit("Submit", () => processApprove(d, r, state, comment)),
      "cancel" -> SHtml.submit("Cancel", () => S.redirectTo(r.info))
    )
  }
  
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

  lazy val approverPartial = TemplateFinder.findAnyTemplate("doc" :: "request-approval" :: Nil) match {
    case Full(in) => in \\ "tr" filter (x => (x \ "@class").text.contains("approval:approver"))
    case _ => NodeSeq.Empty
  }

  def requestApproval(in: NodeSeq): NodeSeq = forRequest(in, (in, d, r) => {
      ("#doc:title *" #> <a href={r.info}>{r.fullTitle}</a> &
       "#doc:version" #> r.version &
       ".approval:approver" #> approver &
       ".approver:add" #> SHtml.ajaxButton("Add", () => {JqJsCmds.AppendHtml("addTo", approver(approverPartial))}) &
       "#submit" #> SHtml.submit("Submit", () => (processRequestApproval(d, r))) &
       "#cancel" #> SHtml.submit("Cancel", () => S.redirectTo(r.info)) &
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
    ".approver:remove" #> SHtml.ajaxButton("Remove", () => {JsCmds.Replace(id, NodeSeq.Empty)})
  }

  def processRequestApproval(d: Document, r: Revision) = {
    selected.is.map(User.forEmailOrCreate(_) openOr null).filterNot(_ == null) match {
      case Nil =>
        S.warning("Approval request with no users!")     
      case xs  =>
        backend ! ApprovalRequested(d, r, xs)
        S.notice("Approval requested for " + (xs.map(_.displayName).reduceRight((a, b) => a + "; " + b)))
    }
    S.redirectTo(r.info)
  }

/*  def submit(in: NodeSeq) = forRequest(in, (in, d, r) => {
    bind("doc", in,
        "title" -> <a href={r.info}>{r.fullTitle}</a>,
        "submission" -> ((in: NodeSeq) => submissionForm(in, d, r))
    )})

  private object submitFile extends RequestVar[Box[FileParamHolder]](Empty)

   private def submissionForm(in: NodeSeq, d: Document, r: Revision): NodeSeq = {
     var comment = ""
      bind("submission", in,
      "version" -> r.version,
      "by" -> Text(User.loggedInUser map (_.displayName) openOr "?"),
      "file" -> SHtml.fileUpload(ul => submitFile(Full(ul))),
      "comment" -> SHtml.textarea(comment, comment = _) % ("class" -> "smalltextarea"),
      "submit" -> SHtml.submit("Submit", () => { println(submitFile.is.getClass); }),
      "cancel" -> SHtml.submit("Cancel", () => S.redirectTo(r.info))
    )
  }*/
}

