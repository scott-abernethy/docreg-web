/*
 * Copyright (c) 2013 Aviat Networks.
 * This file is part of DocReg+Web. Please refer to the NOTICE.txt file for license details.
 */

package vvv.docreg.rest

import net.liftweb.http.rest.RestHelper
import net.liftweb.http.{RedirectResponse, S, XmlResponse}
import vvv.docreg.model._
import vvv.docreg.util.DatePresentation.formatDateTimeAsRfc822
import net.liftweb.common.Full

object ProjectFeed extends RestHelper {

  serve {
    case "project" :: ValidProject(project) :: "feed" :: _ XmlGet _=>  {
      User.loggedInUser.is match {
        case Full(user) => {
          XmlResponse(createFeed(user, project), "application/rss+xml").toResponse
        }
        case _ => {
          Full(RedirectResponse("/user/signin"))
        }
      }
    }
  }

  lazy val url = S.hostAndPath

  def createFeed(requester: User, project: Project) = {

    val (news, restricted) = UserSession.partitionAuthorized(Project.recentChanges(project.id), (x: (Document, Revision, User)) => x._1)

    <rss version="2.0" xmlns:atom="http://www.w3.org/2005/Atom">
      <channel>
        <title>DocReg Project {project.name}</title>
        <link>{ url + project.url }</link>
        <description>RSS Feed for DocReg project {project.name}</description>
        <atom:link href={url + project.url + "/feed.xml"} rel="self" type="application/rss+xml" />
        <language>en-us</language>
        <ttl>60</ttl>
        { news.map { entry =>
          val (document, revision, user) = entry
        (<item>
          <title>{ document.title }</title>
          <author>{ user.email } ({ user.displayName })</author>
          <description>{ revision.comment }</description>
          <link>{ url + document.infoHref(revision.version) }</link>
          <guid isPermaLink="false">{ document.keyAndVersion(revision.version) }</guid>
          <pubDate>{ formatDateTimeAsRfc822(revision.date) }</pubDate>
        </item>)
      }}
      </channel>
    </rss>
  }

}
