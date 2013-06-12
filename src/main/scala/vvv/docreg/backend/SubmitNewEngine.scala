/*
 * Copyright (c) 2013 Scott Abernethy.
 * This file is part of DocReg+Web. Please refer to the NOTICE.txt file for license details.
 */

package vvv.docreg.backend

import vvv.docreg.agent._
import net.liftweb.common.Loggable
import akka.actor.{PoisonPill, Actor, ActorRef}

class SubmitNewEngine(agent: ActorRef, target: String, clientHost: String, clientVersion: String) extends Actor with Loggable
{
  var cachedRequest: Create = null

  def receive = {
        case msg @ Create(projectName, localFile, userFileName, comment, user) =>
        {
          // todo check the fields, including comment which should default to "[no description]"? Check default for approval etc also.
          cachedRequest = msg
          agent ! RequestPackage(self, target, SubmitEngine.registerRequest(projectName, "Everyone", userFileName, comment, user, clientHost, clientVersion))
        }
        case RegisterReply(response, suggestedFileName) =>
        {
          // todo reply package could include request, thus no need to store it.

          // Sending multiple times with the same file name gives the same doc key suggestion.
          // "Accepted - file name already contained correct version
          if (response != null && response.startsWith("Accepted"))
          {
            logger.info("Register reply " + response + " with suggested filename of " + suggestedFileName)
              // Submit with suggested file name assuming, as long as it is version 001 and has same fileName part.
            val submittedFileName = suggestedFileName
            logger.debug("Copying file")
            SubmitBin.copyTo(cachedRequest.localFile.apply(), submittedFileName)
            logger.debug("Copying file, done")
            // todo check file size
            agent ! RequestPackage(self, target, SubmitRequest(submittedFileName, -1))
            // todo delete local file?
          }
          else
          {
            logger.warn("Failed to register new document " + response + " -> " + suggestedFileName)
            // todo warn user!
          }
        }
        case SubmitReply(response, suggestedFileName) =>
        {
          logger.info("Submit reply " + response + " with suggested filename of " + suggestedFileName)
          // todo, notify user
          self ! 'Die
        }
        case 'Die =>
        {
          self ! PoisonPill
        }
        case other =>
        {
          logger.warn("Submit engine got unexpected " + other)
        }
        // todo timeout
        // todo supervision
  }
}
