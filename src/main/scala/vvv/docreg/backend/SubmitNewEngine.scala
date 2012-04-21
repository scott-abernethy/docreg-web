package vvv.docreg.backend

import actors.Actor
import vvv.docreg.agent._
import net.liftweb.common.Loggable
import java.io.File
import vvv.docreg.model.{Document, User}
import com.hstx.docregsx.ScpClient

class SubmitNewEngine(agent: DaemonAgent, target: String, clientHost: String, clientVersion: String) extends Actor with Loggable
{
  var cachedRequest: Create = null

  def act()
  {
    loop
    {
      react
      {
        case msg @ Create(projectName, localFile, userFileName, comment, user) =>
        {
          // todo check the fields, including comment which should default to "[no description]"? Check default for approval etc also.
          cachedRequest = msg
          agent ! RequestPackage(Actor.self, target, RegisterRequest(userFileName, projectName, if (comment.length() < 1) "[no description]" else comment, "Everyone", user.displayName, user.shortUsername(), clientHost, clientVersion))
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
            var scpClient = new ScpClient(target)
            logger.debug("Copying file")
            scpClient.copy(cachedRequest.localFile.apply().toString(), submittedFileName);
            logger.debug("Copying file, done")
            // todo check file size
            agent ! RequestPackage(Actor.self, target, SubmitRequest(submittedFileName, -1))
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
          Actor.self ! 'Die
        }
        case 'Die =>
        {
          exit()
        }
        case other =>
        {
          logger.warn("Submit engine got unexpected " + other)
        }
        // todo timeout
      }
    }
  }

  override def exceptionHandler =
  {
    case e: Exception =>
    {
      logger.error("SubmitNewEngine exception " + e.getMessage, e)
      Actor.self ! 'Die
    }
  }
}

object SubmitNewEngine
{
  def main(args: Array[String])
  {
    val u = new User
    u.name = "Scott Abernethy"
    u.username = "sabernethy@GNET.global.vpn"

    val agent = new DaemonAgentImpl()
    agent.start()
    val x = new SubmitNewEngine(agent, "shelob", "10.16.2.0", "dr+w 0.7.0.dev")
    x.start()
    x ! Create("DocReg", () => new File("/tmp/garbage.txt"), "New Document Test 4.txt", "Testing document addition with docregweb", u)
    Actor.receiveWithin(30000) {
      case in => println("XX " + in)
    }
    agent ! 'Die
    x ! 'Die
  }
}
