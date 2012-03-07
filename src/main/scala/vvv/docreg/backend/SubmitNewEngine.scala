package vvv.docreg.backend

import actors.Actor
import vvv.docreg.agent._
import net.liftweb.common.Loggable
import java.io.File
import vvv.docreg.model.{Document, User}
import com.hstx.docregsx.ScpClient

class SubmitNewEngine(agent: DaemonAgent, target: String, clientHost: String, clientVersion: String) extends Actor with Loggable
{
  var cachedLocalFile: File = null

  def act()
  {
    loop
    {
      react
      {
        case SubmitNew(projectName, localFile, userFileName, comment, user) =>
        {
          cachedLocalFile = localFile
          agent ! RequestPackage(Actor.self, target, RegisterRequest(userFileName, projectName, comment, "Everyone", user.displayName, user.shortUsername(), clientHost, clientVersion))
        }
        case RegisterReply(response, suggestedFileName) =>
        {
          // todo reply package could include request, thus no need to store it.

          // Sending multiple times with the same file name gives the same doc key suggestion.
          // "Accepted - file name already contained correct version
          println("Register reply " + response + " with suggested filename of " + suggestedFileName)
          if (response != null && response.startsWith("Accepted"))
          {
            // Submit with suggested file name assuming, as long as it is version 001 and has same fileName part.
            val submittedFileName = suggestedFileName
            var scpClient = new ScpClient(target)
            println("Copying file")
            scpClient.copy(cachedLocalFile.toString(), submittedFileName);
            println("Copying file, done")
            // todo check file size
            agent ! RequestPackage(Actor.self, target, SubmitRequest(submittedFileName, -1))
            // todo delete local file?
          }
          // todo else
        }
        case SubmitReply(response, suggestedFileName) =>
        {
          println("Submit reply " + response + " with suggested filename of " + suggestedFileName)
          // todo, notify user
        }
        case 'Die =>
        {
          exit()
        }
        case other =>
        {
          println("Submit engine got unexpected " + other)
        }
      }
    }
  }
}

object SubmitNewEngine
{
  def main(args: Array[String])
  {
    val u = User.create.name("Scott Abernethy").username("sabernethy@GNET.global.vpn")

    val agent = new DaemonAgentImpl()
    agent.start()
    val x = new SubmitNewEngine(agent, "shelob", "10.16.2.0", "dr+w 0.7.0.dev")
    x.start()
    x ! SubmitNew("DocReg", new File("/tmp/garbage.txt"), "New Document Test 4.txt", "Testing document addition with docregweb", u)
    Actor.receiveWithin(30000) {
      case in => println("XX " + in)
    }
    agent ! 'Die
    x ! 'Die
  }
}
