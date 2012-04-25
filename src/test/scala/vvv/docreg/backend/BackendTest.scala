package vvv.docreg.backend

import org.specs._
import org.specs.matcher._
import org.specs.specification._
import scala.actors.Actor
import org.specs.mock.Mockito
import vvv.docreg.db.TestDbVendor
import com.hstx.docregsx.{TabLine, Subscriber => AgentSubscriber}
import vvv.docreg.model.{Subscription, UserLookup, User, Document}
import scala._
import org.squeryl.PrimitiveTypeMode._
import vvv.docreg.agent.{SubscriberInfo, DaemonAgentComponent}

object BackendTest extends Specification with Mockito
{
  val x = new BackendComponentImpl with DocumentServerComponent with DirectoryComponent with DaemonAgentComponent
  {
    val directory = null
    val documentServer = null
    val daemonAgent = null
    def createAgent(version: String, server: String, user: String, backend: Actor) = null
  }

  "Backend" should
  {
    "not add subscriptions when no agent subscriptions exist" >>
    {
      TestDbVendor.initAndClean()
      transaction{
      val (p1, _, _) = TestDbVendor.createProjects
      val d: Document = new Document
      d.number = ("1234")
      d.projectId = (p1.id)
      d.title = ("FooBarBaz")
      Document.dbTable.insert(d)

      x.backend.updateSubscriptions(d, Nil)

      Subscription.forDocument(d) must haveSize(0)
      }
    }

    "add subscriptions for a agent subscribers, ignoring duplicates" >>
    {
      TestDbVendor.initAndClean()
      transaction{
      val (p1, _, _) = TestDbVendor.createProjects
      val (u1, u2) = TestDbVendor.createUsers
      val d: Document = new Document
      d.number = ("1234")
      d.projectId = (p1.id)
      d.title = ("FooBarBaz")
      Document.dbTable.insert(d)

      val subsA = SubscriberInfo("Asutherl","alan.sutherland@hstx.com","always")
      val subsB = SubscriberInfo("Sabernethy","scott_abernethy@stratexnet.com","always")
      val subsC = SubscriberInfo("scott.abernethy@aviatnet.com","scott.abernethy@Aviatnet.com","always")

      val ul1 = new UserLookup
      ul1.username = Some("Asutherl")
      ul1.email = Some("alan.sutherland@hstx.com")
      ul1.name = None
      ul1.userId = (u1.id)
      UserLookup.dbTable.insert(ul1)
      val ul2 = new UserLookup
      ul2.username = Some("Sabernethy")
      ul2.email = Some("scott_abernethy@stratexnet.com")
      ul2.name = None
      ul2.userId = (u2.id)
      UserLookup.dbTable.insert(ul2)
      val ul3 = new UserLookup
      ul3.username = Some("scott.abernethy@aviatnet.com")
      ul3.email = Some("scott.abernethy@Aviatnet.com")
      ul3.name = None
      ul3.userId = (u2.id)
      UserLookup.dbTable.insert(ul3)

      x.backend.updateSubscriptions(d, subsA :: subsB :: subsC :: Nil)

      Subscription.usersFor(d) must haveTheSameElementsAs(u1 :: u2 :: Nil)
      }
    }

    "not duplicate existing subscriptions, and purge existing that a no longer listed" >>
    {
      TestDbVendor.initAndClean()
      transaction{
      val (p1, _, _) = TestDbVendor.createProjects
      val (u1, u2) = TestDbVendor.createUsers
      val d: Document = new Document
      d.number = ("1234")
      d.projectId = (p1.id)
      d.title = ("FooBarBaz")
      Document.dbTable.insert(d)

      val subsA = SubscriberInfo("Asutherl","alan.sutherland@hstx.com","always")
      val ul = new UserLookup
      ul.username = Some("Asutherl")
      ul.email = Some("alan.sutherland@hstx.com")
      ul.name = None
      ul.userId = (u1.id)
      UserLookup.dbTable.insert(ul)

      val s1 = new Subscription
      s1.documentId = (d.id)
      s1.userId = (u1.id)
      val s2 = new Subscription
      s2.documentId = (d.id)
      s2.userId = (u2.id)
      Subscription.dbTable.insert(List(s1,s2))

      x.backend.updateSubscriptions(d, subsA :: Nil)

      Subscription.usersFor(d) must haveTheSameElementsAs(u1 :: Nil)
      }
    }

    "Do TabLines work?" >>
    {
      val x = new TabLine("Asutherl\talan.sutherland@hstx.com\talways")
      x.size() must be_==(3)
      x.get(0) must be_==("Asutherl")
      x.get(1) must be_==("alan.sutherland@hstx.com")
      x.get(2) must be_==("always")
    }

    "Do AgentSubscribers work?" >>
    {
      val x = new AgentSubscriber(new TabLine("Asutherl\talan.sutherland@hstx.com\talways"))
      x.getSubscriberEmail must be_==("alan.sutherland@hstx.com")
      x.getSubscriberUserName must be_==("Asutherl")
      x.getEmailEvent must be_==("always")
    }
  }
}