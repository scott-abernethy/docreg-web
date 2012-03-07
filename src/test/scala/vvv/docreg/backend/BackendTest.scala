package vvv.docreg.backend

import org.specs._
import org.specs.matcher._
import org.specs.specification._
import actors.Actor
import org.specs.mock.Mockito
import vvv.docreg.db.TestDbVendor
import com.hstx.docregsx.{TabLine, Subscriber => AgentSubscriber}
import vvv.docreg.model.{Subscription, UserLookup, User, Document}
import vvv.docreg.agent.DaemonAgentComponent

object BackendTest extends Specification with Mockito
{
  val x = new BackendComponentImpl with DocumentServerComponent with AgentComponent with DirectoryComponent with DaemonAgentComponent
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
      val (p1, _, _) = TestDbVendor.createProjects
      val d: Document = Document.create.key("1234").project(p1).title("FooBarBaz")
      d.save

      x.backend.updateSubscriptions(d, Nil)

      d.reload.subscribers must haveSize(0)
    }

    "add subscriptions for a agent subscribers, ignoring duplicates" >>
    {
      TestDbVendor.initAndClean()
      val (p1, _, _) = TestDbVendor.createProjects
      val (u1, u2) = TestDbVendor.createUsers
      val d: Document = Document.create.key("1234").project(p1).title("FooBarBaz")
      d.save

      val subsA = new AgentSubscriber(new TabLine("Asutherl\talan.sutherland@hstx.com\talways"))
      val subsB = new AgentSubscriber(new TabLine("Sabernethy\tscott_abernethy@stratexnet.com\talways"))
      val subsC = new AgentSubscriber(new TabLine("scott.abernethy@aviatnet.com\tscott.abernethy@Aviatnet.com\talways"))

      UserLookup.create.username("Asutherl").email("alan.sutherland@hstx.com").name("").user(u1).save
      UserLookup.create.username("Sabernethy").email("scott_abernethy@stratexnet.com").name("").user(u2).save
      UserLookup.create.username("scott.abernethy@aviatnet.com").email("scott.abernethy@Aviatnet.com").name("").user(u2).save

      x.backend.updateSubscriptions(d, subsA :: subsB :: subsC :: Nil)

      d.reload.subscribers must haveTheSameElementsAs(u1 :: u2 :: Nil)
    }

    "not duplicate existing subscriptions, and purge existing that a no longer listed" >>
    {
      TestDbVendor.initAndClean()
      val (p1, _, _) = TestDbVendor.createProjects
      val (u1, u2) = TestDbVendor.createUsers
      val d: Document = Document.create.key("1234").project(p1).title("FooBarBaz")
      d.save

      val subsA = new AgentSubscriber(new TabLine("Asutherl\talan.sutherland@hstx.com\talways"))
      UserLookup.create.username("Asutherl").email("alan.sutherland@hstx.com").name("").user(u1).save

      Subscription.create.document(d).user(u1).save
      Subscription.create.document(d).user(u2).save

      x.backend.updateSubscriptions(d, subsA :: Nil)

      d.reload.subscribers must haveTheSameElementsAs(u1 :: Nil)
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