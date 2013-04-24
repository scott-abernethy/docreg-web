/*
 * Copyright (c) 2013 Scott Abernethy.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package vvv.docreg.backend

import org.specs.mock.Mockito
import org.specs.Specification
import org.squeryl.PrimitiveTypeMode._
import vvv.docreg.db.TestDbVendor
import vvv.docreg.model.{UserLookupProvider, Subscription}
import vvv.docreg.agent.SubscriberInfo
import net.liftweb.common.Full
import org.mockito.Matchers

class SubscriptionReconcileTest extends Specification with Mockito {
  "SubscritionReconcile" should {
    "Do nothing for no subscriptions" >> {
      TestDbVendor.initAndClean()
      transaction{
        val (p1, _, _) = TestDbVendor.createProjects
        val (u1, u2) = TestDbVendor.createUsers
        val (d, _, _, _) = TestDbVendor.createDocument(p1, u1)

        val lookup = mock[UserLookupProvider]
        val x = new SubscriptionReconcile {
          val userLookup = lookup
        }

        x.reconcileSubscriptions(d, Nil)

        Subscription.forDocument(d) must beEmpty
      }
    }

    "Parse options to either notification or bookmark" >> {
      TestDbVendor.initAndClean()
      transaction{
        val (p1, _, _) = TestDbVendor.createProjects
        val (u1, u2) = TestDbVendor.createUsers
        val (d, _, _, _) = TestDbVendor.createDocument(p1, u1)

        val lookup = mock[UserLookupProvider]
        lookup.lookup(Matchers.eq(Some("jroads")), Matchers.eq(Some("j@f.com")), Matchers.eq(None), Matchers.anyString()) returns(Full(u1))

        val x = new SubscriptionReconcile {
          val userLookup = lookup
        }

        "always option is notification" >> {
          Subscription.unsubscribe(d, u1)
          Subscription.forDocument(d) must beEmpty

          x.reconcileSubscriptions(d, SubscriberInfo("jroads", "j@f.com", "always") :: Nil)

          val ss = Subscription.forDocument(d)
          ss must haveSize(1)
          ss(0).userId must be_==(1)
          ss(0).documentId must be_==(1)
          ss(0).notification must be_==(true)
          ss(0).bookmark must be_==(false)
        }

        "bookmark option is bookmark" >> {
          Subscription.unsubscribe(d, u1)
          Subscription.forDocument(d) must beEmpty

          x.reconcileSubscriptions(d, SubscriberInfo("jroads", "j@f.com", "bookmark") :: Nil)

          val ss = Subscription.forDocument(d)
          ss must haveSize(1)
          ss(0).userId must be_==(1)
          ss(0).documentId must be_==(1)
          ss(0).notification must be_==(false)
          ss(0).bookmark must be_==(true)
        }

        "both options are possible" >> {
          Subscription.unsubscribe(d, u1)
          Subscription.forDocument(d) must beEmpty

          x.reconcileSubscriptions(d, SubscriberInfo("jroads", "j@f.com", "bookmark always") :: Nil)

          val ss = Subscription.forDocument(d)
          ss must haveSize(1)
          ss(0).userId must be_==(1)
          ss(0).documentId must be_==(1)
          ss(0).notification must be_==(true)
          ss(0).bookmark must be_==(true)
        }

        "even when other junk exists in options" >> {
          Subscription.unsubscribe(d, u1)
          Subscription.forDocument(d) must beEmpty

          x.reconcileSubscriptions(d, SubscriberInfo("jroads", "j@f.com", "stuff always foo bar") :: Nil)

          val s = Subscription.forDocument(d).apply(0)
          s.notification must be_==(true)
          s.bookmark must be_==(false)
        }

        "currently the mail files have no space separation in options" >> {
          Subscription.unsubscribe(d, u1)
          Subscription.forDocument(d) must beEmpty

          x.reconcileSubscriptions(d, SubscriberInfo("jroads", "j@f.com", "alwaysalwaysalways") :: Nil)

          val s = Subscription.forDocument(d).apply(0)
          s.notification must be_==(true)
          s.bookmark must be_==(false)
        }

        "currently the mail files have no space separation in options" >> {
          Subscription.unsubscribe(d, u1)
          Subscription.forDocument(d) must beEmpty

          x.reconcileSubscriptions(d, SubscriberInfo("jroads", "j@f.com", "alwaysnomailbookmarkalwaysalways") :: Nil)

          val s = Subscription.forDocument(d).apply(0)
          s.notification must be_==(true)
          s.bookmark must be_==(true)
        }
      }
    }

    "add multiple subscriptions, ignoring duplicates" >> {
      TestDbVendor.initAndClean()
      transaction{
        val (p1, _, _) = TestDbVendor.createProjects
        val (u1, u2) = TestDbVendor.createUsers
        val (d, _, _, _) = TestDbVendor.createDocument(p1, u1)

        val lookup = mock[UserLookupProvider]
        lookup.lookup(Matchers.eq(Some("Asutherl")), Matchers.eq(Some("alan.sutherland@hstx.com")), Matchers.eq(None), Matchers.anyString()) returns(Full(u2))
        lookup.lookup(Matchers.eq(Some("Sabernethy")), Matchers.eq(Some("scott_abernethy@stratexnet.com")), Matchers.eq(None), Matchers.anyString()) returns(Full(u1))
        lookup.lookup(Matchers.eq(Some("scott.abernethy@aviatnet.com")), Matchers.eq(Some("scott.abernethy@Aviatnet.com")), Matchers.eq(None), Matchers.anyString()) returns(Full(u1))

        val x = new SubscriptionReconcile {
          val userLookup = lookup
        }

        val subsA = SubscriberInfo("Asutherl","alan.sutherland@hstx.com","always")
        val subsB = SubscriberInfo("Sabernethy","scott_abernethy@stratexnet.com","always")
        val subsC = SubscriberInfo("scott.abernethy@aviatnet.com","scott.abernethy@Aviatnet.com","always bookmark")

        x.reconcileSubscriptions(d, List(subsA, subsB, subsC))

        val ss = Subscription.forDocument(d)
        ss must haveSize(2)
        ss(0).userId must be_==(2)
        ss(0).documentId must be_==(1)
        ss(0).notification must be_==(true)
        ss(0).bookmark must be_==(false)
        ss(1).userId must be_==(1)
        ss(1).documentId must be_==(1)
        ss(1).notification must be_==(true)
        ss(1).bookmark must be_==(true)
      }
    }

    "No change" >> {
      TestDbVendor.initAndClean()
      transaction{
        val (p1, _, _) = TestDbVendor.createProjects
        val (u1, u2) = TestDbVendor.createUsers
        val (d, _, _, _) = TestDbVendor.createDocument(p1, u1)

        val lookup = mock[UserLookupProvider]
        lookup.lookup(Matchers.eq(Some("fg")), Matchers.eq(Some("fg@google.com")), Matchers.eq(None), Matchers.anyString()) returns(Full(u2))

        val x = new SubscriptionReconcile {
          val userLookup = lookup
        }
        "for subsequent reconciles" >> {
          x.reconcileSubscriptions(d, SubscriberInfo("fg", "fg@google.com", "always") :: Nil)
          x.reconcileSubscriptions(d, SubscriberInfo("fg", "fg@google.com", "always") :: Nil)
          x.reconcileSubscriptions(d, SubscriberInfo("fg", "fg@google.com", "always") :: Nil)

          val ss = Subscription.forDocument(d)
          ss must haveSize(1)
          ss(0).userId must be_==(2)
          ss(0).documentId must be_==(1)
          ss(0).notification must be_==(true)
          ss(0).bookmark must be_==(false)
        }

        "unless the represent a change in subscription option" >> {
          x.reconcileSubscriptions(d, SubscriberInfo("fg", "fg@google.com", "bookmark") :: Nil)

          val ss = Subscription.forDocument(d)
          ss must haveSize(1)
          ss(0).userId must be_==(2)
          ss(0).documentId must be_==(1)
          ss(0).notification must be_==(false)
          ss(0).bookmark must be_==(true)
        }
      }
    }

    "Remove subscriptions if no longer there" >> {
      TestDbVendor.initAndClean()
      transaction{
        val (p1, _, _) = TestDbVendor.createProjects
        val (u1, u2) = TestDbVendor.createUsers
        val (d, _, _, _) = TestDbVendor.createDocument(p1, u1)

        val lookup = mock[UserLookupProvider]
        lookup.lookup(Matchers.eq(Some("jroads")), Matchers.eq(Some("j@f.com")), Matchers.eq(None), Matchers.anyString()) returns(Full(u1))

        val x = new SubscriptionReconcile {
          val userLookup = lookup
        }

        "when no subscriptions should exist" >> {
          Subscription.subscribe(d, u1)
          Subscription.subscribe(d, u2)
          Subscription.forDocument(d) must haveSize(2)

          x.reconcileSubscriptions(d, Nil)
          Subscription.forDocument(d) must beEmpty
        }

        "when only some of the existing subscriptions should now exist" >> {
          Subscription.subscribe(d, u1)
          Subscription.subscribe(d, u2)
          Subscription.forDocument(d) must haveSize(2)

          x.reconcileSubscriptions(d, SubscriberInfo("jroads", "j@f.com", "always") :: Nil)
          val ss = Subscription.forDocument(d)
          ss must haveSize(1)
          ss(0).userId must be_==(1)
          ss(0).documentId must be_==(1)
          ss(0).notification must be_==(true)
          ss(0).bookmark must be_==(false)
        }
      }
    }
  }
}
