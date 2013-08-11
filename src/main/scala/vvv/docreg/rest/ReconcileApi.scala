/*
 * Copyright (c) 2013 Aviat Networks.
 * This file is part of DocReg+Web. Please refer to the NOTICE.txt file for license details.
 */

package vvv.docreg.rest

import net.liftweb.http.rest.RestHelper
import net.liftweb.common.{Full, Loggable}
import net.liftweb.http.{PlainTextResponse}
import vvv.docreg.util.Environment

object ReconcileApi extends RestHelper with Loggable {

  serve ("api" / "reconcile" prefix {
    case Nil Get req => {
      logger.debug("Resync requested")
      Environment.env.backend ! 'Resync
      Full(PlainTextResponse("Resync requested " ))
    }
  })

}
