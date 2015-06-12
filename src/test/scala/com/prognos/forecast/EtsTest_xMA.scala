package com.ets.forecast

/**
 * Created by nikhil.vavs on 11/06/15.
 */

import org.scalatest.{FlatSpec,Matchers}

class EtsTest_xMA extends FlatSpec with Matchers{
  it should "testing none as all are forbidden" in {
    // ama is not allowed
    1 should equal(1)   // :P
  }
}
