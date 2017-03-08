/*
 * Source file provided under Apache License, Version 2.0, January 2004,
 * http://www.apache.org/licenses/
 * (c) Copyright DecisionBrain SAS 2016,2017
 */

package com.decisionbrain.cplex.cp

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FunSuite, Matchers}

@RunWith(classOf[JUnitRunner])
class SchedSquareTest extends FunSuite with Matchers {

  val epsilon = 1e-6

  test("SchedSquare") {
    val model = SchedSquare.build()
    val status = model.solve()

    status should equal(true)

    model.end()
  }
}
