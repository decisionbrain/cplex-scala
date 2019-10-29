/*
 *  Source file provided under Apache License, Version 2.0, January 2004,
 *  http://www.apache.org/licenses/
 *  (c) Copyright DecisionBrain SAS 2016,2019
 */

package com.decisionbrain.cplex.mp

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FunSuite, Matchers}

/**
  * Created by dgodard on 07/02/2017.
  */
@RunWith(classOf[JUnitRunner])
class IndefQPex1Test extends FunSuite with Matchers {

  private val epsilon = 1e-6

  test("IndefQPex1") {
    val model = IndefQPex1.buildModel()
    val status = IndefQPex1.solve()

    status should equal(true)
    model.getObjValue() should equal(-2.5 +- epsilon)

    model.end()
  }
}
