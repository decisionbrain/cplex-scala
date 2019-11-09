/*
 *  Source file provided under Apache License, Version 2.0, January 2004,
 *  http://www.apache.org/licenses/
 *  (c) Copyright DecisionBrain SAS 2016,2019
 */

package com.decisionbrain.cplex.mp

import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner
import org.scalatest.{FunSuite, Matchers}

import scala.reflect.io.File

/**
 *
 */

@RunWith(classOf[JUnitRunner])
class SailcoPWTest  extends FunSuite with Matchers {

  private val epsilon = 1e-6

  test("sailcopw") {
    val model = SailcoPW.buildModel()

    model.exportModel("sailcopw.lp")

    val status = model.solve()

    status should be (true)
    model.getObjValue() should equal (78450.0 +- epsilon)

    model.end()
  }

}