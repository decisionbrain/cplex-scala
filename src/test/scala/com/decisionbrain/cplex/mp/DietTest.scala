/*
 * Source file provided under Apache License, Version 2.0, January 2004,
 * http://www.apache.org/licenses/
 * (c) Copyright DecisionBrain SAS 2016,2018
 */

package com.decisionbrain.cplex.mp

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FunSuite, Matchers}

/**
  * Created by dgodard on 07/02/2017.
  */
@RunWith(classOf[JUnitRunner])
class DietTest extends FunSuite with Matchers {

  private val epsilon = 1e-6

  test("Diet") {
    val model = Diet.buildDietModel()
    val status = model.solve()

    status should equal(true)
    model.getObjectiveValue() should equal(2.690409 +- epsilon)
    model.end()

  }
}
