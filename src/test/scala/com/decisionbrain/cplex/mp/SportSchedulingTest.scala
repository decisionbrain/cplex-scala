/*
 * Source file provided under Apache License, Version 2.0, January 2004,
 *  http://www.apache.org/licenses/
 *  (c) Copyright DecisionBrain SAS 2016,2020
 *
 */

package com.decisionbrain.cplex.mp

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FunSuite, Matchers}

/**
 * Created by dgodard on 06/02/2017.
 */
@RunWith(classOf[JUnitRunner])
class SportSchedulingTest extends FunSuite with Matchers {

  private val epsilon = 1e-6

  test("SportScheduling") {

    val model = SportScheduling.buildSportSchedulingModel()
    val status = model.solve()

    status should equal(true)
    model.getObjValue() should equal(4448.0 +- epsilon)
    model.end()

  }


}