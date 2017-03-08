/*
 * Source file provided under Apache License, Version 2.0, January 2004,
 * http://www.apache.org/licenses/
 * (c) Copyright DecisionBrain SAS 2016,2017
 */

package com.decisionbrain.cplex.cp

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FunSuite, Matchers}

/**
  * Created by dgodard on 11/02/2017.
  */
/**
  * Created by dgodard on 07/02/2017.
  */
@RunWith(classOf[JUnitRunner])
class GolombTest extends FunSuite with Matchers {

  val epsilon = 1e-6

  test("Golomb") {

    val model = Golomb.build()
    val status = model.solve()

    status should equal(true)

    model.getObjectiveValue() should equal (34.0 +- epsilon)

    model.end()
  }
}
