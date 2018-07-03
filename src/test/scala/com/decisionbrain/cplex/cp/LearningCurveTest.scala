/*
 * Source file provided under Apache License, Version 2.0, January 2004,
 * http://www.apache.org/licenses/
 * (c) Copyright DecisionBrain SAS 2016,2018
 */

package com.decisionbrain.cplex.cp

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FunSuite, Matchers}

@RunWith(classOf[JUnitRunner])
class LearningCurveTest extends FunSuite with Matchers {

  val epsilon = 1e-6

  test("LearningCurve") {
    val model = LearningCurve.build()

    val status = LearningCurve.solve()

    status should equal(true)
    // Note: a solution with objective 174 can be obtained by increasing the fail limit (see method solve)
    model.getObjectiveValue() should be <= 269.0

    model.end()
  }
}
