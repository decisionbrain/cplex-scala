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
class LearningCurve2Test extends FunSuite with Matchers {

  val epsilon = 1e-6

  test("LearningCurve2") {
    val model = LearningCurve2.build()

    val status = LearningCurve2.solve(failLimit=100000, logPeriod=10000)
//    val status = LearningCurve2.solve(timeLimit=600, logPeriod=10000)

    status should equal(true)
    // Note: a solution with objective 174 can be obtained by increasing the fail limit (see method solve)
    model.getObjectiveValue() should be <= 300.0

    model.end()
  }
}
