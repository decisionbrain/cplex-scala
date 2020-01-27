/*
 * Source file provided under Apache License, Version 2.0, January 2004,
 *  http://www.apache.org/licenses/
 *  (c) Copyright DecisionBrain SAS 2016,2020
 *
 */

package com.decisionbrain.cplex.cp

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FunSuite, Matchers}

@RunWith(classOf[JUnitRunner])
class SchedCalendarTest extends FunSuite with Matchers {

  val epsilon = 1e-6

  test("SchedCalendar") {
    val model = SchedCalendar.build()

    val status = model.solve()

    status should equal(true)
    model.getObjectiveValue() should equal(638.0 +- epsilon)

    SchedCalendar.allTaskVars("H1-masonry") should not equal None
    val joeFirstTask = SchedCalendar.allTaskVars("H1-masonry")
    model.getStart(joeFirstTask) should equal (0)
    model.getEnd(joeFirstTask) should equal (54)
    model.getSize(joeFirstTask) should equal (35)
    model.getLength(joeFirstTask) should equal (54)

    model.end()
  }
}
