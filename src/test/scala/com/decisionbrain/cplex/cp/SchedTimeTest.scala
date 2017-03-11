/*
 * Source file provided under Apache License, Version 2.0, January 2004,
 * http://www.apache.org/licenses/
 * (c) Copyright DecisionBrain SAS 2016,2017
 */

package com.decisionbrain.cplex.cp

import ilog.cp.IloCP
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FunSuite, Matchers}

@RunWith(classOf[JUnitRunner])
class SchedTimeTest extends FunSuite with Matchers {

  val epsilon = 1e-6

  test("SchedTime") {
    val model = SchedTime.build()

    val status = model.solve()

    status should equal(true)
    model.getObjectiveValue() should equal(5000.0 +- epsilon)

    model.getStart(SchedTime.taskVars("masonry")) should equal (20)
    model.getStart(SchedTime.taskVars("carpentry")) should equal (75)
    model.getStart(SchedTime.taskVars("plumbing")) should equal (55)
    model.getStart(SchedTime.taskVars("ceiling")) should equal (75)
    model.getStart(SchedTime.taskVars("roofing")) should equal (90)
    model.getStart(SchedTime.taskVars("painting")) should equal (90)
    model.getStart(SchedTime.taskVars("windows")) should equal (95)
    model.getStart(SchedTime.taskVars("facade")) should equal (95)
    model.getStart(SchedTime.taskVars("garden")) should equal (95)
    model.getStart(SchedTime.taskVars("moving")) should equal (105)

    model.end()
  }
}
