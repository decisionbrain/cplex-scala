/*
 * Source file provided under Apache License, Version 2.0, January 2004,
 * http://www.apache.org/licenses/
 * (c) Copyright DecisionBrain SAS 2016,2018
 */

package com.decisionbrain.cplex.cp

import ilog.cp.IloCP
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FunSuite, Matchers}

@RunWith(classOf[JUnitRunner])
class SchedStateTest extends FunSuite with Matchers {

  val epsilon = 1e-6

  test("SchedState") {
    val model = SchedState.build()

    model.cp.setParameter(IloCP.IntParam.FailLimit, 10000)
    val status = model.solve()

    status should equal(true)
    model.getObjectiveValue() should equal(381.0 +- epsilon)

    model.end()
  }
}
