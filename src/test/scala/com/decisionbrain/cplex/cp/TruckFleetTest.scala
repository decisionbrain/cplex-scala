/*
 * Source file provided under Apache License, Version 2.0, January 2004,
 * http://www.apache.org/licenses/
 * (c) Copyright DecisionBrain SAS 2016,2017
 */

package com.decisionbrain.cplex.cp

import com.decisionbrain.cplex.cp.Color._
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
class TruckFleetTest extends FunSuite with Matchers {

  val epsilon = 1e-6

  test("TruckFleet") {
    val model = TruckFleet.build()
    val status = model.solve(timeLimit=30, logPeriod=3000)

    status should equal(true)

    model.getObjectiveValue() should equal (26.0 +- epsilon)
    model.getValue(TruckFleet.obj1) should equal (26)
    model.getValue(TruckFleet.obj2) should equal (13)
    model.getValue(TruckFleet.numUsed) should equal (13)

    model.end()
  }
}
