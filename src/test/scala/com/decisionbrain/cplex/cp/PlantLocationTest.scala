/*
 *  Source file provided under Apache License, Version 2.0, January 2004,
 *  http://www.apache.org/licenses/
 *  (c) Copyright DecisionBrain SAS 2016,2019
 */

package com.decisionbrain.cplex.cp

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FunSuite, Matchers}

@RunWith(classOf[JUnitRunner])
class PlantLocationTest extends FunSuite with Matchers {

  val epsilon = 1e-6

  test("PlantLocation") {
    val model = PlantLocation.build("data/plant_location.data")
    val status = PlantLocation.solve(failLimit=50000, logPeriod = 10000)

    status should equal(true)

    model.getObjectiveValue() should be <= 70908.0

    model.end()
  }
}
