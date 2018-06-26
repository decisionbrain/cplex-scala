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
 * Created by dgodard on 06/02/2017.
 */
@RunWith(classOf[JUnitRunner])
class ProductionTest extends FunSuite with Matchers {

  private val epsilon = 1e-6

  test("Production") {

    val model = Production.buildProductionProblem()
    val status = model.solve()

    status should equal(true)
    model.getObjectiveValue() should equal(372.0 +- epsilon)
    model.getValue(Production.totalInsideCost) should equal(24.0 +- epsilon)
    model.getValue(Production.totalOutsideCost) should equal(348.0 +- epsilon)
    model.end()

  }

}