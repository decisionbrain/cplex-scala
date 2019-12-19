/*
 *  Source file provided under Apache License, Version 2.0, January 2004,
 *  http://www.apache.org/licenses/
 *  (c) Copyright DecisionBrain SAS 2016,2019
 */

package com.decisionbrain.cplex.mp

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FunSuite, Matchers}

/**
 * Created by dgodard on 06/02/2017.
 */
@RunWith(classOf[JUnitRunner])
class NursesTest extends FunSuite with Matchers {

  private val epsilon = 1e-6

  test("Nurses") {

    val nursesModel = Nurses
    val model = nursesModel.buildModel()
    val status = nursesModel.solve(model)

    implicit val order = Ordering.Double.IeeeOrdering

    model.getObjValue() should be <= 29975.5
    model.getValue(nursesModel.totalSalaryCost) should equal(29736.0 +- epsilon)
    model.getValue(nursesModel.totalNumberOfAssignments) should equal(224.0 +- epsilon)
    model.getValue(nursesModel.averageNurseWorkTime) should equal(39.75 +- epsilon)
    model.getValue(nursesModel.totalFairness) should be <= 15.5
    model.end()
  }

  test("Nurses2") {

    val nursesModel = Nurses2

    val model = nursesModel.buildModel()
    val status = nursesModel.solve(model)

    implicit val order = Ordering.Double.IeeeOrdering

    model.getObjValue() should be <= 29975.5
    model.getValue(nursesModel.totalSalaryCost) should equal(29736.0 +- epsilon)
    model.getValue(nursesModel.totalNumberOfAssignments) should equal(224.0 +- epsilon)
    model.getValue(nursesModel.averageNurseWorkTime) should equal(39.75 +- epsilon)
    model.getValue(nursesModel.totalFairness) should be <= 15.5
    model.end()
  }

}