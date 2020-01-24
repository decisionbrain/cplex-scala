/*
 * Source file provided under Apache License, Version 2.0, January 2004,
 *  http://www.apache.org/licenses/
 *  (c) Copyright DecisionBrain SAS 2016,2020
 *
 */

package com.decisionbrain.cplex.mp

import org.junit.runner.RunWith
import org.scalatest.{FunSuite, Matchers}
import org.scalatestplus.junit.JUnitRunner

/**
 *
 */

@RunWith(classOf[JUnitRunner])
class TransportTest  extends FunSuite with Matchers {

  private val epsilon = 1e-6

  test("transport_convex") {

    val transport = Transport("0") // convex case

    transport.isEmpty should not be (true)
    val model = transport.get

    //    model.exportModel("transport.lp")

    val status = model.solve()

    status should be (true)
    model.getObjValue() should equal (140500.0 +- epsilon)

    model.end()
  }

  test("transport_concave") {

    val transport = Transport("1") // convex case

    transport.isEmpty should not be (true)
    val model = transport.get

    //    model.exportModel("transport.lp")

    val status = model.solve()

    status should be (true)
    model.getObjValue() should equal (238500.0 +- epsilon)

    model.end()
  }

}