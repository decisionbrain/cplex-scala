/*
 *  Source file provided under Apache License, Version 2.0, January 2004,
 *  http://www.apache.org/licenses/
 *  (c) Copyright DecisionBrain SAS 2016,2019
 */

package com.decisionbrain.cplex.mp

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FunSuite, Matchers}

import scala.reflect.io.File

/**
 *
 */

@RunWith(classOf[JUnitRunner])
class BendersTest  extends FunSuite with Matchers {

  private val epsilon = 1e-6

  test("Benders UFL_25_35_1.mps UFL_25_35_1.ann") {
    val benders = Benders("src/examples/data/mp/UFL_25_35_1.mps", "src/examples/data/mp/UFL_25_35_1.ann")

    benders.isEmpty should not be (true)

    val model = benders.get

    val status = model.solve()

    status should be (true)
    model.getObjValue() should equal (245.0 +- epsilon)
    model.end()
  }

  test("Benders UFL_25_35_1.mps create") {
    val benders = Benders("src/examples/data/mp/UFL_25_35_1.mps", "create")

    benders.isEmpty should not be (true)

    val model = benders.get

    val status = model.solve()

    status should be (true)
    model.getObjValue() should equal (245.0 +- epsilon)
    model.end()
  }

  test("Benders  UFL_25_35_1.mps") {
    val benders = Benders("src/examples/data/mp/UFL_25_35_1.mps")

    benders.isEmpty should not be (true)

    val model = benders.get

    val status = model.solve()

    status should be (true)
    model.getObjValue() should equal (245.0 +- epsilon)
    File("benders.ann").exists should be (true)

    // remove generated "benders.ann" file
    val file = File("benders.ann")
    var isDeleted = false
    if (file.exists) {
      isDeleted = file.delete()
    }
    isDeleted should be (true)

    model.end()
  }
}