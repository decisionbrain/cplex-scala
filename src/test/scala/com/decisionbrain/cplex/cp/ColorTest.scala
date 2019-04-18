/*
 *  Source file provided under Apache License, Version 2.0, January 2004,
 *  http://www.apache.org/licenses/
 *  (c) Copyright DecisionBrain SAS 2016,2019
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
class ColorTest extends FunSuite with Matchers {

  test("Color") {
    val model = Color.build()
    val status = model.solve()

    status should equal(true)

    val Belgium = model.getValue(colors("Belgium"))
    val France = model.getValue(colors("France"))
    val Germany = model.getValue(colors("Germany"))
    val Netherlands = model.getValue(colors("Netherlands"))
    val Luxembourg = model.getValue(colors("Luxembourg"))
    val Denmark = model.getValue(colors("Denmark"))

    Belgium should not equal France
    Belgium should not equal Germany
    Belgium should not equal Netherlands
    Belgium should not equal Luxembourg
    Denmark should not equal Germany
    France should not equal Germany
    France should not equal Luxembourg
    Germany should not equal Luxembourg
    Germany should not equal Netherlands

    model.end()
  }
}
