/*
 * Source file provided under Apache License, Version 2.0, January 2004,
 *  http://www.apache.org/licenses/
 *  (c) Copyright DecisionBrain SAS 2016,2020
 *
 */

package com.decisionbrain.cplex.mp

import com.decisionbrain.cplex.Modeler._
import com.decisionbrain.cplex.NumVar
import com.decisionbrain.cplex.Range
import ilog.cplex.IloCplex


/**
  * Build and solve an simple quadratic programming problem:
  *
  * Minimize
  *   obj:   0.5 (-3 * x^2 - 3 * y^2 - 1 * x * y)
  * Subject To
  *   c1: -x + y >= 0
  *   c2:  x + y >= 0
  * Bounds
  *   -1 <= x <= 1
  *    0 <= y <= 1
  * End
  *
  */
object IndefQPex1 {

  implicit var model: MpModel = _

  var vars: Array[NumVar] = _
  var ranges: Array[Range] = _

  def buildModel(): MpModel = {

    model = MpModel("IndexQPex1")

    val x = model.numVar(-1, 1, "x")
    val y = model.numVar(0, 1, "y")

    vars = Array(x, y)

    val r1 = -x + y >= 0
    val r2 =  x + y >= 0

    ranges = Array(r1, r2)

    model.add(r1, r2)

    val objective = minimize(0.5 * (-3.0 * x * x - 3.0 * y * y - 1.0 * x * y))

    model.add(objective)

    model.printInformation()

    model
  }

  def solveAndDisplay(): Boolean = {

    val status = model.solve()

    if (status) {
      val values = model.getValues(vars)
      val reducedCosts = model.getReducedCosts(vars)
      val duals = model.getDuals(ranges)
      val slacks = model.getSlacks(ranges)

      System.out.println("Solution status = " + model.getStatus)
      System.out.println("Solution value  = " + model.getObjValue())

      for (j <- vars.indices) {
        val value = values(j)
        val reducedCost: Double = reducedCosts(j)
        println(s"Variable $j: Value = $value Reduced cost = $reducedCost")
      }

      for (i <- ranges.indices) {
        val slack = slacks(i)
        val dual: Double = model.getDual(ranges(i))
        println(s"Constraint $i: Slack = $slack Pi = $dual")
      }
    }

    status
  }

  def solve(): Boolean = {

    // When a non-convex objective function is present, CPLEX
    // will raise an exception unless the parameter
    // IloCplex.Param.OptimalityTarget is set to accept
    // first-order optimal solutions
    model.setParam(IloCplex.Param.OptimalityTarget, IloCplex.OptimalityTarget.FirstOrder)

    // CPLEX may converge to either local optimum
    solveAndDisplay()

    val x = vars(0)

    // 0 <= x
    val r1 = x >= 0.0
    model.add(r1)
    ranges = ranges :+ r1 // add the new range in the array

    solveAndDisplay()

    // Remove the newly added constraint and add a new constraint
    // with the opposite sense to cut off the solution at (1, 1)
    model.remove(r1)
    ranges = ranges.dropRight(1)
    val r2 = x <= 0.0
    model.add(r2)
    ranges = ranges :+ r2 // add the new range in the array

    solveAndDisplay()
  }

  def main(args: Array[String]): Unit = {

    val model = buildModel()

    solve()

    model.end()
  }
}
