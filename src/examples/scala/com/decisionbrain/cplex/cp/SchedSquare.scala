/*
 * Source file provided under Apache License, Version 2.0, January 2004,
 * http://www.apache.org/licenses/
 * (c) Copyright DecisionBrain SAS 2016,2018
 */

package com.decisionbrain.cplex.cp

import com.decisionbrain.cplex.cp.CpModel._
import com.decisionbrain.cplex.cp.CumulFunctionExprNumeric._

/**
  * The aim of the square example is to place a set of small squares of different sizes into a large square.
  */
object SchedSquare {

  val sizeSquare = 112  //
  val nbSquares = 21
  val size = Vector(50, 42, 37, 35, 33, 29, 27, 25, 24, 19, 18, 17, 16, 15, 11, 9, 8, 7, 6, 4, 2)

  implicit var model: CpModel = _
  var x: Vector[IntervalVar] = _
  var y: Vector[IntervalVar] = _

  def build(): CpModel = {

    model = CpModel("SchedSquare")

    x = (for (i <- 0 until nbSquares) yield model.intervalVar(size(i), "X("+i+")")).toVector
    y = (for (i <- 0 until nbSquares) yield model.intervalVar(size(i), "Y("+i+")")).toVector

    for (i <- 0 until nbSquares) {
      x(i).setEndMax(sizeSquare)
      y(i).setEndMax(sizeSquare)
    }

    // the lines below are all equivalent expressions
//    var rx: CumulFunctionExpr = (for (i <- 0 until nbSquares) yield model.pulse(x(i), size(i))).sum // add line: import com.decisionbrain.cplex.cp.CumulFunctionExprNumeric._
//    var ry: CumulFunctionExpr = (for (i <- 0 until nbSquares) yield model.pulse(y(i), size(i))).sum
    var rx: CumulFunctionExpr = model.sum(for (i <- 0 until nbSquares) yield model.pulse(x(i), size(i)))
    var ry: CumulFunctionExpr = model.sum(for (i <- 0 until nbSquares) yield model.pulse(y(i), size(i)))

    for (i <- 0 until nbSquares; j <- 0 until i) {
      model.add((endOf(x(i)) <= startOf(x(j)))
        || (endOf(x(j)) <= startOf(x(i)))
        || (endOf(y(i)) <= startOf(y(j)))
        || (endOf(y(j)) <= startOf(y(i)))
      )
    }

    model.add(alwaysIn(rx, 0, sizeSquare, sizeSquare, sizeSquare))
    model.add(alwaysIn(ry, 0, sizeSquare, sizeSquare, sizeSquare))

    // the lines below are all equivalent expressions
//    model.setSearchPhases(searchPhase(x.toArray), searchPhase(y.toArray))
    model.setSearchPhases(x.toSearchPhase, y.toSearchPhase)

    model
  }

  def solve(): Boolean = {

    println(s"Solving model $model....")

//    model.exportModel("SchedSquare.cpo")

//    val status = model.solve(timeLimit=20, logPeriod=3000)
    val status = model.solve()

    if (status) {
      println(s"Solution status: $status")
      for (i <- 0 until nbSquares) {
        println("Square " + i + ": ["
          + model.getStart(x(i)) + "," + model.getEnd(x(i))
          + "] x ["
          + model.getStart(y(i)) + "," + model.getEnd(y(i))
          + "]")
      }
    }

    true
  }

  def run(): Boolean = {
    val model = build()
    val status = solve()
    model.end()
    status
  }

  def main(args: Array[String]): Unit = {
    run()
  }

}
