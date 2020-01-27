/*
 * Source file provided under Apache License, Version 2.0, January 2004,
 *  http://www.apache.org/licenses/
 *  (c) Copyright DecisionBrain SAS 2016,2020
 *
 */

package com.decisionbrain.cplex.cp

import com.decisionbrain.cplex.{IntExpr, IntVar}
import com.decisionbrain.cplex.Modeler._
import com.decisionbrain.cplex.cp.CpModel._

/**
  * Created by dgodard on 11/02/2017.
  */
object Golomb {

  implicit var model: CpModel = _

  val ORDER: Int = 8                    // Number of marks
  val MAX_LENGTH: Int = (ORDER - 1) * (ORDER -1)  // Max rule length

  var marks: List[IntVar] = _

  def build(): CpModel = {

    model = CpModel("Golomb")

    marks = model.intVars(ORDER, 0, MAX_LENGTH, namer= (i: Int) => "marks_" + i)

    val dist: List[IntExpr] = (for (i <- 1 until ORDER; j <- 0 until i)
          yield marks(i) - marks(j)).toList

    model.add(allDiff(dist))

    model.add(marks(0) == 0)
    for (i <- 1 until ORDER)
      model.add(marks(i) > marks(i - 1))

    // Avoid mirror solution
    model.add((marks(1) - marks(0)) < (marks(ORDER - 1) - marks(ORDER - 2)))

    // Minimize ruler size (position of the last mark)
    model.add(minimize(marks(ORDER - 1)))

    model
  }

  def solve(): Boolean = {

    println(s"Solving model $model....")

//    model.exportModel("Golomb.cpo")

    val status = model.solve()

    if (status) {
      println(s"Solution status: $status")
      println("Position of ruler marks: ")
      for (v <- marks)
        print(" " + model.getValue(v))
      println()
    }

    status
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
