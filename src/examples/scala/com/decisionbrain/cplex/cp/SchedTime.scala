/*
 *  Source file provided under Apache License, Version 2.0, January 2004,
 *  http://www.apache.org/licenses/
 *  (c) Copyright DecisionBrain SAS 2016,2019
 */

package com.decisionbrain.cplex.cp

import com.decisionbrain.cplex.NumExpr
import com.decisionbrain.cplex.Modeler._
import com.decisionbrain.cplex.cp.CpModel._

/**
  * This is a problem of building a house. The masonry, roofing, painting, etc. must be scheduled.  Some tasks must
  * necessarily take place before others and these requirements are expressed through precedence constraints.
  *
  * Moreover, there are earliness and tardiness costs associated with some tasks. The objective is to minimize these
  * costs.
  */
object SchedTime {

  val nbTasks   = 10

  val tasks = List(
    ("masonry", 35), // pair of task name and duration
    ("carpentry", 15),
    ("plumbing", 40),
    ("ceiling", 15),
    ("roofing",  5),
    ("painting", 10),
    ("windows",  5),
    ("facade", 10),
    ("garden",  5),
    ("moving",  5)
  )

  def computeEarlinessCostExp(task: IntervalVar, rd: Double, weight: Double, useFunction: Boolean): NumExpr = {
    if (useFunction) {
      val arrX = Array(rd)
      val arrV = Array(-weight, 0.0)
      val f = model.piecewiseLinearFunction(arrX, arrV, rd, 0.0)
      startEval(task,f)
    } else {
      weight * max(.0, rd - startOf(task))
    }
  }

  def computeTardinessCostExp(task: IntervalVar, dd: Double, weight: Double, useFunction: Boolean): NumExpr = {
    if (useFunction) {
      val arrX = Array(dd)
      val arrV = Array(0.0, weight)
      val f = model.piecewiseLinearFunction(arrX, arrV, dd, 0.0)
      endEval(task,f)
    } else {
      weight * max(.0, endOf(task) - dd)
    }
  }

  implicit var model: CpModel = _

  var taskVars: Map[String, IntervalVar] = _

  def build(): CpModel = {

    model = CpModel("SchedCumul")

    taskVars = (for (task <- tasks; (tname, tduration) = task)
      yield (tname , model.intervalVar(sizeMin = tduration, sizeMax = tduration, name = tname))).toMap

    // precedence constraints
    model.add(taskVars("masonry") < taskVars("carpentry"))
    model.add(taskVars("masonry") < taskVars("plumbing"))
    model.add(taskVars("masonry") < taskVars("ceiling"))
    model.add(taskVars("carpentry") < taskVars("roofing"))
    model.add(taskVars("ceiling") < taskVars("painting"))
    model.add(taskVars("roofing") < taskVars("windows"))
    model.add(taskVars("roofing") < taskVars("facade"))
    model.add(taskVars("plumbing") < taskVars("facade"))
    model.add(taskVars("roofing") < taskVars("garden"))
    model.add(taskVars("plumbing") < taskVars("garden"))
    model.add(taskVars("windows") < taskVars("moving"))
    model.add(taskVars("facade") < taskVars("moving"))
    model.add(taskVars("garden") < taskVars("moving"))
    model.add(taskVars("painting") < taskVars("moving"))

    val useFunction = true
    val costExpr = (computeEarlinessCostExp(taskVars("masonry"), 25, 200.0, useFunction)
      + computeEarlinessCostExp(taskVars("carpentry"), 75, 300.0, useFunction)
      + computeEarlinessCostExp(taskVars("ceiling"), 75, 100.0, useFunction)
      + computeTardinessCostExp(taskVars("moving"), 100, 400.0, useFunction))


    model.add(minimize(costExpr))

    model
  }

  def solve(): Boolean = {

    println(s"Solving model $model....")

//    model.exportModel("SchedTime.cpo")

    //    val status = model.solve(timeLimit=20, logPeriod=3000)
    val status = model.solve()

    if (status) {
      println(s"Solution status: $status")
      println("Solution with objective " + model.getObjectiveValue())
      for ((name, task) <- taskVars) {
        println(model.getDomain(task))
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
