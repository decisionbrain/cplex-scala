/*
 *  Source file provided under Apache License, Version 2.0, January 2004,
 *  http://www.apache.org/licenses/
 *  (c) Copyright DecisionBrain SAS 2016,2019
 */

package com.decisionbrain.cplex.cp

import com.decisionbrain.cplex.IntExpr
import com.decisionbrain.cplex.Modeler._
import com.decisionbrain.cplex.cp.CpModel._
import ilog.cp.IloCP

/**
  * This is a problem of building five houses in different locations. The masonry, roofing, painting, etc. must be
  * scheduled. Some tasks must necessarily take place before others and these requirements are expressed through
  * precedence constraints.
  * There are three workers, and each task requires a worker.  There is also a cash budget which starts with a given
  * balance.  Each task costs a given amount of cash per day which must be available at the start of the task. A cash
  * payment is received periodically. The objective is to minimize the overall completion date.
  */
object SchedCumul {

  val nbWorkers = 3
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


  implicit var model: CpModel = _

  var allTaskVars: Vector[IntervalVar] = _
  var endVars: List[IntExpr] = _

  var workersUsage: CumulFunctionExpr  = _
  var cash: CumulFunctionExpr = _


  def makeHouse(id: Int, releaseDate: Int): (IntExpr, IntervalVarArray) = {

    val taskVars: Map[String, IntervalVar] = (for (task <- tasks; (tname, tduration) = task)
          yield (tname , model.intervalVar(sizeMin = tduration, sizeMax = tduration, name = "H" + id + "-" + tname))).toMap

    workersUsage += model.sum(for (e <- taskVars; (name, v) = e) yield pulse(v, 1))

    cash -= model.sum(for (e <- taskVars; (name, v) = e) yield stepAtStart(v, 200 * v.getSizeMin))

    taskVars("masonry").setStartMin(releaseDate)


    // The lines below are equivalent
//    model.add(endBeforeStart(taskVars("masonry"),   taskVars("carpentry")))
//    model.add(endBeforeStart(taskVars("masonry"),   taskVars("plumbing")))
//    model.add(endBeforeStart(taskVars("masonry"),   taskVars("ceiling")))
//    model.add(endBeforeStart(taskVars("carpentry"), taskVars("roofing")))
//    model.add(endBeforeStart(taskVars("ceiling"),   taskVars("painting")))
//    model.add(endBeforeStart(taskVars("roofing"),   taskVars("windows")))
//    model.add(endBeforeStart(taskVars("roofing"),   taskVars("facade")))
//    model.add(endBeforeStart(taskVars("plumbing"),  taskVars("facade")))
//    model.add(endBeforeStart(taskVars("roofing"),   taskVars("garden")))
//    model.add(endBeforeStart(taskVars("plumbing"),  taskVars("garden")))
//    model.add(endBeforeStart(taskVars("windows"),   taskVars("moving")))
//    model.add(endBeforeStart(taskVars("facade"),    taskVars("moving")))
//    model.add(endBeforeStart(taskVars("garden"),    taskVars("moving")))
//    model.add(endBeforeStart(taskVars("painting"),  taskVars("moving")))
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

    (model.endOf(taskVars("moving")), taskVars.values)
  }

  def build(): CpModel = {

    model = CpModel("SchedCumul")

    workersUsage = model.cumulFunctionExpr()
    cash = model.cumulFunctionExpr()

    /* CASH PAYMENTS. */
    for (p <- 0 until 5)
      cash += step(60*p, 30000)

    val results = List(
      makeHouse(0,  31),
      makeHouse(1,   0),
      makeHouse(2,  90),
      makeHouse(3, 120),
      makeHouse(4,  90))

    // lines below are equivalents
//    model.add(IntExpr(0) <= cash)
    model.add(cash >= 0)

    model.add(workersUsage <= nbWorkers)

    endVars = results.map(_._1)
    allTaskVars = results.flatMap(_._2).toVector

    model.add(minimize(max(endVars)))

    model
  }

  def solve(): Boolean = {

    println(s"Solving model $model....")

//    model.exportModel("SchedCumul.cpo")

    model.cp.setParameter(IloCP.IntParam.FailLimit, 10000)

    //    val status = model.solve(timeLimit=20, logPeriod=3000)
    val status = model.solve()

    if (status) {
      println(s"Solution status: $status")
      println("Solution with objective " + model.getObjectiveValue())
      for (v <- allTaskVars) {
        println(model.getDomain(v))
      }
      for (s <- cash) {
        println("Cash is " + s.value +
          " in [" + s.start +
          " .. " + s.end +
          ")"
        )
      }
      for (s <- workersUsage) {
        println(
          "# Workers is " + s.value +
            " in [" + s.start +
            " .. " + s.end +
            ")"
        )
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
