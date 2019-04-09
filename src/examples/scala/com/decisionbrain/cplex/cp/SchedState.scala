/*
 * Source file provided under Apache License, Version 2.0, January 2004,
 * http://www.apache.org/licenses/
 * (c) Copyright DecisionBrain SAS 2016,2018
 */

package com.decisionbrain.cplex.cp

import com.decisionbrain.cplex.IntExpr
import com.decisionbrain.cplex.cp.CpModel._
import ilog.cp.IloCP

/**
  * This is a problem of building five houses. The masonry, roofing, painting, etc. must be scheduled. Some tasks must
  * necessarily take place before others and these requirements are expressed through precedence constraints.
  *
  * A pool of two workers is available for building the houses. For a given house, some tasks (namely: plumbing,
  * ceiling and painting) require the house to be clean whereas other tasks (namely: masonry, carpentry, roofing and
  * windows) put the house in a dirty state. A transition time of 1 is needed to clean the house so to change from
  * state 'dirty' to state 'clean'.
  *
  * The objective is to minimize the makespan.
  */
object SchedState {

  val nbWorkers = 2
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

  val clean: Int = 0
  val dirty: Int = 1

  implicit var model: CpModel = _

  var allTaskVars: Vector[IntervalVar] = _
  var endVars: List[IntExpr] = _

  var workersUsage: CumulFunctionExpr  = _
  var houseStates: List[StateFunction] = _

  def makeHouse(id: Int, releaseDate: Int): (IntExpr, IntervalVarArray, StateFunction) = {

    val taskVars: Map[String, IntervalVar] = (for (task <- tasks; (tname, tduration) = task)
          yield (tname , model.intervalVar(sizeMin = tduration, sizeMax = tduration, name = "H" + id + "-" + tname)))(collection.breakOut)

    workersUsage += sum(for (e <- taskVars; (name, v) = e) yield pulse(v, 1))

    taskVars("masonry").setStartMin(releaseDate)

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

    // state constraints
    val ttime = model.transitionDistance(2)
    ttime.setValue(dirty, clean, 1)
    val houseState = model.stateFunction(ttime)

    model.add(alwaysEqual(houseState, taskVars("masonry"),   dirty))
    model.add(alwaysEqual(houseState, taskVars("carpentry"), dirty))
    model.add(alwaysEqual(houseState, taskVars("plumbing"),  clean))
    model.add(alwaysEqual(houseState, taskVars("ceiling"),   clean))
    model.add(alwaysEqual(houseState, taskVars("roofing"),   dirty))
    model.add(alwaysEqual(houseState, taskVars("painting"),  clean))
    model.add(alwaysEqual(houseState, taskVars("windows"),   dirty))

    (model.endOf(taskVars("moving")), taskVars.values, houseState)
  }

  def build(): CpModel = {

    model = CpModel("SchedCumul")

    workersUsage = model.cumulFunctionExpr()

    val results = List(
      makeHouse(0,  31),
      makeHouse(1,   0),
      makeHouse(2,  90),
      makeHouse(3, 120),
      makeHouse(4,  90))

    model.add(workersUsage <= nbWorkers)

    endVars = results.map(_._1)
    allTaskVars = results.flatMap(_._2).toVector
    houseStates = results.map(_._3)

    model.add(minimize(max(endVars)))

    model
  }

  def solve(): Boolean = {

    println(s"Solving model $model....")

//    model.exportModel("SchedState.cpo")

    model.cp.setParameter(IloCP.IntParam.FailLimit, 10000)

    //    val status = model.solve(timeLimit=20, logPeriod=3000)
    val status = model.solve()

    if (status) {
      println(s"Solution status: $status")
      println("Solution with objective " + model.getObjectiveValue())
      println("Tasks:")
      for (t <- allTaskVars) {
        println(model.getDomain(t))
      }
      println("Workers Usage:")
      for (s <- workersUsage) {
        System.out.println(
          "# Workers  is " + s.value +
            " in [" + s.start +
            " .. " + s.end +
            ")"
        )
      }
      println("House States:")
      for (h <- houseStates.indices) {
        val houseState = houseStates(h)
        for (s <- houseState) {
          print("House " + h + " has state ")
          val v = s.value
          if (v == clean)                 print("Clean")
          else if (v == dirty)            print("Dirty")
          else if (v == IloCP.NoState)    print("None")
          else                            print("Unknown (problem)")
          print(" from ")
          if (s.start == IloCP.IntervalMin)  print("Min")
          else print(s.start)
          System.out.print(" to ")
          if (s.end == IloCP.IntervalMax)    println("Max")
          else println(s.end - 1)
        }
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
