/*
 * Source file provided under Apache License, Version 2.0, January 2004,
 *  http://www.apache.org/licenses/
 *  (c) Copyright DecisionBrain SAS 2016,2020
 *
 */

package com.decisionbrain.cplex.mp

import com.decisionbrain.cplex.Modeler._

/**
 * The company Sailco must determine how many sailboats to produce over several time periods,
 * while satisfying demand and minimizing costs.
 * The demand for the periods is known and an inventory of boats is available initially.
 * In each period, Sailco can produce boats inside at a fixed cost per boat.
 * Additional boats can be produced outside at a higher cost per boat.
 * There is an inventory cost per boat per period.
 * The business objective is to minimize the overall cost, which is the sum of the
 * production cost and inventory cost.
 * The production cost is modeled using a *piecewise-linear* function.
 */
object SailcoPW {

  //
  // Data
  //

  val nbPeriods: Int = 4
  val demands: Map[Int, Int] = Map(
    1 -> 40,
    2 -> 60,
    3 -> 75,
    4 -> 25).withDefaultValue(0)

  val regularCost: Double = 400
  val capacity: Int = 40
  val extraCost: Double = 450

  val initialInventory: Int = 10
  val inventoryCost: Double = 20

  def buildModel(): MpModel = {

    implicit val model: MpModel = MpModel("sailcopw")

    val periods0 = 0 to nbPeriods + 1
    val periods1 = 1 to nbPeriods + 1 // full range from 0 to nb_periods

    // variables
    val boatVars = model.numVars(periods1, namer = (t: Int) => "boat %d".format(t))
    val inventoryVars = model.numVars(periods0, namer = (t: Int) => "inventory %d".format(t))

    // objectives

    // piecewise cost:
    // up to zero boat cost is zero.
    // up to capacity, each boat costs the regular cost
    // above capacity, unit cost is extra cost (higher than regular cost...)
    val productionCost = model.piecewiseLinear(preslope = .0, points=List((.0, .0), (capacity, capacity * regularCost)), postslope=extraCost)
    val totalProductionCost = sum(for (t <- periods1) yield productionCost(boatVars(t)))
    model.addKPI(totalProductionCost, "Total production cost")
    val totalInventoryCost = inventoryCost * sum(for (t <- periods1) yield inventoryVars(t))
    model.addKPI(totalInventoryCost, "Total inventory cost")

    model.add(minimize(totalProductionCost + totalInventoryCost))

    // constraints

    // initial inventory
    model.add(inventoryVars(0) == initialInventory)

    // balance
    for (t <- periods1) {
      model.add(boatVars(t) + inventoryVars(t - 1) == inventoryVars(t) + demands(t))
    }

    model
  }


  def solveModel(): Double = {

    val model = buildModel()

    // export model to LP file
    //    model.exportModel("sailcopw.lp")

    // print some info on the optim model e.g. number of rows, number of columns...
    model.printInformation()

    if (!model.solve()) {
      println("Problem has no solution")
      return -1
    }

    val obj = model.getObjValue()

    // release memory
    model.end()

    obj
  }

}
