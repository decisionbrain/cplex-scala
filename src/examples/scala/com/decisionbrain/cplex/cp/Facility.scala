/*
 * Source file provided under Apache License, Version 2.0, January 2004,
 * http://www.apache.org/licenses/
 * (c) Copyright DecisionBrain SAS 2016,2017
 */

package com.decisionbrain.cplex.cp

import com.decisionbrain.cplex.cp.CpModel._

/**
  * Created by dgodard on 11/02/2017.
  */
object Facility {

  val nbLocations = 5
  val nbStores = 8
  val capacity = List(3, 1, 2, 4, 1)
  val fixedCost = List(480, 200, 320, 340, 300)
  val cost = List(
    List(24, 74, 31, 51, 84),
    List(57, 54, 86, 61, 68),
    List(57, 67, 29, 91, 71),
    List(54, 54, 65, 82, 94),
    List(98, 81, 16, 61, 27),
    List(13, 92, 34, 94, 87),
    List(54, 72, 41, 12, 78),
    List(54, 64, 65, 89, 89))

  implicit var model: CpModel = _
  var suppliers: List[IntVar] = _
  var open: List[IntVar] = _

  def build(): CpModel = {

    model = CpModel("Facility")

    suppliers = (for (s <- 0 until nbStores)
      yield model.intVar(0, nbLocations - 1, "supplier")).toList
    open = (for (l <- 0 until nbLocations)
      yield model.intVar(0, 1, "open")).toList

    for (supplier <- suppliers) {
//      model.add(element(open.toArray[IntExpr], supplier) == 1)
      model.add(open.element(supplier) == 1) // equivalent to previous line
    }

    for (j <- 0 until nbLocations)
      model.add(count(suppliers, j) <= capacity(j))

    val fixedCostExpr: NumExpr = scalarProduct(fixedCost, open)
    val variableCostExpr = sum(for (s <- 0 until nbStores)
      yield cost(s).element(suppliers(s))) // element expression 'element(costs(s), suppliers(s))'

    model.add(minimize(fixedCostExpr + variableCostExpr))

    model
  }

  def solve(): Boolean = {

    println(s"Solving model $model....")

//    model.exportModel("Facility.cpo")

    val status = model.solve()

    if (status) {
      println(s"Solution status: $status")
      println("Objective value: " + model.getObjectiveValue())
      println("Locations: ")
      for (l <- 0 until nbLocations)
        println(s"\tLocation $l: " + model.getValue(open(l)))
      println("Suppliers: ")
      for (s <- 0 until nbStores)
        println(s"\tSupplier of store $s: location " + model.getValue(suppliers(s)))
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
