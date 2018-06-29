/*
 * Source file provided under Apache License, Version 2.0, January 2004,
 * http://www.apache.org/licenses/
 * (c) Copyright DecisionBrain SAS 2016,2018
 */

package com.decisionbrain.cplex.cp

import com.decisionbrain.cplex.cp.CpModel._

/**
  * The problem is to deliver some orders to several clients with a single truck. Each order consists of a given
  * quantity of a product of a certain type (called its color). The truck must be configured in order to handle one,
  * two or three different colors of products. The cost for configuring the truck from a configuration A to a
  * configuration B depends on A and B. The configuration of the truck determines its capacity and its loading cost.
  * A truck can only be loaded with orders for the same customer. Both the cost (for configuring and loading the truck)
  * and the number of travels needed to deliver all the orders must be minimized, the cost being the most important
  * criterion.

  */
object TruckFleet {

  val nbTruckConfigs = 7  // Number of possible configurations for the truck
  val nbOrders = 21
  val nbCustomers = 3
  val nbTrucks = 15  // Max. number of travels of the truck

  val maxTruckConfigLoad = List(11, 11, 11, 11, 10, 10, 10)
  val maxLoad = maxTruckConfigLoad.max

  val customerOfOrder = List(0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2)
  val volumes = List(3, 4, 3, 2, 5, 4, 11, 4, 5, 2, 4, 7, 3, 5, 2, 5, 6, 11, 1, 6, 3)
  val colors = List(1, 2, 0, 1, 1, 1, 0, 0, 0, 0, 2, 2, 2, 0, 2, 1, 0, 2, 0, 0, 0)

  // Cost for loading a truck of a given config
  val truckCost = List(2, 2, 2, 3, 3, 3, 4)

  // Transition costs between trucks e.g. from config 0 to config 3 the cost is 10
  val costTuples = List(
    List(0, 0, 0), List(0, 1, 0), List(0, 2, 0), List(0, 3, 10), List(0, 4, 10), List(0, 5, 10), List(0, 6, 15),
    List(1, 0, 0), List(1, 1, 0), List(1, 2, 0), List(1, 3, 10), List(1, 4, 10), List(1, 5, 10), List(1, 6, 15),
    List(2, 0, 0), List(2, 1, 0), List(2, 2, 0), List(2, 3, 10), List(2, 4, 10), List(2, 5, 10), List(2, 6, 15),
    List(3, 0, 3), List(3, 1, 3), List(3, 2, 3), List(3, 3,  0), List(3, 4, 10), List(3, 5, 10), List(3, 6, 15),
    List(4, 0, 3), List(4, 1, 3), List(4, 2, 3), List(4, 3, 10), List(4, 4,  0), List(4, 5, 10), List(4, 6, 15),
    List(5, 0, 3), List(5, 1, 3), List(5, 2, 3), List(5, 3, 10), List(5, 4, 10), List(5, 5,  0), List(5, 6, 15),
    List(6, 0, 3), List(6, 1, 3), List(6, 2, 3), List(6, 3, 10), List(6, 4, 10), List(6, 5, 10), List(6, 6,  0)
  )

  implicit var model: CpModel = _

  var truckConfigs: List[IntVar] = _
  // In which truck is an order
  var where: List[IntVar] = _
  // Load of a truck
  var load: List[IntVar] = _

  // Number of trucks used
  var numUsed: IntVar = _

  var obj1: IntExpr = _
  var obj2: IntExpr = _

  def build(): CpModel = {

    model = CpModel("TruckFleet")

    truckConfigs = model.intVars(nbTrucks, 0, nbTruckConfigs - 1, (t) => "configOfTruck(" + t + ")")
    // In which truck is an order
    where = model.intVars(nbOrders, 0, nbTrucks - 1, (t) => "truckOfOrder(" + t +")")
    //Load of a truck
    load = model.intVars(nbTrucks, 0, maxLoad, (t) => "loadOfTruck(" + t +")")

    // Number of trucks used
    numUsed = model.intVar(0, nbTrucks)
    //
    val customerOfTruck: List[IntVar] = model.intVars(nbTrucks, 0, nbCustomers, (t) => "customerOfTruck(" + t +")")
    //
    val transitionCost: List[IntVar] = model.intVars(nbTrucks - 1, 0, 1000, (t) => "costOfTruck(" + t +")")

    for (t <- 1 until nbTrucks) {
      val vars = Array(truckConfigs(t - 1), truckConfigs(t), transitionCost(t - 1))
      val values = costTuples.map(v => v.toArray)
      model.add(allowedAssignments(vars, values))
    }

    // Constrain the volume of the orders in each truck
    model.add(pack(load, where, volumes, numUsed))
    for (t <- 0 until nbTrucks) {
      model.add(load(t) <= element(maxTruckConfigLoad, truckConfigs(t)))
    }

    // Compatibility between the colors of an order and the configuration of its truck
    val allowedContainerConfigs = List(
      List(0, 3, 4, 6),
      List(1, 3, 5, 6),
      List(2, 4, 5, 6)
    )
    for (o <- 0 until nbOrders) {
      val values = allowedContainerConfigs(colors(o))
      val configOfContainer = model.intVar(values)
      model.add(configOfContainer == truckConfigs.element(where(o)))
    }

    // Only one customer per truck
    for (o <- 0 until nbOrders)
      model.add(customerOfTruck.element(where(o)) == customerOfOrder(o))

    // Non-used trucks are at the end
    for (j <- 1 until nbTrucks) {
      model.add((load(j - 1) > 0) || (load(j) == 0))
    }

    // Dominance: the non used trucks keep the last used configuration
    model.add(load(0) > 0)
    for (i <- 1 until nbTrucks) {
      model.add((load(i) > 0) || (truckConfigs(i) == truckConfigs(i - 1)))
    }

    // Dominance: regroup deliveries with same configuration
    for (i <- nbTrucks - 2 until 0 by -1) {
      var ct: Constraint = Constraint.TRUE // equivalent to 'model.constraint(true)'
      for (p <- i + 1 until nbTrucks) {
        ct = (truckConfigs(p) != truckConfigs(i - 1)) && ct
        model.add((truckConfigs(i) == truckConfigs(i - 1)) || ct)
      }
    }

    // Objective: first criterion for minimizing the cost for configuring and loading trucks
    //            second criterion for minimizing the number of trucks
    obj1 = IntExpr(0)
    for (i <- 0 until nbTrucks) {
      obj1 = obj1 + element(truckCost, truckConfigs(i)) * (load(i) != 0)
    }
    obj1 = obj1 + sum(transitionCost.toArray[IntExpr])

    obj2 = numUsed

    // Search strategy: first assign order to truck
    model.setSearchPhases(searchPhase(where))

    // Multicriteria lexicographic optimization
    model.add(minimize(staticLex(obj1, obj2)))

    model
  }

  def solve(): Boolean = {

    println(s"Solving model $model....")

//    model.exportModel("TruckFleet.cpo")

    val status = model.solve(timeLimit=20, logPeriod=3000)

    if (status) {
      println(s"Solution status: $status")
      println("Objective value: " + model.getObjectiveValue())
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
