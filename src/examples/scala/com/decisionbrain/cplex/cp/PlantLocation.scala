/*
 *  Source file provided under Apache License, Version 2.0, January 2004,
 *  http://www.apache.org/licenses/
 *  (c) Copyright DecisionBrain SAS 2016,2019
 */

package com.decisionbrain.cplex.cp

import java.io.{BufferedReader, FileInputStream, InputStreamReader, StreamTokenizer}

import com.decisionbrain.cplex._
import com.decisionbrain.cplex.Modeler._
import com.decisionbrain.cplex.cp.CpModel.{Infinity, IntMax, IntMin}
import com.decisionbrain.cplex.cp.LearningCurve.model
import ilog.cp.IloCP


/**
  * Problem Description
  * -------------------
  *
  * A ship-building company has a certain number of customers. Each customer is supplied
  * by exactly one plant. In turn, a plant can supply several customers. The problem is
  * to decide where to set up the plants in order to supply every customer while minimizing
  * the cost of building each plant and the transportation cost of supplying the customers.
  *
  * For each possible plant location there is a fixed cost and a production capacity.
  * Both take into account the country and the geographical conditions.
  *
  * For every customer, there is a demand and a transportation cost with respect to
  * each plant location.
  *
  * While a first solution of this problem can be found easily by CP Optimizer, it can take
  * quite some time to improve it to a very good one. We illustrate the warm start capabilities
  * of CP Optimizer by giving a good starting point solution that CP Optimizer will try to improve.
  * This solution could be one from an expert or the result of another optimization engine
  * applied to the problem.
  *
  * In the solution we only give a value to the variables that determine which plant delivers
  * a customer. This is sufficient to define a complete solution on all model variables.
  * CP Optimizer first extends the solution to all variables and then starts to improve it.
  *
  * The model has been further enriched by the addition of KPIs (key performance
  * indicators).  These are named expressions which are of interest to help you get
  * an idea of the performance of the model.  Here, there are two indicators of
  * interest, both of which indicate in different ways how efficiently the
  * plant capacity is being used.  The first KPI is the `mean occupancy'' defined
  * as the total demand divided by the total capacity of the used plants.  The second
  * indicator is the minimum plant occupancy defined as the ratio of demand to capacity
  * of the plant where this ratio is the smallest.  The KPIs are displayed in the log whenever
  * an improved solution is found and at the end of the search.
  *
  **/
object PlantLocation {


  var nbCustomers: Int = _
  var nbLocations: Int = _

  var costs: Vector[Vector[Int]] = _
  var demands: Vector[Int] = _
  var fixedCosts: Vector[Int] = _
  var capacities: Vector[Int] = _

  var cust: List[IntVar] = _
  var open: List[IntVar] = _
  var load: List[IntVar] = _

  implicit var model: CpModel = _


  /**
    * Data reader
    *
    * @param filename
    */
  private class DataReader(val filename: String) {

    val filestream = new FileInputStream(filename)
    val r = new BufferedReader(new InputStreamReader(filestream))
    val st = new StreamTokenizer(r)

    def next: Int = {
      st.nextToken
      st.nval.toInt
    }
  }

  def readData(fileName: String) = {
    val data = new DataReader(fileName)
    nbCustomers = data.next
    nbLocations = data.next

    costs = (for (c <- 0 until nbCustomers) yield
      (for (w <- 0 until nbLocations) yield data.next).toVector).toVector

    demands = (for (c <- 0 until nbCustomers) yield data.next).toVector

    println(s"Costs: $costs")
    println(s"Demands: $demands")

    val totalDemand = demands.sum
    println(s"Total demand: $totalDemand")

    fixedCosts = (for (w <- 0 until nbLocations) yield data.next).toVector
    capacities = (for (w <- 0 until nbLocations) yield data.next).toVector

  }

  var inputFile = "data/plant_location.data"
  var tlim = 10.0
  var logPeriod = 10000

  def build(fileName: String): CpModel = {

    model = CpModel("PlantLocation")
    readData(fileName)

    cust = model.intVars(nbCustomers, 0, nbLocations-1)
    open = model.intVars(nbLocations, 0, 1)
    load = (for (w <- 0 until nbLocations) yield model.intVar(0, capacities(w))).toList

    for (w <- 0 until nbLocations)
      model.add(open(w) == (load(w) > 0))

    model.add(model.pack(load, cust, demands))

    val obj1 = fixedCosts * open

    val obj2 = model.sum(for(c <- 0 until nbCustomers) yield costs(c)(cust(c)))

    model.add(minimize(obj1 + obj2))

    // add KPIs for mean, min and max occupancy

    model.addKPI(demands.sum.toDouble / (open*capacities), name="Mean occupancy")

    val usages = for (w <- 0 until nbLocations) yield load(w) / capacities(w).toDouble

    // minimum occupancy among the locations open: consider 100% occupancy for locations closed
    model.addKPI(min(for (w <- 0 until nbLocations) yield usages(w) + (1 - open(w))), name="Min occupancy")
    // maximum occupancy among the locations open
    model.addKPI(max(usages), name="Max occupancy")

    // set starting point solution

    val custValues = Vector(
      19, 0, 11, 8, 29, 9, 29, 28, 17, 15, 7, 9, 18, 15, 1, 17, 25, 18, 17, 27,
      22, 1, 26, 3, 22, 2, 20, 27, 2, 16, 1, 16, 12, 28, 19, 2, 20, 14, 13, 27,
      3, 9, 18, 0, 13, 19, 27, 14, 12, 1, 15, 14, 17, 0, 7, 12, 11, 0, 25, 16,
      22, 13, 16, 8, 18, 27, 19, 23, 26, 13, 11, 11, 19, 22, 28, 26, 23, 3, 18, 23,
      26, 14, 29, 18, 9, 7, 12, 27, 8, 20
    )

    val sol = model.solution()
    for (c <- 0 until nbCustomers) sol.setValue(cust(c).getIloIntVar(), custValues(c))

    model.setStartingPoint(sol)

    model
  }

  def solve(timeLimit: Double = Infinity,
            failLimit : Int = 0,
            solutionLimit: Int = IntMax,
            logPeriod: Int = IntMin): Boolean = {
    val status = model.solve(timeLimit, failLimit, solutionLimit, logPeriod)

    if (status) {
      println(s"Solution status: $status")
      println("Solution with objective " + model.getObjectiveValue())

      for (w <- 0 until nbLocations) {
        print(s"Location $w: ")
        print("open: ")
        print(model.getValue(open(w)))
        print("; occupancy: ")
        val usage = model.getValue(load(w)).toDouble / capacities(w)
        print(usage)
        System.out.println()
      }
    }

    status
  }

  def run(): Boolean = {

    val model = build(inputFile)
    val status = solve(timeLimit=tlim, logPeriod=logPeriod)
    model.end()
    status
  }

  def main(args: Array[String]): Unit = {
    if (args.length > 1) inputFile = args(1)
    if (args.length > 2) tlim = args(2).toDouble
    run()
  }
}
