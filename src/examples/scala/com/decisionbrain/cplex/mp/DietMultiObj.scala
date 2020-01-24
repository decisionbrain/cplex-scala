/*
 * Source file provided under Apache License, Version 2.0, January 2004,
 *  http://www.apache.org/licenses/
 *  (c) Copyright DecisionBrain SAS 2016,2020
 *
 */

package com.decisionbrain.cplex.mp

import com.decisionbrain.cplex.Modeler._
import com.decisionbrain.cplex.mp.MpModel._
import com.decisionbrain.cplex.NumExpr
import com.decisionbrain.cplex.NumVar
import com.decisionbrain.cplex.Objective
import ilog.concert.IloException
import ilog.cplex.IloCplex
import ilog.cplex.IloCplex.{MultiObjIntInfo, MultiObjLongInfo, MultiObjNumInfo, Param}

object DietMultiObj {

  //
  // Data
  //
  val foods = List(
    ("Roasted Chicken", 0.84, 0, 10), // name, unit cost, qmin, qmax
    ("Spaghetti W/ Sauce", 0.78, 0, 10),
    ("Tomato,Red,Ripe,Raw", 0.27, 0, 10),
    ("Apple,Raw,W/Skin", .24, 0, 10),
    ("Grapes", 0.32, 0, 10),
    ("Chocolate Chip Cookies", 0.03, 0, 10),
    ("Lowfat Milk", 0.23, 0, 10),
    ("Raisin Brn", 0.34, 0, 10),
    ("Hotdog", 0.31, 0, 10)
  )

  val nutrients = List(
    ("Calories", 2000, 2500), // name, qmin, qmax
    ("Calcium", 800, 1600),
    ("Iron", 10, 30),
    ("Vit_A", 5000, 50000),
    ("Dietary_Fiber", 25, 100),
    ("Carbohydrates", 0, 300),
    ("Protein", 50, 100)
  )

  val foodNutients = List(
    ("Roasted Chicken", 277.4, 21.9, 1.8, 77.4, 0.0, 0.0, 42.2),
    ("Spaghetti W/ Sauce", 358.2, 80.2, 2.3, 3055.2, 11.6, 58.3, 8.2),
    ("Tomato,Red,Ripe,Raw", 25.8, 6.2, 0.6, 766.3, 1.4, 5.7, 1.0),
    ("Apple,Raw,W/Skin", 81.4, 9.7, 0.2, 73.1, 3.7, 21.0, 0.3),
    ("Grapes", 15.1, 3.4, 0.1, 24.0, 0.2, 4.1, 0.2),
    ("Chocolate Chip Cookies", 78.1, 6.2, 0.4, 101.8, 0.0, 9.3, 0.9),
    ("Lowfat Milk", 121.2, 296.7, 0.1, 500.2, 0.0, 11.7, 8.1),
    ("Raisin Brn", 115.1, 12.9, 16.8, 1250.2, 4.0, 27.9, 4.0),
    ("Hotdog", 242.1, 23.5, 2.3, 0.0, 0.0, 18.0, 10.4)
  )

  //
  // Types
  //
  class Food(val name: String, val unitCost: Double, val qMin: Double, val qMax: Double)

  class Nutrient(val name: String, val qmin: Double, val qmax: Double)

  var theFoods : List[Food]= _
  var theNutrients : List[Nutrient] = _
  var theFoodNutrients : Map[(String, String), Double] = _

  //
  // Variables
  //

  implicit var model: MpModel = _

  var qtyFoods : Map[Food, NumVar] = _
  var qtyNutrients: Map[Nutrient, NumExpr] = _
  var used : Map[Food, NumVar] = _

  var cost: Objective = _
  var variety: Objective = _

  def buildDietModel() = {

    theFoods = for ((name, unitCost, qmin, qmax) <- foods) yield new Food(name, unitCost, qmin, qmax)
    theNutrients = for ((name, qmin, qmax) <- nutrients) yield new Nutrient(name, qmin, qmax)

    theFoodNutrients =
      (for (t <- foodNutients; n <- nutrients.indices)
        yield (t._1, nutrients(n)._1) -> t.productElement(1 + n).asInstanceOf[Double]).toMap


    model = new MpModel("diet")

    // create the numeric variables for the foods
    qtyFoods = model.numVars(theFoods)

    // initialize each variable with a name, a lower bound and an upper bound
    qtyFoods.foreach{ case (f, v) => v.setName(f.name); v.setLB(f.qMin); v.setUB(f.qMax) }

    // limit range of nutrients, and mark them as KPIs
    qtyNutrients = Map()
    for (n <- theNutrients) {
      val amount = sum(for (f <- theFoods) yield qtyFoods(f)* theFoodNutrients(f.name, n.name))
      qtyNutrients += (n -> amount)
      model.addRange(n.qmin, amount, n.qmax)
//      model.addKpi(amount, publish_name=s"Total $n.name") // TODO: to have same API as in DOCplex
    }

    // first objective is to minimize total cost
    cost = minimize(sum(for (f <- theFoods) yield qtyFoods(f) * f.unitCost))
    cost.setName("cost")


    // second objective to maximize variety
    used = model.boolVars(theFoods)

    for (f <- theFoods) {
      model.add((used(f) == 0) <= (qtyFoods(f) == 0))
      model.add((qtyFoods(f) == 0) <= (used(f) == 0))
    }

    variety = maximize(sum(for (f <- theFoods) yield used(f)))
    variety.setName("number of foods")

    val objectives = Array(cost.getNumExpr(), variety.getNumExpr())

    // Use a negative weight to maximize the second objective. The
    // objective senses of all objective functions must match. Hence, we
    // have to formulate max(sum) as min(-sum).

    val weights = Array(1.0, -1.0)
    val priorities = Array(2, 1)

    val absTols = Array(0.5, 0.0)
    val relTols = Array(0.0, 0.0)

    val objectiveExpr = staticLex(objectives, weights, priorities, absTols, relTols, "staticLex")

    val objective = model.minimize(objectiveExpr)

    model.add(objective)

    model.printInformation()

    // return the model
    model
  }

  def solve(): Boolean = {

    //    model.exportModel("Diet.java.lp")

    // Solve model

    // Set multi-objective display level to "detailed".
    model.setParam(IloCplex.Param.MultiObjective.Display, 2)

    // Purely for demonstrative purposes, set global and local limits
    // using parameter sets.

    // First, set the global deterministic time limit.
    model.setParam(IloCplex.Param.DetTimeLimit, 60000)

    // Second, create a parameter set for each priority.
    var params = Array(new ParameterSet(), new ParameterSet())

    // Set the local deterministic time limits. Optimization will stop
    // whenever either the global or local limit is exceeded.
    params(0).setParam(Param.DetTimeLimit, 50000)
    params(1).setParam(Param.DetTimeLimit, 25000)

    // Optimize the multi-objective problem and apply the parameter
    // sets that were created above. The parameter sets are used
    // one-by-one by each optimization.
    val status = model.solve(params)
    if (!status) {
      println("*** Problem has no solution!")
      return false
    }

    System.out.println()
    System.out.println("Solution status = " + model.getStatus())

    // Print the objective value(s).

    val costName = cost.getName
    val costValue = model.getValue(cost.getNumExpr())
    val varietyName = variety.getName
    val varietyValue = model.getValue(variety.getNumExpr())
    println(s"Solution value 0 ($costName) = $costValue")
    println(s"Solution value 1 ($varietyName) = $varietyValue")
    println()

    printSolutionStats()

    printSolution()

    status
  }

  def printSolutionStat(what: String, value: String): Unit = {
    if (value != null)
      printf(s"    $what: $value%n")
    else
      printf(s"    $what: ---%n")
  }


  def printMultiObjInfo(what: IloCplex.MultiObjIntInfo, subprob: Int): Unit = {
    try {
      val value = model.getMultiObjInfo(what, subprob)
      printSolutionStat(what.toString, value.toString)
    }
    catch {
      case e: IloException => printSolutionStat(what.toString, null)
    }
  }

  def printMultiObjInfo(what: IloCplex.MultiObjLongInfo, subprob: Int): Unit = {
    try {
      val value: Long = model.getMultiObjInfo(what, subprob)
      printSolutionStat(what.toString, value.toString)
    }
    catch {
      case e: IloException => printSolutionStat(what.toString, null)
    }
  }

  def printMultiObjInfo(what: IloCplex.MultiObjNumInfo, subprob: Int): Unit = {
    try {
      val value: Double = model.getMultiObjInfo(what, subprob)
      printSolutionStat(what.toString, value.toString)
    }
    catch {
      case e: IloException => printSolutionStat(what.toString, null)
    }
  }

  def printSolutionStats(): Unit = {
    val num = model.getMultiObjNsolves()

    for (i <- 0 until num) {
      val prio = model.getMultiObjInfo(MultiObjIntInfo.MultiObjPriority, i)
      println(s"subproblem $i [priority $prio]:")
      printMultiObjInfo(MultiObjIntInfo.MultiObjError, i)
      printMultiObjInfo(MultiObjIntInfo.MultiObjStatus, i)
      printMultiObjInfo(MultiObjNumInfo.MultiObjTime, i)
      printMultiObjInfo(MultiObjNumInfo.MultiObjDetTime, i)
      printMultiObjInfo(MultiObjLongInfo.MultiObjNiterations, i)
      printMultiObjInfo(MultiObjNumInfo.MultiObjObjValue, i)
      printMultiObjInfo(MultiObjNumInfo.MultiObjBestObjValue, i)
      printMultiObjInfo(MultiObjLongInfo.MultiObjNnodes, i)
      printMultiObjInfo(MultiObjLongInfo.MultiObjNnodesLeft, i)
    }
  }

  def printSolution(): Unit = {
    val obj = model.getObjValue()
    println("Solution found!")
    println("Total cost: " + obj)
    println("Foods:")
    for (f <- theFoods) {
      val qty = model.getValue(qtyFoods(f))
      val name = f.name
      println(s"\tFood $name: $qty")
    }
    println("Nutrients:")
    for (n <- theNutrients) {
      val amount = model.getValue(qtyNutrients(n))
      val name = n.name
      println(s"\tNutrient $name: $amount")
    }
  }

  def main(args: Array[String]): Unit = {

    val model = buildDietModel()

    val result = model.solve()

    if (!result) {
      println("*** Problem has no solution!")
      return
    }

    model.end()
  }
}