/*
 * Source file provided under Apache License, Version 2.0, January 2004,
 * http://www.apache.org/licenses/
 * (c) Copyright DecisionBrain SAS 2016,2018
 */

package com.decisionbrain.cplex.mp

import com.decisionbrain.cplex.mp.MpModel._
import com.decisionbrain.cplex.Modeler._
import com.decisionbrain.cplex._

object Production {

  /**
    * This is a "direct" translation in Scala of the DOCplex example production.py.
    *
    * Notice the interesting things:
    * <ul>
    *   <li>Implicit conversion of IloNumExpr to NumExpr</li>
    *   <li>Operator overloading for NumExpr : </li>
    *   <li>Implicit argument of tyep IloCplex to operators and class CplexModel
    *   <li>Use of tuples, sets, arrays, dictionaries...</li>
    *   <li>Use of generator comprehension</li>
    *   <li>There is no slicing in this example, but that could be easily added</li>
    * </ul>
    *
    */

  // Products: list of tuples (product, demand, insideCost, outsideCost)
  val products = List(
    ("kluski", 100, 0.6, 0.8), // product, demand, insideCost, outsideCost
    ("capellini", 200, 0.8, 0.9),
    ("fettucine", 300, 0.3, 0.4))

  // Resources: list of tuples (resource, capacity)
  val resources = List(
    ("flour", 20), // resource, capacity
    ("eggs", 40))

  // Consumptions: a dictionary where the key is a tuple (product, resource) and the value the capacity
  // required to produce one unit of the product
  val consumptions = Map(
    ("kluski", "flour") -> 0.5,
    ("kluski", "eggs") -> 0.2,
    ("capellini", "flour") -> 0.4,
    ("capellini", "eggs")  -> 0.4,
    ("fettucine", "flour") -> 0.3,
    ("fettucine", "eggs")  -> 0.6
  )

  var insideVars: Map[String, NumVar] = _
  var outsideVars: Map[String, NumVar] = _

  var totalInsideCost : NumExpr = _
  var totalOutsideCost : NumExpr = _


  /**
    * Build the MIP model.
    *
    * @return
    */
  def buildProductionProblem() = {

    implicit val model = new MpModel("production")

    //--- decision variables ---
    /*
        // In python:
        mdl.inside_vars = mdl.continuous_var_dict(products, name='inside')
        mdl.outside_vars = mdl.continuous_var_dict(products, name='outside')
    */
    insideVars = model.numVars(products.map(p => p._1))
    outsideVars = model.numVars(products.map(p => p._1))

    // --- constraints ---
    // demand satisfaction
    for ((product, demand, _, _) <- products) {
      model.add(insideVars(product) + outsideVars(product) >=  demand)
    }

    // --- resource capacity ---
    for ((resource, capacity) <- resources) {
      // In python:
      //      mdl.add(mdl.sum([mdl.inside_vars[p] * consumptions[p[ 0], res[ 0]] for p in products] ) <= res[ 1] )
      // In scala, the generator comprehension start with a for loop and the expression is after the keyword yield
      model.add(sum(for ((product, _, _, _) <- products)
        yield insideVars(product) * consumptions((product, resource))) <= capacity)
    }

    // --- objective ---
    // In python:
    //    mdl.total_inside_cost = mdl.sum(mdl.inside_vars[p] * p[2] for p in products)
    //    mdl.total_outside_cost = mdl.sum(mdl.outside_vars[p] * p[3] for p in products)
    //    mdl.minimize(mdl.total_inside_cost + mdl.total_outside_cost)
    // In Scala using a generator
    totalInsideCost = sum(for ((product, _, insideCost, _) <- products) yield insideVars(product) * insideCost)
    println(totalInsideCost)
    totalOutsideCost = sum(for ((product, _, _, outsideCost) <- products) yield outsideVars(product) * outsideCost)
    println(totalOutsideCost)
    model.add(minimize(totalInsideCost + totalOutsideCost))

    // return the model
    model
  }


  /**
    * This method build the optimization model and solve it using CPLEX.
    *
    */
  def solveProductionProblem() : Double = {

    val model= buildProductionProblem()

    // export model to LP file
//    model.exportModel("production.lp")

    // print some info on the optim model e.g. number of rows, number of columns...
    model.printInformation()

    if (!model.solve()) {
      println("Problem has no solution")
      return -1
    }

    // print the solution: objective, criteria, inside production, outside production
    val obj = model.getObjectiveValue()
    val totalInsideCostValue = model.getValue(totalInsideCost)
    val totalOutsideCostValue = model.getValue(totalOutsideCost)

    println(s"* Production model solved with objective: $obj")
    println(s"* Total inside cost: $totalInsideCostValue")
    for ((product, _, _, _) <- products) {
      val insideValue = model.getValue(insideVars(product))
      println(s"Inside production of $product : $insideValue")
    }
    println(s"* Total outside cost: $totalOutsideCostValue")
    for ((product, _, _, _) <- products) {
      val outsideValue = model.getValue(outsideVars(product))
      println(s"Outside production of $product : $outsideValue")
    }

    // release memory
    model.end()

    obj
  }

  def main(args: Array[String]): Unit = {


    println("products:")
    for (p <- products)
      println(p.toString())
    println("resources:")
    for (r <- resources)
      println(r.toString())
    println("consumptions:")
    for (c <- consumptions)
      println(c)

    solveProductionProblem()
  }
}
