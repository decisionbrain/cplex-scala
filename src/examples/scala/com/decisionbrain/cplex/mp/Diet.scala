/*
 * Source file provided under Apache License, Version 2.0, January 2004,
 * http://www.apache.org/licenses/
 * (c) Copyright DecisionBrain SAS 2016,2018
 */

package com.decisionbrain.cplex.mp

object Diet {

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

  var qtyFoods : Map[Food, NumVar] = _
  var qtyNutrients: Map[Nutrient, NumExpr] = _

  def buildDietModel() = {

    theFoods = for ((name, unitCost, qmin, qmax) <- foods) yield new Food(name, unitCost, qmin, qmax)
    theNutrients = for ((name, qmin, qmax) <- nutrients) yield new Nutrient(name, qmin, qmax)

    theFoodNutrients =
      (for (t <- foodNutients; n <- nutrients.indices)
        yield (t._1, nutrients(n)._1) -> t.productElement(1 + n).asInstanceOf[Double])(collection.breakOut).toMap


    val model = new MpModel("diet")

    // create the numeric variables for the foods
    qtyFoods = model.numVars(theFoods)

    // initialize each variable with a name, a lower bound and an upper bound
    qtyFoods.foreach{ case (f, v) => v.setName(f.name); v.setLB(f.qMin); v.setUB(f.qMax) }

    // limit range of nutrients, and mark them as KPIs
    qtyNutrients = Map()
    for (n <- theNutrients) {
      val amount = model.sum(for (f <- theFoods) yield qtyFoods(f)* theFoodNutrients(f.name, n.name))
      qtyNutrients += (n -> amount)
      model.addRange(n.qmin, amount, n.qmax)
//      model.addKpi(amount, publish_name=s"Total $n.name") // TODO: to have same API as in DOCplex
    }

    val objective = model.minimize(model.sum(for (f <- theFoods) yield qtyFoods(f) * f.unitCost))

    model.add(objective)

    model.printInformation()

    // return the model
    model
  }


  def main(args: Array[String]): Unit = {

    val model = buildDietModel()

    val result = model.solve()

    if (!result) {
      println("*** Problem has no solution!")
      return
    }

    val obj = model.getObjectiveValue()
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

    model.end()
  }
}