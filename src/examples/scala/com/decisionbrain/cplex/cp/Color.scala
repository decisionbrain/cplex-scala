/*
 * Source file provided under Apache License, Version 2.0, January 2004,
 *  http://www.apache.org/licenses/
 *  (c) Copyright DecisionBrain SAS 2016,2020
 *
 */

package com.decisionbrain.cplex.cp

import com.decisionbrain.cplex.IntVar

/**
  * The problem involves choosing colors for the countries on a map in such a way that at most four colors (blue, white,
  * yellow, green) are used and no neighboring countries are the same color. In this exercise, six countries are
  * considered for the map coloring problem: Belgium, Denmark, France, Germany, Luxembourg, and the Netherlands.
  */
object Color {

  var model: CpModel = _

  var colors: Map[String, IntVar] = _

  val countries = List("Belgium", "Denmark", "France", "Germany", "Luxembourg", "Netherlands")

  def build(): CpModel = {

    model = CpModel("Color")

    colors = model.intVars(countries, 0, 3, namer= (c: String) => c)

    model.add(colors("Belgium") != colors("France"))
    model.add(colors("Belgium") != colors("Germany"))
    model.add(colors("Belgium") != colors("Netherlands"))
    model.add(colors("Belgium") != colors("Luxembourg"))
    model.add(colors("Denmark") != colors("Germany"))
    model.add(colors("France") != colors("Germany"))
    model.add(colors("France") != colors("Luxembourg"))
    model.add(colors("Germany") != colors("Luxembourg"))
    model.add(colors("Germany") != colors("Netherlands"))

    model
  }

  def solve(): Boolean = {

    println(s"Solving model $model....")

//    model.exportModel("Color.cpo")

    val status = model.solve()

    val allColors = List("Yellow", "Red", "Green", "Blue")

    if (status) {
      println(s"Solution status: $status")
      for (country <- countries) {
        println("\t" + country + ": " + allColors(model.getValue(colors(country))))
      }
    }

    status
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
