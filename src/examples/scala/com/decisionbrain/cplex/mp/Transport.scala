package com.decisionbrain.cplex.mp

import com.decisionbrain.cplex.Modeler._
import com.decisionbrain.cplex.NumExpr

object Transport {

  // Data
  val supplies: List[Double] = List(1000.0, 850.0, 1250.0)
  val demands: List[Double] = List(900.0, 1200.0, 600.0, 400.0)
  val nbSupplies = supplies.size
  val nbDemands = demands.size


  implicit var model: MpModel = _
  var x: Array[Array[NumExpr]] = _
  var y: Array[Array[NumExpr]] = _

  private def printUsage(): Unit = {
    System.err.println("Usage: java Transport <type>")
    System.err.println("  type = 0 -> convex  piecewise linear model")
    System.err.println("  type = 1 -> concave piecewise linear model")
    System.exit(-1)
  }

  def apply(args: String*) : Option[MpModel] = {

    if (args.size < 1) {
      printUsage()
      return None
    }

    model = MpModel()

    x = Array.ofDim[NumExpr](supplies.size, demands.size)
    y = Array.ofDim[NumExpr](supplies.size, demands.size)

    for (s <- 0 until nbSupplies; d <- 0 until nbDemands) {
      x(s)(d) = model.numVar()
      y(s)(d) = model.numVar()
    }

    // supply must meet demand
    for (s <- 0 until nbSupplies)
      model.add(sum(x(s)) == supplies(s))

    // demand must meet supply
    for (d <- 0 until nbDemands)
      model.add(sum(for (s <- 0 until nbSupplies) yield x(s)(d)) == demands(d))

    val isConvex = (args(0).charAt(0) == '0')

    // piecewise linear transportation cost function
    val f = if (isConvex)
      model.piecewiseLinear(30, List((200, 200 * 30), (400, 200*30+(400-200)*80)), 130)
    else
      model.piecewiseLinear(120, List((200, 200*120), (400, 200*120+(400-200)*80)), 50)

    for (s <- 0 until nbSupplies; d <- 0 until nbDemands)
        model.add(y(s)(d) == f(x(s)(d)))

    model.add(minimize(sum(for(s <- 0 until nbSupplies; d <- 0 until nbDemands) yield y(s)(d))))

    Some(model)
  }

  def solve(): Boolean = {

//    model.exportModel("transport.lp")

    val status = model.solve()

    status
  }

  def printSolution(): Unit = {
    println("Solution status: " + model.getStatus)
    println(" - Solution: ")
    for (s <- 0 until nbSupplies) {
      print("   " + s + ": ")
      for (d <- 0 until nbDemands) {
        print("" + model.getValue(x(s)(d)) + "\t")
      }
      println()
    }
    println("   Cost = " + model.getObjValue())
  }

  def main(args: Array[String]): Unit = {

    val transport = Transport(args.toList: _*)
    if (transport.isEmpty) return

    val model = transport.get

    val status = solve()

    if (status)
      printSolution()

    model.end()
  }

}
