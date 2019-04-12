/*
 * Source file provided under Apache License, Version 2.0, January 2004,
 * http://www.apache.org/licenses/
 * (c) Copyright DecisionBrain SAS 2016,2018
 */

package com.decisionbrain.cplex.cp

import java.io.{BufferedReader, FileInputStream, InputStreamReader, StreamTokenizer}

import com.decisionbrain.cplex.{IntExpr, IntSet, IntVar}
import com.decisionbrain.cplex.cp.CpModel._
import ilog.cp.IloCP

/**
  * This example is inspired from the talent hold cost scheduling problem described in:
  *
  * T.C.E Cheng, J. Diamond, B.M.T. Lin.  Optimal scheduling in film production to minimize talent holding cost.
  * Journal of Optimization Theory and Applications, 79:197-206, 1993.
  *
  * of which the 'Rehearsal problem' is a specific case:
  *
  * Barbara M. Smith.  Constraint Programming In Practice: Scheduling a Rehearsal. Report APES-67-2003, September 2003.
  *
  * See: http://www.csplib.org/Problems/prob039/
  */
object Talent {

  var numActors: Int = _
  var numScenes: Int = _
  var actorPay: Array[Int] = _
  var sceneDuration: Array[Int] = _

  implicit var model: CpModel = _
  var actorInScene: Array[IntSet] = _
  var idleCost: IntExpr = _
  var scene: List[IntVar] = _

  private class DataReader(val filename: String) {

    val fstream = new FileInputStream(filename)
    val r = new BufferedReader(new InputStreamReader(fstream))
    val st = new StreamTokenizer(r)

    def next: Int = {
      st.nextToken
      st.nval.toInt
    }
  }

  def readData(fileName: String) = {
    val data = new DataReader(fileName)
    numActors = data.next
    actorPay = (for (a <- 0 until numActors)
      yield data.next).toArray

    numScenes = data.next
    sceneDuration = (for (s <- 0 until numScenes)
      yield data.next).toArray

    actorInScene =
    (for (a <- 0 until numActors) yield {
      val inScene = Array.fill[Int](numScenes)(0)
      var nbScene = 0
      for (s <- 0 until numScenes) {
        inScene(s) = data.next
        if (inScene(s) != 0) {
          nbScene += 1
        }
      }
//      println("inScene: " + inScene.mkString("<", ",", ">"))
      val playScene = Array.fill[Int](nbScene)(0)
      var n = 0
      for (s <- 0 until numScenes) {
        if (inScene(s) != 0) {
          playScene(n) = s
          n += 1
        }
      }
//      println("playScene: " + playScene.mkString("<", ",", ">"))
      model.intSet(playScene)
    }).toArray
  }

  def build(fileName: String): CpModel = {

    model = CpModel("Talent")
    readData(fileName)

    scene = model.intVars(numScenes, 0, numScenes -1)

    idleCost = 0

    val slot = model.intVars(numScenes, 0, numScenes -1)

    model.add(inverse(scene, slot))

    for (a <- 0 until numActors) {

      var actorWait: IntExpr = 0

      val position = for (s <- actorInScene(a))
        yield slot(s)

      val firstSlot = min(position)
      val lastSlot = max(position)

      for (s <- 0 until numScenes if !actorInScene(a).contains(s)) {
        val wait = (firstSlot <= slot(s)) && (slot(s) <= lastSlot)
        actorWait += sceneDuration(s) * wait
      }

      // Accumulate the cost of waiting time for this actor
      idleCost += actorPay(a) * actorWait
    }

    model.add(minimize(idleCost))

    model
  }

  var inputFile = "data/rehearsal.data"
  var tlim = 10.0

  def solve(): Boolean = {

    println(s"Solving model $model....")

    //    model.exportModel("Talent.cpo")

    model.cp.setParameter(IloCP.DoubleParam.TimeLimit, tlim)

    val status = model.solve()

    if (status) {
      println(s"Solution status: $status")
      println("Solution with objective " + model.getObjectiveValue())
      print("Order:")
      for (s <- 0 until numScenes) {
        System.out.print(" " + (model.getValue(scene(s)) + 1))
      }
      println()
      for (a <- 0 until numActors) {
        System.out.print("|")
        for (s <- 0 until numScenes) {
          val sc = model.getValue(scene(s))
          for (d <- 0 until sceneDuration(sc)) {
            if (actorInScene(a).contains(sc))
              print("X")
            else
              print(".")
          }
          print("|")
        }
        println("  Rate = " + actorPay(a) + ")")
      }
    }

    status
  }

  def run(): Boolean = {

    val model = build(inputFile)
    val status = solve()
    model.end()
    status
  }

  def main(args: Array[String]): Unit = {
    if (args.length > 1) inputFile = args(1)
    if (args.length > 2) tlim = args(2).toDouble
    run()
  }
}

