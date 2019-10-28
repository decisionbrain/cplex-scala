/*
 *  Source file provided under Apache License, Version 2.0, January 2004,
 *  http://www.apache.org/licenses/
 *  (c) Copyright DecisionBrain SAS 2016,2019
 */

package com.decisionbrain.cplex.mp

import ilog.concert.{IloLPMatrix, IloNumVar, IloNumVarType}
import ilog.cplex.IloCplex

object Benders {

  private def usage(): Unit = {
    println("Usage:  Benders filename [annofile]")
    println("   where filename is a file with extension ")
    println("      MPS, SAV, or LP (lower case is allowed)")
    println("   and annofile is an optional .ann file with model annotations")
    println("      If \"create\" is used, the annotation is computed.")
    println(" Exiting...")
  }

  def apply(args: String*) : Option[MpModel] = {

    var hasAnnoFile = false

    // Check the arguments.
    val argsLength = args.length
    if (argsLength == 2) hasAnnoFile = true
    else if (argsLength != 1) {
      usage()
      return None
    }

    implicit val model = MpModel()

    // Read the problem file.
    model.importModel(args(0))

    if (hasAnnoFile) {
      // Generate default annotations if annofile is "create".
      if (args(1) == "create") {
        val benders = model.newLongAnnotation(IloCplex.CPX_BENDERS_ANNOTATION, IloCplex.CPX_BENDERS_MASTERVALUE)
        for (v <- model.getNumVars()) {
          if (v.getType eq IloNumVarType.Float) model.setAnnotation(benders, v, IloCplex.CPX_BENDERS_MASTERVALUE + 1)
        }
      }
      else { // Otherwise, read the annotation file.
        model.readAnnotations(args(1))
      }
    }
    else {
      // Set benders strategy to auto-generate a decomposition.
      model.setParam(IloCplex.Param.Benders.Strategy, IloCplex.BendersStrategy.Full)
      // Write out the auto-generated annotation.
      model.writeBendersAnnotation("benders.ann")
    }

    Some(model)
  }

  /**
   *
   * @param args
   */
  def main(args: Array[String]): Unit = {

    val benders  = Benders(args: _*)

    if (benders.isEmpty) return

    implicit val model: MpModel = benders.get

    // Solve the problem using Benders' decomposition.
    if (!model.solve()) throw new RuntimeException("Failed to optimize.")

    val status = model.getStatus
    val bestObjValue = model.getBestObjValue()
    val objValue = model.getObjValue()

    println("Solution status: " + status)
    println("Best bound:      " + bestObjValue)
    println("Best integer:    " + objValue)
  }


}
