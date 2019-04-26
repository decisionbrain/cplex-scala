/*
 *  Source file provided under Apache License, Version 2.0, January 2004,
 *  http://www.apache.org/licenses/
 *  (c) Copyright DecisionBrain SAS 2016,2019
 */

package com.decisionbrain.cplex

import ilog.concert.{IloAddable, IloNumVar, IloNumVarType}

/**
  * Class for numeric variables.
  *
  * @param v     is the CPLEX numeric variable
  * @param modeler is the constraint programming model
  */
class NumVar(v: IloNumVar)(implicit modeler: Modeler) extends NumExpr(v) with Addable {

  /**
    * Return the type of the numeric variable i.e. Float, Int or Bool
    *
    * @return the type
    */
  def getType(): IloNumVarType = v.getType

  /**
    * Return the upper bound of the numeric variable.
    *
    * @return the upper bound
    */
  def getUB(): Double = v.getUB()

  /**
    * Set the upper bound of the numeric variable.
    *
    * @param value is the upper bound
    */
  def setUB(value: Double) = v.setUB(value)

  /**
    * Return the lower bound of the numeric variable
    *
    * @return the lower bound
    */
  def getLB(): Double = v.getLB()

  /**
    * Set the lower bound of the numeric variable.
    *
    * @param value is the
    */
  def setLB(value: Double) = v.setLB(value)

  /**
    * Return the CPLEX numeric variable.
    *
    * @return the numeric variable
    */
  def getIloNumVar(): IloNumVar = v

  /**
    * Returns the CPLEX numeric variable
    *
    * @return the CPLEX numeric variable
    */
  override def getIloAddable(): IloAddable = v

  /**
    * Return a character string that represents the integer variable.
    *
    * @return a character string
    */
  override def toString() : String = {
    val strBuilder = new StringBuilder()
    if (getName.isDefined) strBuilder.append(getName.get)
    strBuilder.append("[")
    val vmin = getLB()
    val vmax = getUB()
    strBuilder.append(vmin)
    if (vmin < vmax) {
      strBuilder.append("..")
      strBuilder.append(vmax)
    }
    strBuilder.append("]")
    strBuilder.toString()
  }

}

object NumVar {

  /**
    * Converts a CPLEX numeric variable to a numeric variable.
    *
    * @param v is the CPLEX numeric variable
    * @param model is the constraint programming model
    * @return a numeric variable
    */
  def apply(v: IloNumVar)(implicit model: Modeler): NumVar = new NumVar(v)
}