/*
 * Source file provided under Apache License, Version 2.0, January 2004,
 * http://www.apache.org/licenses/
 * (c) Copyright DecisionBrain SAS 2016,2018
 */

package com.decisionbrain.cplex

import ilog.concert.{IloAddable, IloIntVar, IloNumVar, IloNumVarType}

/**
  * Class for numeric variables.
  *
  * @param v     is the CPLEX variable
  * @param modeler is the optimization model
  */
class IntVar(v: IloIntVar)(implicit modeler: Modeler) extends IntExpr(v) with Addable {

  /**
    * Returns the CPLEX integer variable.
    *
    * @return the CPLEX integer variable
    */
  def getIloIntVar(): IloIntVar = v


  /**
    * Return the type of the numeric variable i.e. Float, Int or Bool
    *
    * @return the type
    */
  def getType(): IloNumVarType = v.getType

  /**
    * Return the name of the numeric variable
    * @return the name
    */
  def getName(): Option[String] = Option(v.getName())

  /**
    * Set the name of the numeric variable.
    *
    * @param name is the name of the numeric variable
    */
  def setName(name: String) = v.setName(name)

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
    * Returns the minimum value of the integer variable.
    *
    * @return the minimum value of the integer variable
    */
  def getMin(): Int = v.getMin

  /**
    * Sets a new minimum value for this integer variable.
    *
    * @return the minimum value of the integer variable
    */
  def setMin(value: Int)= v.setMin(value)

  /**
    * Returns the maximum value of the integer variable.
    *
    * @return the maximum value of the integer variable
    */
  def getMax(): Int = v.getMax

  /**
    * Sets a new maximum value for this integer variable.
    *
    * @return the minimum value of the integer variable
    */
  def setMax(value: Int)= v.setMax(value)

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
    if (getName().isDefined) strBuilder.append(getName().get)
    strBuilder.append("[")
    val vmin = getMin()
    val vmax = getMax()
    strBuilder.append(vmin)
    if (vmin < vmax) {
      strBuilder.append("..")
      strBuilder.append(vmax)
    }
    strBuilder.append("]")
    strBuilder.toString()
  }

}

object IntVar {

  /**
    * Converts a CPLEX numeric variable to a numeric variable.
    *
    * @param v is the CPLEX numeric variable
    * @param modeler is the optimization model
    * @return a numeric variable
    */
  def apply(v: IloIntVar)(implicit modeler: Modeler): IntVar = new IntVar(v)
}
