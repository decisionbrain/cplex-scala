/*
 * Source file provided under Apache License, Version 2.0, January 2004,
 * http://www.apache.org/licenses/
 * (c) Copyright DecisionBrain SAS 2016,2018
 */

package com.decisionbrain.cplex.cp

import com.decisionbrain.cplex.Addable
import ilog.concert.{IloAddable, IloNumVar, IloNumVarType}

/**
  * Class for numeric variables.
  *
  * @param v     is the CPLEX variable
  * @param model is the constraint programming model
  */
class NumVar(v: IloNumVar)(implicit model: CpModel) extends NumExpr(v) with Addable {

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
}

object NumVar {

  /**
    * Converts a CPLEX numeric variable to a numeric variable.
    *
    * @param v is the CPLEX numeric variable
    * @param model is the constraint programming model
    * @return a numeric variable
    */
  def apply(v: IloNumVar)(implicit model: CpModel): NumVar = new NumVar(v)
}