/*
 * Source file provided under Apache License, Version 2.0, January 2004,
 *  http://www.apache.org/licenses/
 *  (c) Copyright DecisionBrain SAS 2016,2020
 *
 */

package com.decisionbrain.cplex

import ilog.concert.IloRange

/**
  * Constructor of class RangeConstraint.
  *
  * @param r is a CPLEX range constraint
  * @param modeler is the constraint programming model
  */
class Range(r: IloRange)(implicit modeler: Modeler) extends Constraint(r) {

  /**
    * Returns the CPLEX range constraint
    *
    * @return the CPLEX range constraint
    */
  def getIloRange(): IloRange = r

  /**
    * Returns the lower bound of the range constraint
    *
    * @return the lower bound
    */
  def getLB(): Double = r.getLB

  /**
    * Return the upper bound of the range constraint?
    *
    * @return the upper bound
    */
  def getUB: Double = r.getUB

  /**
    * Returns the numeric expression of the range constraint
    *
    * @return the numeric expression
    */
  def getNumExpr(): NumExpr = NumExpr(r.getExpr)

  @deprecated("This method has been replaced by getNumExpr", "decisionbrain-cplex-scala-1.0.0")
  def getExpr(): NumExpr = NumExpr(r.getExpr)

}

object Range {
  /**
    * Converts a CPLKEX range constraint to a range constraint.
    *
    * @param r is the CPLEX range constraint
    * @param modeler is the constraint programming model
    * @return a range constraint
    */
  def apply(r: IloRange)(implicit modeler: Modeler): Range = new Range(r)
}