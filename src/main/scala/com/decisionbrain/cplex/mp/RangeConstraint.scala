/*
 * Source file provided under Apache License, Version 2.0, January 2004,
 * http://www.apache.org/licenses/
 * (c) Copyright DecisionBrain SAS 2016,2017
 */

package com.decisionbrain.cplex.mp

import ilog.concert.IloRange

/**
  * Created by dgodard on 09/02/2017.
  */
class RangeConstraint(r: IloRange)(implicit model: MpModel) extends Constraint(r) {

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

object RangeConstraint {
  /**
    * Converts a CPLKEX range constraint to a range constraint.
    *
    * @param r is the CPLEX range constraint
    * @param model is the mathematical programming model
    * @return a range constraint
    */
  def apply(r: IloRange)(implicit model: MpModel): RangeConstraint = new RangeConstraint(r)
}