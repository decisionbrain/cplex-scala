/*
 *  Source file provided under Apache License, Version 2.0, January 2004,
 *  http://www.apache.org/licenses/
 *  (c) Copyright DecisionBrain SAS 2016,2019
 */

package com.decisionbrain.cplex.mp

import com.decisionbrain.cplex.NumExpr

/**
 * This is the class for piecewise linear function.
 *
 * @param preslope is the slope of the first segment of the piecewise linear function
 * @param points is the set of points of the piecewise linear function
 * @param postslope is the slope of the last segment of the piecewise linear function
 * @param model is the mathematical programming model
 */
class PiecewiseLinearFunction(val preslope: Double,
                              val points: Iterable[(Double, Double)],
                              val postslope: Double)(implicit model: MpModel) {

  /**
   * Returns a numeric expression representing the application of a piecewise linear function to a numeric expression.
   *
   * @param expr is an expression indicating where to evaluate the piecewise linear function
   * @return a numeric expression
   */
  def apply(expr: NumExpr) : NumExpr = {
    val x = points.map(p => p._1)
    val y = points.map(p => p._2)
    NumExpr(model.cplex.piecewiseLinear(expr.getIloNumExpr(), preslope, x.toArray, y.toArray, postslope))
  }

}

object PiecewiseLinearFunction {
  /**
   * Creates and returns a piecewise linear function.
   *
   * @param preslope is the slope of the first segment of the piecewise linear function
   * @param points is the list of points of the piecewise linear function
   * @param postslope is the slope of the last segment of the piecewise linear function
   * @return a piecewise linear function
   */
  def apply(preslope: Double, points: Iterable[(Double, Double)], postslope: Double)(implicit model: MpModel) =
    new PiecewiseLinearFunction(preslope, points, postslope)
}
