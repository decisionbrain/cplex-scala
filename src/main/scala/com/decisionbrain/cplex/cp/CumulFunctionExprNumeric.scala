/*
 * Source file provided under Apache License, Version 2.0, January 2004,
 *  http://www.apache.org/licenses/
 *  (c) Copyright DecisionBrain SAS 2016,2020
 *
 */

package com.decisionbrain.cplex.cp

/**
  * This class allows one to write things like List(expr1, expr2, ...).sum
  *
  * @param model is the mathematical programming model
  */
class CumulFunctionExprNumeric(implicit model: CpModel) extends Numeric[CumulFunctionExpr] {

  override def fromInt(x: Int): CumulFunctionExpr = model.cumulFunctionExpr(x)

  override def plus(x: CumulFunctionExpr, y: CumulFunctionExpr): CumulFunctionExpr = x + y

  override def minus(x: CumulFunctionExpr, y: CumulFunctionExpr): CumulFunctionExpr = x - y

  override def times(x: CumulFunctionExpr, y: CumulFunctionExpr): CumulFunctionExpr = ???

  override def negate(x: CumulFunctionExpr): CumulFunctionExpr = -x

  override def toInt(x: CumulFunctionExpr): Int = ???

  override def toLong(x: CumulFunctionExpr): Long = ???

  override def toFloat(x: CumulFunctionExpr): Float = ???

  override def toDouble(x: CumulFunctionExpr): Double = ???

  override def compare(x: CumulFunctionExpr, y: CumulFunctionExpr): Int = ???

  override def parseString(str: String): Option[CumulFunctionExpr] = ???
}

object CumulFunctionExprNumeric {

  def apply(model: CpModel): Numeric[CumulFunctionExpr] = new CumulFunctionExprNumeric()(implicitly(model))

  implicit def num(implicit model: CpModel): Numeric[CumulFunctionExpr] = model.getCumulFunctionExprNumeric()
}
