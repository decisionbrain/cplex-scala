/*
 * Source file provided under Apache License, Version 2.0, January 2004,
 * http://www.apache.org/licenses/
 * (c) Copyright DecisionBrain SAS 2016,2017
 */

package com.decisionbrain.cplex.cp

/**
  * Created by dgodard on 11/02/2017.
  */
/**
  * This class allows one to write things like List(expr1, expr2, ...).sum
  *
  * @param model is the mathematical programming model
  */
class IntExprNumeric(implicit model: CpModel) extends Numeric[IntExpr] {

  override def fromInt(x: Int): IntExpr = IntExpr(model.cp.linearIntExpr(x))

  override def plus(x: IntExpr, y: IntExpr): IntExpr = x + y

  override def minus(x: IntExpr, y: IntExpr): IntExpr = x - y

  override def times(x: IntExpr, y: IntExpr): IntExpr = x * y

  override def negate(x: IntExpr): IntExpr = -x

  override def toInt(x: IntExpr): Int = ???

  override def toLong(x: IntExpr): Long = ???

  override def toFloat(x: IntExpr): Float = ???

  override def toDouble(x: IntExpr): Double = ???

  override def compare(x: IntExpr, y: IntExpr): Int = ???
}


object IntExprNumeric {

  def apply(model: CpModel): Numeric[IntExpr] = new IntExprNumeric()(implicitly(model))

  implicit def num(implicit model: CpModel): Numeric[IntExpr] = model.getIntExprNumeric()
}
