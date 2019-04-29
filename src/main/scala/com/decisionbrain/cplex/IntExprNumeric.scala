/*
 *  Source file provided under Apache License, Version 2.0, January 2004,
 *  http://www.apache.org/licenses/
 *  (c) Copyright DecisionBrain SAS 2016,2019
 */

package com.decisionbrain.cplex

/**
  * Created by dgodard on 11/02/2017.
  */
/**
  * This class allows one to write things like List(expr1, expr2, ...).sum
  *
  * @param modeler is the optimization model
  */
class IntExprNumeric(modeler: Modeler) extends Numeric[IntExpr] {

  override def fromInt(x: Int): IntExpr = modeler.linearIntExpr(x)

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

  def apply(modeler: Modeler): IntExprNumeric = new IntExprNumeric(modeler)

  implicit def num(implicit modeler: Modeler): IntExprNumeric = modeler.getIntExprNumeric()
}
