/*
 * Source file provided under Apache License, Version 2.0, January 2004,
 * http://www.apache.org/licenses/
 * (c) Copyright DecisionBrain SAS 2016,2018
 */

package com.decisionbrain.cplex

import com.decisionbrain.cplex.cp.CpModel

/**
  * Created by dgodard on 11/02/2017.
  */
/**
  * This class allows one to write things like List(expr1, expr2, ...).sum
  *
  * @param modeler is the mathematical programming model
  */
class NumExprNumeric(implicit modeler: Modeler) extends Numeric[NumExpr] {

  override def fromInt(x: Int): NumExpr = modeler.linearNumExpr(x)

  override def plus(x: NumExpr, y: NumExpr): NumExpr = x + y

  override def minus(x: NumExpr, y: NumExpr): NumExpr = x - y

  override def times(x: NumExpr, y: NumExpr): NumExpr = x * y

  override def negate(x: NumExpr): NumExpr = -x

  override def toInt(x: NumExpr): Int = ???

  override def toLong(x: NumExpr): Long = ???

  override def toFloat(x: NumExpr): Float = ???

  override def toDouble(x: NumExpr): Double = ???

  override def compare(x: NumExpr, y: NumExpr): Int = ???
}

object NumExprNumeric {

  def apply(modeler: Modeler): Numeric[NumExpr] = new NumExprNumeric()(implicitly(modeler))

  implicit def num(implicit model: Modeler): Numeric[NumExpr] = model.getNumExprNumeric()
}
