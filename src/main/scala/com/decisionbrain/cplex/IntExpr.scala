/*
 * Source file provided under Apache License, Version 2.0, January 2004,
 * http://www.apache.org/licenses/
 * (c) Copyright DecisionBrain SAS 2016,2018
 */

package com.decisionbrain.cplex

import com.decisionbrain.cplex.cp.{CpModel, CumulFunctionExpr}
import ilog.concert.IloIntExpr

/**
  * Created by dgodard on 11/02/2017.
  */
class IntExpr(expr: IloIntExpr)(implicit modeler: Modeler) extends NumExpr(expr) {

  /**
    * Returns the CPLEX integer expression
    *
    * @return
    */
  def getIloIntExpr(): IloIntExpr = expr

  /**
    * Creates and returns an expression representing the sum of this numeric expression and another numeric
    * expression.
    *
    * @param y is the other numeric expression
    * @return a numeric expression representing the sum <em>x + y</em>
    */
  def +(y: IntExpr): IntExpr = modeler.sum(this, y)

  /**
    * Creates and returns an expression representing the sum of this numeric expression and a numeric constant.
    *
    * @param v is the numeric constant
    * @return a numeric expression representing the sum <em>x + v</em>
    */
  def +(v: Int): IntExpr = modeler.sum(this, v)

  /**
    * Creates and returns an expression representing the sum of the numeric expressions <em>x</em> and <em>that</em>
    *
    * @param y is the numeric expression to add
    * @return a numeric expression representing the sum <em>x - y</em>
    */
  def -(y: IntExpr): IntExpr = modeler.diff(this, y)

  /**
    * Creates and returns an expression representing the diff of this numeric expression and a numeric constant.
    *
    * @param v is the numeric constant
    * @return a numeric expression representing the sum <em>x - v</em>
    */
  def -(v: Int): IntExpr = modeler.diff(this, v)

  /**
    * Create and returns an expression that is the negation of this expression.
    *
    * @return the negation of this expression
    */
  override def unary_- : IntExpr = modeler.negative(this)

  /**
    * Creates and returns an expression representing the product of the expression x and the value v.
    *
    * @param v is the value to use in the product.
    * @return a numeric expression representing the product <em>e1 * v</em>.
    */
  def *(v: Int): IntExpr = modeler.prod(this, v)

  /**
    * Creates and returns an expression representing the product of the expression x and another expression y.
    *
    * @param expr is the value to use in the product.
    * @return a numeric expression representing the product <em>e1 * v</em>.
    */
  def *(expr: IntExpr): IntExpr = modeler.prod(this, expr)

  /**
    * Creates and returns the integer division e1 / e2.
    *
    * @param expr is the divisor
    * @return an integer expression equals to the integer division e1 / e2.
    */
  def /(expr: IntExpr): IntExpr = modeler.div(this, expr)

  /**
    * Creates and returns the integer division e1 / e2.
    *
    * @param v is the divisor
    * @return an integer expression equals to the integer division e1 / e2.
    */
  def /(v: Int): IntExpr = modeler.div(this, v)

  /**
    * Creates and returns the constraint <em>expr1 != expr2</em>.
    *
    * @param expr is the numeric expression of the new not-equal-to constraint.
    * @return a new constraint <em>expr1 != expr2</em>.
    */
  def !=(expr: IntExpr): Constraint = modeler.neq(this, expr)

  /**
    * Creates and returns the constraint <em>expr1 != value</em>.
    *
    * @param value is an integer value
    * @return a new constraint <em>expr1 != value</em>.
    */
  def !=(value: Int): Constraint = modeler.neq(this, value)

  /**
    * Creates and returns an instance of IloRange that represents the constraint <em>expr >= v</em>.
    *
    * @param value is the lower bound of the new greater-than-or-equal-to constraint.
    * @return a new <em>IloRange</em> instance that represents the constraint <em>x >= value</em>.
    */
  def >(value: Int): Constraint = modeler.gt(this, value)

  /**
    * Creates and returns an instance of IloConstraint that represents the constraint <em>expr1 >= expr2</em>.
    *
    * @param e is the right-hand side expression of the greater than or equal constraint
    * @return a new constraint
    */
  def >(expr: IntExpr): Constraint = modeler.gt(this, expr)

  /**
    * Creates and returns an instance of IloRange that represents the constraint <em>expr <= v</em>.
    *
    * @param value is the upper bound of the new less-than-or-equal-to constraint.
    * @return a new <em>IloRange</em> instance that represents the constraint <em>x <= value</em>.
    */
  def <(value: Int): Constraint = modeler.lt(this, value)

  /**
    * Creates and returns an instance of IloRange that represents the constraint <em>expr1 <= expr2</em>.
    *
    * @param expr is the right-handside expression of the new less-than-or-equal-to constraint.
    * @return a new <em>IloConstraint</em>
    */
  def <(expr: IntExpr): Constraint = modeler.lt(this, expr)

  /**
    * This function returns a constraint that states that the value of cumul function expression f should never be
    * smaller than this integer expression.
    *
    * @param f is the cumul function expression
    * @return a constraint on the minimum value of the cumul function
    */
  def <=(f: CumulFunctionExpr): Constraint = modeler.le(this, f)

  /**
    * This function returns a constraint that states that the value of cumul function expression f should never be
    * greater than this integer expression.
    *
    * @param f is the cumul function expression
    * @return a constraint on the maximum value of the cumul function
    */
  def >=(f: CumulFunctionExpr): Constraint = modeler.ge(this, f)
}

object IntExpr {
  /**
    * Converts a CPLEX integer expression to an integer expression.
    *
    * @param e is the CPLEX numeric expression
    * @param modeler is the optimization modeler
    * @return a numeric expression
    */
  def apply(e: IloIntExpr)(implicit modeler: Modeler) = new IntExpr(e)

  /**
    * Convert a constant value to an integer expression.
    *
    * @param v is the constant value
    * @param modeler is the constraint programming model
    * @return a integer expression
    */
  def apply(v: Int)(implicit modeler: Modeler) = modeler.linearIntExpr(v)

  /**
    * Implicit conversion of an integer to a constant integer expression.
    *
    * @param value is the integer value
    * @param modeler is the constraint programming model
    */
  implicit class LinearIntExpr(val value: Int)(implicit modeler: Modeler)
    extends IntExpr(modeler.getIloModeler().linearIntExpr(value)) {
  }

}

