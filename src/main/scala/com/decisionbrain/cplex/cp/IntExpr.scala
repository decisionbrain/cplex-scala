/*
 * Source file provided under Apache License, Version 2.0, January 2004,
 * http://www.apache.org/licenses/
 * (c) Copyright DecisionBrain SAS 2016,2017
 */

package com.decisionbrain.cplex.cp

import ilog.concert.{IloIntExpr, IloNumExpr}
import ilog.cp.IloCP

/**
  * Created by dgodard on 11/02/2017.
  */
class IntExpr(expr: IloIntExpr)(implicit model: CpModel) extends NumExpr(expr) {

  /**
    * Returns the CPLEX integer expression
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
  def +(y: IntExpr): IntExpr = {
    IntExpr(model.cp.sum(expr, y.getIloIntExpr()))
  }

  /**
    * Creates and returns an expression representing the sum of this numeric expression and a numeric constant.
    *
    * @param v is the numeric constant
    * @return a numeric expression representing the sum <em>x + v</em>
    */
  def +(v: Int): IntExpr = {
    IntExpr(model.cp.sum(expr, v))
  }

  /**
    * Creates and returns an expression representing the sum of the numeric expressions <em>x</em> and <em>that</em>
    *
    * @param y is the numeric expression to add
    * @return a numeric expression representing the sum <em>x - y</em>
    */
  def -(y: IntExpr): IntExpr = {
    IntExpr(model.cp.diff(expr, y.getIloIntExpr))
  }

  /**
    * Creates and returns an expression representing the diff of this numeric expression and a numeric constant.
    *
    * @param v is the numeric constant
    * @return a numeric expression representing the sum <em>x - v</em>
    */
  def -(v: Int): IntExpr = {
    IntExpr(model.cp.diff(expr, v))
  }

  /**
    * Create and returns an expression that is the negation of this expression.
    *
    * @return the negation of this expression
    */
  override def unary_- : IntExpr = IntExpr(model.cp.negative(expr))

  /**
    * Creates and returns an expression representing the product of the expression x and the value v.
    *
    * @param v is the value to use in the product.
    * @return a numeric expression representing the product <em>e1 * v</em>.
    */
  def *(v: Int): IntExpr = {
    IntExpr(model.cp.prod(expr, v))
  }

  /**
    * Creates and returns an expression representing the product of the expression x and another expression y.
    *
    * @param expr is the value to use in the product.
    * @return a numeric expression representing the product <em>e1 * v</em>.
    */
  def *(expr: IntExpr): IntExpr = {
    IntExpr(model.cp.prod(this.expr, expr.getIloIntExpr()))
  }

  /**
    * Creates and returns the integer division e1 / e2.
    *
    * @param e2 is the divisor
    * @return an integer expression equals to the integer division e1 / e2.
    */
  def /(e2: IntExpr): IntExpr =
    IntExpr(model.cp.div(this.expr, e2.getIloIntExpr()))

  /**
    * Creates and returns the integer division e1 / e2.
    *
    * @param e2 is the divisor
    * @return an integer expression equals to the integer division e1 / e2.
    */
  def /(e2: Int): IntExpr =
    IntExpr(model.cp.div(this.expr, e2))

  /**
    * Creates and returns the constraint <em>expr1 != expr2</em>.
    *
    * @param e is the numeric expression of the new not-equal-to constraint.
    * @return a new constraint <em>expr1 != expr2</em>.
    */
  def !=(e: IntExpr): Constraint = {
    Constraint(model.cp.neq(expr, e.getIloIntExpr()))
  }

  /**
    * Creates and returns the constraint <em>expr1 != value</em>.
    *
    * @param value is an integer value
    * @return a new constraint <em>expr1 != value</em>.
    */
  def !=(value: Int): Constraint = {
    Constraint(model.cp.neq(expr, value))
  }

  /**
    * Creates and returns an instance of IloRange that represents the constraint <em>expr >= v</em>.
    *
    * @param value is the lower bound of the new greater-than-or-equal-to constraint.
    * @return a new <em>IloRange</em> instance that represents the constraint <em>x >= value</em>.
    */
  def >(value: Int): Constraint = {
    Constraint(model.cp.gt(expr, value))
  }

  /**
    * Creates and returns an instance of IloConstraint that represents the constraint <em>expr1 >= expr2</em>.
    *
    * @param e is the right-hand side expression of the greater than or equal constraint
    * @return a new constraint
    */
  def >(e: IntExpr): Constraint = {
    Constraint(model.cp.gt(expr, e.getIloIntExpr()))
  }

  /**
    * Creates and returns an instance of IloRange that represents the constraint <em>expr <= v</em>.
    *
    * @param value is the upper bound of the new less-than-or-equal-to constraint.
    * @return a new <em>IloRange</em> instance that represents the constraint <em>x <= value</em>.
    */
  def <(value: Int): Constraint = {
    Constraint(model.cp.lt(expr, value))
  }

  /**
    * Creates and returns an instance of IloRange that represents the constraint <em>expr1 <= expr2</em>.
    *
    * @param e is the right-handside expression of the new less-than-or-equal-to constraint.
    * @return a new <em>IloConstraint</em>
    */
  def <(e: IntExpr): Constraint = {
    Constraint(model.cp.lt(expr, e.getIloIntExpr()))
  }

  /**
    * This function returns a constraint that states that the value of cumul function expression f should never be
    * smaller than this integer expression.
    *
    * @param f is the cumul function expression
    * @return a constraint on the minimum value of the cumul function
    */
  def <=(f: CumulFunctionExpr): Constraint =
    Constraint(model.cp.le(this.getIloIntExpr(), f.getIloCumulFunctionExpr()))

  /**
    * This function returns a constraint that states that the value of cumul function expression f should never be
    * greater than this integer expression.
    *
    * @param f is the cumul function expression
    * @return a constraint on the maximum value of the cumul function
    */
  def >=(f: CumulFunctionExpr): Constraint =
    Constraint(model.cp.ge(this.getIloIntExpr(), f.getIloCumulFunctionExpr()))
}

object IntExpr {
  /**
    * Converts a CPLEX numeric expression to a numeric expression.
    *
    * @param e is the CPLEX numeric expression
    * @param model is the constraint programming model the numeric expression belongs to
    * @return a numeric expression
    */
  def apply(e: IloIntExpr)(implicit model: CpModel) = new IntExpr(e)

  /**
    * Convert a constant value to an integer expression.
    *
    * @param v is the constant value
    * @param model is the constraint programming model
    * @return a integer expression
    */
  def apply(v: Int)(implicit model: CpModel) = new IntExpr(model.cp.linearIntExpr(v))

  /**
    * Implicit conversion of an integer to a constant integer expression.
    *
    * @param value is the integer value
    * @param model is the constraint programming model
    */
  implicit class LinearIntExpr(val value: Int)(implicit model: CpModel)
    extends IntExpr(model.cp.linearIntExpr(value)) {
  }

}

