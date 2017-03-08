/*
 * Source file provided under Apache License, Version 2.0, January 2004,
 * http://www.apache.org/licenses/
 * (c) Copyright DecisionBrain SAS 2016,2017
 */

package com.decisionbrain.cplex.cp

import com.decisionbrain.cplex.Addable
import ilog.concert.{IloAddable, IloCumulFunctionExpr, IloNumExpr}


/**
  * Class for numeric expressions
  *
  * @param expr  is the numeric expression
  * @param model is the constraint programming model
  */
class CumulFunctionExpr(val expr: IloCumulFunctionExpr)(implicit model: CpModel) extends Addable {

  /**
    * Returns the constraint programming model
    *
    * @return the constraint programming model
    */
  def getCpModel(): CpModel = model

  /**
    * Return the CPLEX numeric expression.
    *
    * @return the numeric expression
    */
  def getIloCumulFunctionExpr(): IloCumulFunctionExpr = expr

  /**
    * Creates and returns an expression representing the sum of this numeric expression and another numeric
    * expression.
    *
    * @param y is the other numeric expression
    * @return a numeric expression representing the sum <em>x + y</em>
    */
  def +(y: CumulFunctionExpr): CumulFunctionExpr = {
    CumulFunctionExpr(model.cp.sum(expr, y.getIloCumulFunctionExpr()))
  }

  /**
    * Creates and returns an expression representing the sum of the numeric expressions <em>x</em> and <em>that</em>
    *
    * @param y is the numeric expression to add
    * @return a numeric expression representing the sum <em>x - y</em>
    */
  def -(y: CumulFunctionExpr): CumulFunctionExpr =
    CumulFunctionExpr(model.cp.diff(expr, y.getIloCumulFunctionExpr()))

  /**
    * This function returns a constraint that states that the value of cumul function expression f should never be
    * greater than vmax.
    *
    * @param vmax is an integer value
    * @return a constraint on the maximum value of the cumul function
    */
  def <=(vmax: Int): Constraint =
    Constraint(model.cp.le(this.getIloCumulFunctionExpr(), vmax))

  /**
    * This function returns a constraint that states that the value of cumul function expression f should never be
    * greater than vmax.
    *
    * Note: This constraint cannot be used in a logical constraint.

    * @param vmax is the integer expression
    * @return a constraint on the maximum vale of the cumul function
    */
  def <=(vmax: IntExpr): Constraint =
    Constraint(model.cp.le(this.getIloCumulFunctionExpr(), vmax.getIloIntExpr()))

  /**
    * This function returns a constraint that states that the value of cumul function expression f should never be
    * smaller than vmin.
    *
    * @param vmin is an integer value
    * @return a constraint on the minimum value of the cumul function
    */
  def >=(vmin: Int): Constraint =
    Constraint(model.cp.ge(this.getIloCumulFunctionExpr(), vmin))

  /**
    * This function returns a constraint that states that the value of cumul function expression f should never be
    * smaller than vmin.
    *
    * @param vmin is an integer expression
    * @return a constraint on the minimum value of the cumul function
    */
  def >=(vmin: IntExpr): Constraint =
    Constraint(model.cp.ge(this.getIloCumulFunctionExpr(), vmin.getIloIntExpr()))


  /**
    * Return a character string that represents the numeric expression.
    *
    * @return a character string
    */
  override def toString: String = expr.toString()

  /**
    * Returns the name of model addable object.
    *
    * @return the name of the object
    */
  override def getName: Option[String] = Option(expr.getName())

  /**
    * Set the name of the model addable object.
    *
    * @param name is the name of the object
    */
  override def setName(name: String): Unit = expr.setName(name)

  /**
    * Returns the CPLEX addable object.
    *
    * @return the CPLEX addable object
    */
  override def getIloAddable(): IloAddable = expr
}

object CumulFunctionExpr {
  /**
    * Converts a CPLEX cumul function expression to a cumul function expression.
    *
    * @param e     is the CPLEX cumul funciton expression
    * @param model is the constraint programming model the cumul function expression belongs to
    * @return a cumul function expression
    */
  def apply(e: IloCumulFunctionExpr)(implicit model: CpModel) = new CumulFunctionExpr(e)
}

