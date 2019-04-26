/*
 *  Source file provided under Apache License, Version 2.0, January 2004,
 *  http://www.apache.org/licenses/
 *  (c) Copyright DecisionBrain SAS 2016,2019
 */

package com.decisionbrain.cplex.mp

import com.decisionbrain.cplex._
import com.decisionbrain.cplex.mp.MpModel._
import com.decisionbrain.cplex.Modeler._
import ilog.concert.IloModeler
import ilog.cplex.IloCplex.Param
import ilog.cplex.{IloCplex, IloCplexMultiCriterionExpr}

/**
  *
  * The class Model is for modeling mathematical programming (MP) problems using IloCplex. It adds support for
  * several modeling object specific to mathematical programming, including LP matrices, semi-continuous variables,
  * and special ordered sets (SOSs). It extends the functionality of IloModeler to column-wise modeling and supports
  * modification of variable types and expressions of ranged constraints and objective functions. This interface is
  * implemented by the <em>IloCplex</em> optimizer class.

  * @param name is the name of the model
  */
case class MpModel(name: String=null) extends Modeler {

  val cplex: IloCplex = {
    val iloCplex = new IloCplex()
    iloCplex.setName(name)
    iloCplex
  }

  /**
    * Returns the CPLEX modeler i.e the interface for building optimization models.
    *
    * @return the CPLEX modeler
    */
  def getIloModeler(): IloModeler = this.cplex


  /**
    * Returns the name of the optimization model
    *
    * @return the name of the optimization model
    */
  def getName(): Option[String] = Option(cplex.getName())

  /**
    * Return the CPLEX mathematical programming optimizer
    *
    * @return the CPELX mathematical programming Optimizer
    */
  def getIloCplex(): IloCplex = cplex

  /**
    * Return the value of a numeric expression in the solution.
    *
    * @param expr is the numeric expression; the expression must be in the active model
    * @return the value of the expression in the solution
    */
  def getValue(expr: NumExpr) : Double = cplex.getValue(expr.getIloNumExpr())

  /**
    * Print some information on the CPLEX model e.g. number of variables, number of constraints...
    */
  def printInformation(): Unit = {
    println("CPLEX model: " + name)
    println("\t# variables: " + cplex.getNcols())
    println("\t# binary variables: " + cplex.getNbinVars)
    println("\t# integer variables: " + cplex.getNintVars())
    println("\t# constraints: " + cplex.getNrows())
  }

  /**
    * Return a range that represent the constraint <em>expr >= rhs<em>.
    *
    * @param expr is the numeric expression of the new greater-than-or-equal-to constraint
    * @param rhs is the upper bound of the new greater-than-or-equal-to constraint
    * @return a new range constraint <em>expr >= rhs</em>
    */
  def ge(expr: NumExpr, rhs: Double, name: String=null): Range = {
    Range(cplex.ge(expr.getIloNumExpr, rhs, name))(implicitly(this))
  }

  /**
    * Return a range that represent the constraint <em>expr >= rhs<em>.
    *
    * @param expr is the numeric expression of the new greater-than-or-equal-to constraint
    * @param rhs is the upper bound of the new greater-than-or-equal-to constraint
    * @return a range constraint <em>expr >= rhs</em>
    */
  def le(expr: NumExpr, rhs: Double, name: String=null): Range = {
    Range(cplex.le(expr.getIloNumExpr, rhs, name))(implicitly(this))
  }

  /**
    * Create and return a range constraint <em>expr == rhs<em>. Add it to the model.
    *
    * @param expr is the expression
    * @param value is the value
    * @return the model
    */
  def addEq(expr: NumExpr, value: Double): MpModel = {
    addEq(expr, value, null)
  }

  /**
    * Create and return a range constraint <em>expr == rhs<em>. Add it to the model.
    *
    * @param expr is the expression
    * @param value is the value
    * @param name is the name of the range constraint
    * @return the model
    */
  def addEq(expr: NumExpr, value: Double, name: String): MpModel = {
    cplex.addEq(expr.getIloNumExpr, value, name)
    this
  }

  /**
    * Create and return a range constraint <em>expr1 == expr2<em>. Add it to the model.
    *
    * @param expr1 is the first expression
    * @param expr2 is the second expression
    * @return the model
    */
  def addEq(expr1: NumExpr, expr2: NumExpr): MpModel = {
    addEq(expr1, expr2, null)
  }

  /**
    * Create and return a range constraint <em>expr1 == expr2<em>. Add it to the model.
    *
    * @param expr1 is the first expression
    * @param expr2 is the second expression
    * @return the model
    */
  def addEq(expr1: NumExpr, expr2: NumExpr, name: String): MpModel = {
    cplex.addEq(expr1.getIloNumExpr, expr2.getIloNumExpr, name)
    this
  }

  /**
    * Creates and returns an range constraint <em>lb <= expr <= ub</em>.
    *
    * @param lb is the lower bound
    * @param expr is the numeric expression of the constraint
    * @param ub is the upper bound
    * @return an new range constraint
    */
  def range(lb: Double, expr: NumExpr, ub: Double, name: String=null): Range = {
    Range(cplex.range(lb, expr.getIloNumExpr, ub, name))(implicitly(this))
  }

  /**
    * Creates a range constraint <em>lb <= expr <= ub</em> and add it to the model.
    *
    * @param lb is the lower bound
    * @param expr is the numeric expression of the constraint
    * @param ub is the upper bound
    * @return the model
    */
  def addRange(lb: Double, expr: NumExpr, ub: Double, name: String=""): MpModel = {
    add(range(lb, expr, ub, name))
    this
  }

  /**
    * Creates and return a minimization multi-criteria objective.
    *
    * @param expr is the multi-criteria expressions
    * @return an objective
    */
  def minimize(expr: CplexMultiCriterionExpr): Objective = Objective(cplex.minimize(expr))(implicitly(this))

    /**
    * Creates and return a maximization multi-criteria objective.
    *
    * @param expr is the multi-criteria expressions
    * @return an objective
    */
  def maximize(expr: CplexMultiCriterionExpr): Objective = Objective(cplex.maximize(expr))(implicitly(this))

  /**
    * This function defines a multi-criteria expression for lexicographic ordering. A lexicographic ordering means that
    * any improvement of the i-th criterion is more important than any improvement of the subsequent criteria.
    *
    * @param exprs a set of numeric expressions for the lexicographic ordering
    */
  def staticLex(exprs: NumExpr*): CplexMultiCriterionExpr = {
    cplex.staticLex(exprs.map(e => e.getIloNumExpr()).toArray, name)
  }

  /**
    * This function defines a multi-criteria expression for lexicographic ordering. A lexicographic ordering means that
    * any improvement of the i-th criterion is more important than any improvement of the subsequent criteria.
    *
    * @param exprs a set of numeric expressions for the lexicographic ordering
    * @param name is the name of the multi-criteria expression
    * @return
    */
  def staticLex(exprs: NumExprArray, name: String): CplexMultiCriterionExpr = {
    cplex.staticLex(exprs.toIloArray, name)
  }

  /**
    * This function defines a multi-criteria expression for lexicographic ordering. A lexicographic ordering means that
    * any improvement of the i-th criterion is more important than any improvement of the subsequent criteria.
    *
    * @param exprs a set of numeric expressions for the lexicographic ordering
    * @return
    *
    */
  def staticLex(exprs: NumExprArray): CplexMultiCriterionExpr = {
    cplex.staticLex(exprs.toIloArray)
  }

  /**
    * This function defines a multi-criteria expression for lexicographic ordering. A lexicographic ordering means that
    * any improvement of the i-th criterion is more important than any improvement of the subsequent criteria.
    *
    * @param exprs a set of numeric expressions for the lexicographic ordering
    */
  def staticLex(exprs: Array[NumExpr], name: String =null): CplexMultiCriterionExpr = {
    cplex.staticLex(exprs.map(e => e.getIloNumExpr()), name)
  }

  /**
    * This function defines a multi-criteria expression for lexicographic ordering. A lexicographic ordering means that
    * any improvement of the i-th criterion is more important than any improvement of the subsequent criteria.
    *
    * @param exprs a set of numeric expressions for the lexicographic ordering
    * @param weights the weight of each criterion
    * @param priorities the priority of each criterion
    * @param absTols the absolute tolerance of each criterion
    * @param relTols the relative tolerance of each criterion
    * @param name the name of the multi-criteria expression
    * @return
    */
  def staticLex(exprs: Array[NumExpr],
                weights: Array[Double],
                priorities: Array[Int],
                absTols: Array[Double],
                relTols: Array[Double],
                name: String): CplexMultiCriterionExpr = {
    cplex.staticLex(exprs.map(e => e.getIloNumExpr()), weights, priorities, absTols, relTols, name)
  }

  /**
    * This function defines a multi-criteria expression for lexicographic ordering. A lexicographic ordering means that
    * any improvement of the i-th criterion is more important than any improvement of the subsequent criteria.
    *
    * @param exprs a set of numeric expressions for the lexicographic ordering
    * @param weights the weight of each criterion
    * @param priorities the priority of each criterion
    * @param absTols the absolute tolerance of each criterion
    * @param relTols the relative tolerance of each criterion
    * @return
    */
  def staticLex(exprs: Array[NumExpr],
                weights: Array[Double],
                priorities: Array[Int],
                absTols: Array[Double],
                relTols: Array[Double]): CplexMultiCriterionExpr = {
    cplex.staticLex(exprs.map(e => e.getIloNumExpr()), weights, priorities, absTols, relTols, null)
  }

  /**
    * Solves the active model.
    *
    * @return A Boolean value indicating whether a feasible solution has been found. This solution is not
    *         necessarily optimal. If <em>false</em> is returned, a feasible solution may still be present,
    *         but IloCplex has not been able to prove its feasibility.
    */
  def solve(timeLimit: Double = Double.PositiveInfinity, mipGap: Double = .0) = {
    if (timeLimit < Double.PositiveInfinity) cplex.setParam(Param.TimeLimit, timeLimit) // name prior to V12.6.0: IloCplex.DoubleParam.TiLim
    if (mipGap > .0) cplex.setParam(Param.MIP.Tolerances.MIPGap, mipGap) // name prior to V12.6.0: IloCplex.DoubleParam.EpGap
    cplex.solve()
  }

  /**
    * the Solves the active multi-objective model.
    *
    * @param parameters are the parameters used for solving the sub-problem
    * @return A Boolean value indicating whether a feasible solution has been found. This solution is not
    *         necessarily optimal. If <em>false</em> is returned, a feasible solution may still be present,
    *         but IloCplex has not been able to prove its feasibility.
    */
  def solve(parameters: Array[ParameterSet]) =
    cplex.solve(parameters)


  /**
    * Returns the solution status of the active model.
    *
    * @return the solution status
    */
  def getStatus() = cplex.getStatus()


  /**
    * Returns the value of the objective in the solution.
    *
    * @return the value of the objective in the solution
    */
  def getObjectiveValue() = cplex.getObjValue


  /**
    * Returns the objective value of the best remaining node.
    *
    * @return the objective value of the best remaining node
    */
  def getBestObjValue() = cplex.getBestObjValue

  /**
    * Returns the number of multi-criteria objective solves
    *
    * @return the number of multi-criteria objective solves
    */
  def getMultiObjNsolves() = cplex.getMultiObjNsolves

  /**
    * Returns the solution info of a sub-problem of a multi-objective optimization.
    *
    * @param info is the solution info requested
    * @param step the index of the sub-problem
    * @return the solution info of a multi-objective optimization
    */
  def getMultiObjInfo(info: IloCplex.MultiObjIntInfo, step: Int): Int =
    return cplex.getMultiObjInfo(info, step)

  /**
    * Returns the solution info of a sub-problem of a multi-objective optimization.
    *
    * @param info is the solution info requested
    * @param step the index of the sub-problem
    * @return the solution info of a multi-objective optimization
    */
  def getMultiObjInfo(info: IloCplex.MultiObjLongInfo, step: Int): Long =
    return cplex.getMultiObjInfo(info, step)

  /**
    * Returns the solution info of a sub-problem of a multi-objective optimization.
    *
    * @param info is the solution info requested
    * @param step the index of the sub-problem
    * @return the solution info of a multi-objective optimization
    */
  def getMultiObjInfo(info: IloCplex.MultiObjNumInfo, step: Int): Double =
    return cplex.getMultiObjInfo(info, step)

  /**
    * Returns the value of a CPLEX parameter.
    *
    * @param param is the CPLEX parameter
    * @return the integer value of the integer parameter
    */
  def getParam(param: IloCplex.BooleanParam): Boolean = cplex.getParam(param)

  /**
    * Returns the value of a CPLEX parameter.
    *
    * @param param is the CPLEX parameter
    * @return the integer value of the integer parameter
    */
  def getParam(param: IloCplex.IntParam): Int = cplex.getParam(param)

  /**
    * Returns the value of a CPLEX parameter.
    *
    * @param param is the CPLEX parameter
    * @return the integer value of the integer parameter
    */
  def getParam(param: IloCplex.LongParam): Long = cplex.getParam(param)

  /**
    * Returns the value of a CPLEX parameter.
    *
    * @param param is the CPLEX parameter
    * @return the double value of the integer parameter
    */
  def getParam(param : IloCplex.DoubleParam): Double = cplex.getParam(param)

  /**
    * Returns the value of a CPLEX parameter.
    *
    * @param param is the CPLEX parameter
    * @return the value of the string parameter
    */
  def getParam(param: IloCplex.StringParam): String = cplex.getParam(param)

  /**
    * Sets the value of a CPLEX parameter
    *
    * @param param is the boolean parameter
    * @param value is the value of the parameter
    */
  def setParam(param: IloCplex.BooleanParam, value: Boolean) = cplex.setParam(param, value)

  /**
    * Sets the value of a CPLEX parameter
    *
    * @param param is the integer parameter
    * @param value is the value of the parameter
    */
  def setParam(param: IloCplex.IntParam, value: Int) = cplex.setParam(param, value)

  /**
    * Sets the value of a CPLEX parameter
    *
    * @param param is the integer parameter
    * @param value is the value of the parameter
    */
  def setParam(param: IloCplex.LongParam, value: Long) = cplex.setParam(param, value)

  /**
    * Sets the value of a CPLEX parameter
    *
    * @param param is the double parameter
    * @param value is the value of the parameter
    */
  def setParam(param: IloCplex.DoubleParam, value: Double) = cplex.setParam(param, value)

  /**
    * Sets the value of a CPLEX parameter
    *
    * @param param is the string parameter
    * @param value is the value of the parameter
    */
  def setParam(param: IloCplex.StringParam, value: String) = cplex.setParam(param, value)

  /**
    * Writes the active model to a file named name. The file format is determined by the extension of the filename.
    * The following extensions are recognized on most platforms:
    * <ul>
    *   <li>*.sav</li>
    *   <li>*.mps</li>
    *   <li>*.lp</li>
    *   <li>*.sav.gz</li>
    *   <li>*.mps.gz</li>
    *   <li>*.lp.gz</li>
    * </ul>
    *
    * Microsoft Windows does not support writing to gzipped files. If no name has been assigned to a variable or
    * range (that is, getName returns null for that variable or range), IloCplex uses a default name when
    * writing the model (or in the optimization log). Default names are of the form IloXj for variables and
    * IloC i, where i and j are internal indices of IloCplex.
    *
    * @param name is the name of the file to which the model is written. The extension of the filename determines the format in which to write the model file.
    */
  def exportModel(name: String) = cplex.exportModel(name)

  /**
    * Releases the IloCplex license held by the invoking object, and all the memory allocated by it. When you no
    * longer use an instance of IloCplex and any Concert model created with it, you should call the method end to
    * release the license. After a call of the method end, the invoking IloCplex object and all objects that have
    * been created with it (such as variables and constraints) may no longer be used. Attempting to use them
    * subsequently will cause the exception IloCplex.CplexEndedException to be thrown.
    */
  def end() = cplex.end()
}

object MpModel {

  //
  // Types definition in case we need to encapsulate CPO types later
  //

  type CplexMultiCriterionExpr = IloCplexMultiCriterionExpr
  type ParameterSet = IloCplex.ParameterSet

  /**
    * Create and return a new mathematical programming model.
    *
    * @param name is the name of the model
    * @return a mathematical programming model
    */
  def apply(name: String=null) = new MpModel(name)

  /**
    * Creates and returns an integer linear expression initialized as a constant.
    *
    * @param value is the constant
    * @return
    */
  def linearIntExpr(value: Int = 0)(implicit model: MpModel): IntExpr = model.linearIntExpr(value)

//  /**
//    * Creates and returns an objective object to minimize the expression <em>expr</em>.
//    *
//    * @param expr is the expression to minimize
//    * @return An objective object representing the objective to minimize
//    */
//  def minimize(expr: NumExpr)(implicit model: MpModel): Objective = model.minimize(expr)
//
//  /**
//    * Creates and returns an objective object to maximize the expression <em>expr</em>.
//    *
//    * @param expr is the expression to minimize
//    * @return An objective object representing the objective to maximize
//    */
//  def maximize(expr: NumExpr)(implicit model: MpModel): Objective = model.maximize(expr)
//
  /**
    * This function defines a multi-criteria expression for lexicographic ordering. A lexicographic ordering means that
    * any improvement of the i-th criterion is more important than any improvement of the subsequent criteria.
    *
    * @param exprs a set of numeric expressions for the lexicographic ordering
    */
  def staticLex(exprs: NumExpr*)(implicit model: MpModel): CplexMultiCriterionExpr = model.staticLex(exprs: _*)

  /**
    * This function defines a multi-criteria expression for lexicographic ordering. A lexicographic ordering means that
    * any improvement of the i-th criterion is more important than any improvement of the subsequent criteria.
    *
    * @param exprs a set of numeric expressions for the lexicographic ordering
    * @param name is the name of the multi-criteria expression
    * @return
    */
  def staticLex(exprs: NumExprArray, name: String)(implicit model: MpModel): CplexMultiCriterionExpr =
    model.staticLex (exprs, name)

  /**
    * This function defines a multi-criteria expression for lexicographic ordering. A lexicographic ordering means that
    * any improvement of the i-th criterion is more important than any improvement of the subsequent criteria.
    *
    * @param exprs a set of numeric expressions for the lexicographic ordering
    * @return
    *
    */
  def staticLex(exprs: NumExprArray)(implicit model: MpModel): CplexMultiCriterionExpr =  model.staticLex(exprs)

  /**
    * This function defines a multi-criteria expression for lexicographic ordering. A lexicographic ordering means that
    * any improvement of the i-th criterion is more important than any improvement of the subsequent criteria.
    *
    * @param exprs a set of numeric expressions for the lexicographic ordering
    */
  def staticLex(exprs: Array[NumExpr], name: String =null)(implicit model: MpModel): CplexMultiCriterionExpr =
    model.staticLex(exprs, name)

  /**
    * This function defines a multi-criteria expression for lexicographic ordering. A lexicographic ordering means that
    * any improvement of the i-th criterion is more important than any improvement of the subsequent criteria.
    *
    * @param exprs a set of numeric expressions for the lexicographic ordering
    * @param weights the weight of each criterion
    * @param priorities the priority of each criterion
    * @param absTols the absolute tolerance of each criterion
    * @param relTols the relative tolerance of each criterion
    * @param name the name of the multi-criteria expression
    * @return
    */
  def staticLex(exprs: Array[NumExpr],
                weights: Array[Double],
                priorities: Array[Int],
                absTols: Array[Double],
                relTols: Array[Double],
                name: String)(implicit model: MpModel): CplexMultiCriterionExpr =
    model.staticLex(exprs, weights, priorities, absTols, relTols, name)

  /**
    * This function defines a multi-criteria expression for lexicographic ordering. A lexicographic ordering means that
    * any improvement of the i-th criterion is more important than any improvement of the subsequent criteria.
    *
    * @param exprs a set of numeric expressions for the lexicographic ordering
    * @param weights the weight of each criterion
    * @param priorities the priority of each criterion
    * @param absTols the absolute tolerance of each criterion
    * @param relTols the relative tolerance of each criterion
    * @return
    */
  def staticLex(exprs: Array[NumExpr],
                weights: Array[Double],
                priorities: Array[Int],
                absTols: Array[Double],
                relTols: Array[Double])(implicit model: MpModel): CplexMultiCriterionExpr =
    model.staticLex(exprs, weights, priorities, absTols, relTols)

}