/*
 * Source file provided under Apache License, Version 2.0, January 2004,
 * http://www.apache.org/licenses/
 * (c) Copyright DecisionBrain SAS 2016,2018
 */

package com.decisionbrain.cplex.mp

import com.decisionbrain.cplex.Addable
import com.decisionbrain.cplex.mp.MpModel._
import com.decisionbrain.cplex.mp.NumExpr.{LinearIntExpr, LinearNumExpr}
import ilog.concert._
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
class MpModel(name: String=null) {

  val cplex = new IloCplex()

  private val numExprNumeric: Numeric[NumExpr] = NumExprNumeric(this)

  /**
    * Returns the numeric for numeric expressions. This allows to do things such as calling method sum on list of
    * numeric expressions e.g.
    * <pre>
    *   <code>
    *     implicit val num = model.getNumExprNumeric()
    *     val exprs = List(model.numVar(), model.numVar())
    *     val sumExpr = exprs.sum
    *   </code>
    * </pre>
    * @return the numeric for numeric expression
    */
  def getNumExprNumeric(): Numeric[NumExpr] = numExprNumeric

  /**
    * Returns the name of the mathematical programming model.
    *
    * @return the name of the model
    */
  def getName(): Option[String] = Option(name)

  /**
    * Return the value of a numeric expression in the solution.
    *
    * @param expr is the numeric expression; the expression must be in the active model
    * @return the value of the expression in the solution
    */
  def getValue(expr: NumExpr) : Double = cplex.getValue(expr.expr)

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
    * Create a numeric variable.
    *
    * @param lb is the lower bound
    * @param ub is the upper bound
    * @param name is the name of the variable
    * @return a numeric variable
    */
  def numVar(lb: Double=0.0, ub: Double=Double.MaxValue, name: String=null): NumVar =
    NumVar(cplex.numVar(lb, ub, name))(implicitly(this))

  /**
    * Create an integer variable.
    *
    * @param lb is the lower bound
    * @param ub is the upper bound
    * @param name is the name of the variable
    * @return a numeric variable
    */
  def intVar(lb: Int=0, ub: Int=Int.MaxValue, name: String=null): NumVar =
    NumVar(cplex.intVar(lb, ub, name))(implicitly(this))

  /**
    * Create a boolean variable.
    *
    * @param name is the name of the variable
    * @return a numeric variable
    */
  def boolVar(name: String=null): NumVar =
    NumVar(cplex.boolVar(name))(implicitly(this))

  /**
    * Creates and returns an linear expression initialized with zero.
    *
    * @return
    */
  def linearNumExpr(): LinearNumExpr = new LinearNumExpr(.0)(implicitly(this))

  /**
    * Creates and returns a linear expression initialized as a constant.
    *
    * @param value is the constant
    * @return
    */
  def linearNumExpr(value: Double): LinearNumExpr = new LinearNumExpr(value)(implicitly(this))

  /**
    * Creates and returns an integer linear expression initialized as a constant.
    *
    * @param value is the constant
    * @return
    */
  def linearIntExpr(value: Int = 0): LinearIntExpr = new LinearIntExpr(value)(implicitly(this))

  /**
    * Create a numeric variable for each element in the set and add it in a dictionary
    * where the key is element of the set and the value is the numeric variable.
    *
    * @param set is the set
    * @param lb is the lowver bound
    * @param ub is the upper bound
    * @param namer is a function that is used to set the name of a numeric variable
    * @tparam T it the type of the elements in the set
    * @return a dictionary of numeric variables indexed by the element of the set
    */
  def numVars[T](set: Iterable[T],
                 lb: Double = 0.0,
                 ub: Double = Double.MaxValue,
                 namer: (T) => String = (t: T) => "") : Map[T, NumVar] = {
    val dict: Map[T, NumVar] = set.map(t => {
      val v: NumVar = NumVar(cplex.numVar(lb, ub, namer(t)))(implicitly(this))
      (t, v)
    })(collection.breakOut)
    dict
  }

  /**
    * Creates and returns a map of binary variables.
    *
    * @param keys is an iterable representing the keys
    * @param namer is a function that is used to give a name to the variables
    * @return a map of binary variables
    */
  def boolVars[T](keys: Iterable[T], namer: (T) => String) : Map[T, NumVar] = {
    (for (t <- keys) yield {
      val v: NumVar = NumVar(cplex.boolVar())(implicitly(this))
      v.setName(namer(t))
      t -> v
    })(collection.breakOut)
  }

  /**
    * Creates and returns a map of binary variables.
    *
    * @param keys is an iterable representing the keys
    * @return a map of binary variables
    */
  def boolVars[T](keys: Iterable[T]) : Map[T, NumVar] = {
    boolVars(keys, (t: T) => "")
  }

  /**
    * Creates and returns a matrix of binary variables.
    *
    * @param keys1 is an iterable representing the rows
    * @param keys2 is an iterable representing the columns
    * @param namer is a function that is used to give a name to the variables
    * @return a map of binary variables where the key is a tuple (key1, key2)
    */
  def boolVars[T, U](keys1: Iterable[T], keys2: Iterable[U], namer: (T, U) => String) : Map[(T,U), NumVar] = {
    (for (t <- keys1; u <- keys2) yield {
      val v: NumVar = NumVar(cplex.boolVar())(implicitly(this))
      v.setName(namer(t, u))
      (t,u) -> v
    })(collection.breakOut)
  }

  /**
    * Return the sum of numeric expressions.
    *
    * @param exprs is a sequence of numeric expressions
    * @return a numeric expression that represents the sum of numeric expressions
    */
  def sum(exprs: NumExpr*): NumExpr = {
    NumExpr(cplex.sum(exprs.map(e => e.getIloNumExpr).toArray))(implicitly(this))
  }

  /**
    * Return the sum of numeric expressions.
    *
    * @param exprs is a sequence of numeric variables
    * @return a numeric expression that represents the sum of the numeric expressions
    */
  def sum(exprs: Iterable[NumExpr]) : NumExpr = {
    NumExpr(cplex.sum(exprs.map(e => e.getIloNumExpr).toArray))(implicitly(this))
  }

  /**
    * Return a range that represent the constraint <em>expr >= rhs<em>.
    *
    * @param expr is the numeric expression of the new greater-than-or-equal-to constraint
    * @param rhs is the upper bound of the new greater-than-or-equal-to constraint
    * @return a new range constraint <em>expr >= rhs</em>
    */
  def ge(expr: NumExpr, rhs: Double, name: String=null): RangeConstraint = {
    RangeConstraint(cplex.ge(expr.getIloNumExpr, rhs, name))(implicitly(this))
  }

  /**
    * Return a range constraint <em>expr == value<em>.
    *
    * @param expr is the numeric expression
    * @param value is the value
    * @return a range constraint
    */
  def eq(expr: NumExpr, value: Double): RangeConstraint = {
    eq(expr, value, null)
  }

  /**
    * Return a range constraint <em>expr == value<em>.
    *
    * @param expr is the numeric expression
    * @param value is the value
    * @param name is the name for the range constraint
    * @return a range constraint
    */
  def eq(expr: NumExpr, value: Double, name: String): RangeConstraint = {
    RangeConstraint(cplex.eq(expr.getIloNumExpr, value, name))(implicitly(this))
  }

  /**
    * Return a range constraint <em>expr1 == expr2<em>.
    *
    * @param expr1 is the first numeric expression
    * @param expr2 is the second numeric expression
    * @return a constraint
    */
  def eq(expr1: NumExpr, expr2: NumExpr): Constraint = {
    eq(expr1, expr2, null)
  }

  /**
    * Return a range constraint <em>expr1 == expr2<em>.
    *
    * @param expr1 is the first numeric expression
    * @param expr2 is the second numeric expression
    * @param name is the name of the range constraint
    * @return a constraint
    */
  def eq(expr1: NumExpr, expr2: NumExpr, name: String): Constraint = {
    Constraint(cplex.eq(expr1.getIloNumExpr, expr2.getIloNumExpr, name))(implicitly(this))
  }

  /**
    * Return a range that represent the constraint <em>expr >= rhs<em>.
    *
    * @param expr is the numeric expression of the new greater-than-or-equal-to constraint
    * @param rhs is the upper bound of the new greater-than-or-equal-to constraint
    * @return a range constraint <em>expr >= rhs</em>
    */
  def le(expr: NumExpr, rhs: Double, name: String=null): RangeConstraint = {
    RangeConstraint(cplex.le(expr.getIloNumExpr, rhs, name))(implicitly(this))
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
  def range(lb: Double, expr: NumExpr, ub: Double, name: String=null): RangeConstraint = {
    RangeConstraint(cplex.range(lb, expr.getIloNumExpr, ub, name))(implicitly(this))
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
  }

  /**
    * Add an addable object in the model.
    *
    * @param a is the object to add to the model
    * @return the model
    */
  def add(a: Addable, name: String=null): MpModel = {
    a.setName(name)
    cplex.add(a.getIloAddable())
    this
  }

  /**
    * Creates and returns an objective object to minimize the expression <em>expr</em>.
    *
    * @param expr is the expression to minimize
    * @return An objective object representing the objective to minimize
    */
  def minimize(expr: NumExpr): Objective = Objective(cplex.minimize(expr.getIloNumExpr))(implicitly(this))

  /**
    * Creates and return a minimization multi-criteria objective.
    *
    * @param expr is the multi-criteria expressions
    * @return an objective
    */
  def minimize(expr: MultiCriterionExpr): Objective = Objective(cplex.minimize(expr))(implicitly(this))

  /**
    * Creates and returns an objective object to maximize the expression <em>expr</em>.
    *
    * @param expr is the expression to maximize
    * @return An objective object the objective to maximize
    */
  def maximize(expr: NumExpr): Objective = Objective(cplex.maximize(expr.getIloNumExpr))(implicitly(this))

  /**
    * Creates and return a maximization multi-criteria objective.
    *
    * @param expr is the multi-criteria expressions
    * @return an objective
    */
  def maximize(expr: MultiCriterionExpr): Objective = Objective(cplex.maximize(expr))(implicitly(this))

  /**
    * This function defines a multi-criteria expression for lexicographic ordering. A lexicographic ordering means that
    * any improvement of the i-th criterion is more important than any improvement of the subsequent criteria.
    *
    * @param exprs a set of numeric expressions for the lexicographic ordering
    */
  def staticLex(exprs: NumExpr*): MultiCriterionExpr = {
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
  def staticLex(exprs: NumExprArray, name: String): MultiCriterionExpr = {
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
  def staticLex(exprs: NumExprArray): MultiCriterionExpr = {
    cplex.staticLex(exprs.toIloArray)
  }

  /**
    * This function defines a multi-criteria expression for lexicographic ordering. A lexicographic ordering means that
    * any improvement of the i-th criterion is more important than any improvement of the subsequent criteria.
    *
    * @param exprs a set of numeric expressions for the lexicographic ordering
    */
  def staticLex(exprs: Array[NumExpr], name: String =null): MultiCriterionExpr = {
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
                name: String): MultiCriterionExpr = {
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
                relTols: Array[Double]): MultiCriterionExpr = {
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

  type MultiCriterionExpr = IloCplexMultiCriterionExpr
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
  def linearIntExpr(value: Int = 0)(implicit model: MpModel): LinearIntExpr = model.linearIntExpr(value)

  /**
    * Return the sum of a sequence of numeric expressions.
    *
    * @param exprs is a sequence of numeric variables
    * @return a numeric expression that represents the sum of the numeric expressions
    */
  def sum(exprs: NumExpr*)(implicit model: MpModel) : NumExpr = model.sum(exprs)

  /**
    * Return the sum of a sequence of numeric expressions.
    *
    * @param exprs is a sequence of numeric variables
    * @return a numeric expression that represents the sum of the numeric expressions
    */
  def sum(exprs: Iterable[NumExpr])(implicit model: MpModel) : NumExpr = model.sum(exprs)

  /**
    * Creates and returns an objective object to minimize the expression <em>expr</em>.
    *
    * @param expr is the expression to minimize
    * @return An objective object representing the objective to minimize
    */
  def minimize(expr: NumExpr)(implicit model: MpModel): Objective = model.minimize(expr)

  /**
    * Creates and returns an objective object to maximize the expression <em>expr</em>.
    *
    * @param expr is the expression to minimize
    * @return An objective object representing the objective to maximize
    */
  def maximize(expr: NumExpr)(implicit model: MpModel): Objective = model.maximize(expr)

  /**
    * This function defines a multi-criteria expression for lexicographic ordering. A lexicographic ordering means that
    * any improvement of the i-th criterion is more important than any improvement of the subsequent criteria.
    *
    * @param exprs a set of numeric expressions for the lexicographic ordering
    */
  def staticLex(exprs: NumExpr*)(implicit model: MpModel): MultiCriterionExpr = model.staticLex(exprs: _*)

  /**
    * This function defines a multi-criteria expression for lexicographic ordering. A lexicographic ordering means that
    * any improvement of the i-th criterion is more important than any improvement of the subsequent criteria.
    *
    * @param exprs a set of numeric expressions for the lexicographic ordering
    * @param name is the name of the multi-criteria expression
    * @return
    */
  def staticLex(exprs: NumExprArray, name: String)(implicit model: MpModel): MultiCriterionExpr =
    model.staticLex (exprs, name)

  /**
    * This function defines a multi-criteria expression for lexicographic ordering. A lexicographic ordering means that
    * any improvement of the i-th criterion is more important than any improvement of the subsequent criteria.
    *
    * @param exprs a set of numeric expressions for the lexicographic ordering
    * @return
    *
    */
  def staticLex(exprs: NumExprArray)(implicit model: MpModel): MultiCriterionExpr =  model.staticLex(exprs)

  /**
    * This function defines a multi-criteria expression for lexicographic ordering. A lexicographic ordering means that
    * any improvement of the i-th criterion is more important than any improvement of the subsequent criteria.
    *
    * @param exprs a set of numeric expressions for the lexicographic ordering
    */
  def staticLex(exprs: Array[NumExpr], name: String =null)(implicit model: MpModel): MultiCriterionExpr =
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
                name: String)(implicit model: MpModel): MultiCriterionExpr =
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
                relTols: Array[Double])(implicit model: MpModel): MultiCriterionExpr =
    model.staticLex(exprs, weights, priorities, absTols, relTols)

  /**
    * Creates and return a minimization multi-criteria objective.
    *
    * @param expr is the multi-criteria expressions
    * @return an objective
    */
  def minimize(expr: MultiCriterionExpr)(implicit model: MpModel): Objective = model.minimize(expr)

  /**
    *  Implicit conversion of set of numeric expressions: add behavior
    *
    * @param exprs are the integer expressions
    * @param model is the constraint programming model
    */
  implicit class NumExprArray(val exprs: Iterable[NumExpr])(implicit model: MpModel) {

    /**
      * Converts to scala array
      */
    def toArray: Array[NumExpr] = exprs.toArray

    /**
      * Convert to CPLEX array
      */
    def toIloArray: Array[IloNumExpr] = exprs.map(e => e.getIloNumExpr()).toArray
  }

}