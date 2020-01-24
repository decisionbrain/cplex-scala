/*
 * Source file provided under Apache License, Version 2.0, January 2004,
 *  http://www.apache.org/licenses/
 *  (c) Copyright DecisionBrain SAS 2016,2020
 *
 */

package com.decisionbrain.cplex.mp

import com.decisionbrain.cplex._
import com.decisionbrain.cplex.mp.MpModel._
import com.decisionbrain.cplex.Modeler._
import ilog.concert.{IloAddable, IloLPMatrix, IloModeler}
import ilog.cplex.IloCplex.{MIPInfoCallback, Param}
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
class MpModel(val name: String=null) extends Modeler {


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

  //
  // Solution
  //

  /**
   * Read the solution from a file and add it to the optimization model.
   *
   * @param filename
   */
  def readSolution(filename: String): Unit = cplex.readSolution(filename)

  /**
   * Write the current solution of the optimization model to a file.
   *
   * @param filename is the name of the file.
   */
  def writeSolution(filename: String): Unit = cplex.writeSolution(filename)

  /**
   * Write all the solutions of the optimization model to a file.
   *
   * @param filename is the name of the file.
   */
  def writeSolutions(filename: String): Unit = cplex.writeSolutions(filename)

  //
  // MIP Starts
  //

  /**
   * Adds a MIP start to the current problem.
   *
   * @param vars are the set of variables in the MIP start
   * @param values are the values of the variables in the MIP start
   * @param effort is the effort CPLEX should spend to solve the MIP start.
   * @param name is the name of the MIP start
   * @return The index of the added MIP start among all the existing ones associated with the optimization model.
   */
  def addMIPStart(vars: Iterable[NumVar],
                  values: Iterable[Double],
                  effort: MIPStartEffort  = IloCplex.MIPStartEffort.Auto,
                  name: String ): Int =
    cplex.addMIPStart(vars.map(_.getIloNumVar()).toArray, values.toArray, effort, name)

  /**
   * Deletes the MIP start designated by its index.
   * @param index is the index of the MIP start to remove from the optimization model
   */
  def deleteMIPStart(index: Int): Unit = cplex.deleteMIPStarts(index)

  /**
   * Changes the MIP start with the specified index in the current optimization model by substituting the corresponding
   * values of the designated variables and associates a level of effort.
   *
   * @param index is the index of the MIP start to change
   * @param vars are the variables to change in the MIP start
   * @param values are the new values of the variables in the MIP start
   * @param effort is the effort CPLEX should spend to solve the modified MIP start
   */
  def changeMIPStart(index: Int,
                     vars: Iterable[NumVar],
                     values: Iterable[Double],
                     effort: MIPStartEffort): Unit =
    cplex.changeMIPStart(index, vars.map(_.getIloNumVar).toArray, values.toArray, effort)
  /**
   * Write all the MIP start of the optimization model to a file.
   *
   * @param filename is the name of the file
   */
  def writeMIPStarts(filename: String): Unit = cplex.writeMIPStarts(filename)

  /**
   * Read the MIP start from a file and add it to the optimization model
   *
   * @param filename is the name of the file
   */
  def readMIPStarts(filename: String): Unit = cplex.readMIPStarts(filename)

  //
  // Aborter
  //

  /**
   * Specifies the aborter to be used by the optimization model to control termination of its solving and tuning methods.
   *
   * @param aborter is the aborter
   * @return the aborter used by the optimization model
   */
  def use(aborter: Aborter): Aborter = cplex.use(aborter)

  /**
   * Returns the aborter currently used by the optimizaiton model.
   *
   * @return the aborter currently used by the optimization model
   */
  def getAborter(): Aborter = cplex.getAborter

  /**
   * Removes the aborter from the optimization model
   * @param aborter
   */
  def remove(aborter: Aborter) = cplex.remove(aborter)

  //
  // Piecewise Linear Function
  //

  /**
   * Returns a new piecewise linear functions.
   *
   * @param preslope is the slope of the first segment of the piecewise linear function
   * @param points is the set of points of the piecewise linear function
   * @param postslope is the slope of the last segment of the piecewise linear function
   * @return a new piecewise linear function
   */
  def piecewiseLinear(preslope: Double, points: Iterable[(Double, Double)], postslope: Double): PiecewiseLinearFunction = {
    PiecewiseLinearFunction(preslope, points, postslope)(implicitly(this))
  }

  /**
    * Return the value of a numeric expression in the solution.
    *
    * @param expr is the numeric expression; the expression must be in the active model
    * @return the value of the expression in the solution
    */
  def getValue(expr: NumExpr) : Double = cplex.getValue(expr.getIloNumExpr())

  /**
    * Return the values of a numeric expressions in the solution.
    *
    * @param exprs are the numeric expressions; the expressions must be in the active model
    * @return the valued of the expressions in the solution
    */
  def getValues(exprs: Array[NumVar]) : Array[Double] = cplex.getValues(exprs.map(e => e.getIloNumVar()))

  /**
    * Return the values of a numeric expressions in the solution.
    *
    * @param exprs are the numeric expressions; the expressions must be in the active model
    * @return the valued of the expressions in the solution
    */
  def getValues(exprs: Iterable[NumVar]) : Array[Double] = cplex.getValues(exprs.map(e => e.getIloNumVar()).toArray)

  /**
    * Returns the reduced cost of a numeric variable.
    *
    * @param v is the numeric variable
    * @return the reduced cost of the numeric variable
    */
  def getReducedCost(v: NumVar): Double = cplex.getReducedCost(v.getIloNumVar())

  /**
    * Returns the reduced cost of numeric variables.
    *
    * @param vars are the numeric variables
    * @return the reduced cost of the numeric variables
    */
  def getReducedCosts(vars: Array[NumVar]): Array[Double] = cplex.getReducedCosts(vars.map(v => v.getIloNumVar()))

  /**
    * Returns the reduced cost of numeric variables.
    *
    * @param vars are the numeric variables
    * @return the reduced cost of the numeric variables
    */
  def getReducedCosts(vars: Iterable[NumVar]): Array[Double] = cplex.getReducedCosts(vars.map(v => v.getIloNumVar()).toArray)

  /**
    * Returns the dual value of a range.
    *
    * @param r is the range
    * @return the dual value of the range
    */
  def getDual(r: Range): Double = cplex.getDual(r.getIloRange())

  /**
    * Returns the dual values of ranges.
    *
    * @param ranges are the ranges
    * @return the dual value of the ranges
    */
  def getDuals(ranges: Array[Range]): Array[Double] = cplex.getDuals(ranges.map(r => r.getIloRange()))

  /**
    * Returns the dual values of ranges.
    *
    * @param ranges are the ranges
    * @return the dual value of the ranges
    */
  def getDuals(ranges: Iterable[Range]): Array[Double] = cplex.getDuals(ranges.map(r => r.getIloRange()).toArray)

  /**
    * Get the slack of a range.
    *
    * @param r is the range
    * @return the slack of the range
    */
  def getSlack(r: Range): Double = cplex.getSlack(r.getIloRange())

  /**
    * Get the slack of a range.
    *
    * @param ranges is the range
    * @return the slack of the range
    */
  def getSlacks(ranges: Array[Range]): Array[Double] = cplex.getSlacks(ranges.map(r => r.getIloRange()))

  /**
    * Get the slack of a range.
    *
    * @param ranges is the range
    * @return the slack of the range
    */
  def getSlacks(ranges: Iterable[Range]): Array[Double] = cplex.getSlacks(ranges.map(r => r.getIloRange()).toArray)

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
   * Returns the set of numeric variables in the mathematical model.
   *
   * @return an array of numeric variables
   */
  def getNumVars() : Array[NumVar] = {
    val lp = cplex.LPMatrixIterator.next.asInstanceOf[IloLPMatrix]
    val vars = lp.getNumVars().map(v => NumVar(v)(implicitly(this)))
    vars
  }

  /**
   * Returns the set of range constraints in the mathematical model.
   *
   * @return an array of range constraints
   */
  def getRanges(): Array[Range] = {
    val lp = cplex.LPMatrixIterator.next.asInstanceOf[IloLPMatrix]
    val ranges  = lp.getRanges().map(r => Range(r)(implicitly(this)))
    ranges
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
  def getObjValue() = cplex.getObjValue

  @deprecated("This method has been replaced by method getObjValue", "decisionbrain-cplex-scala-1.7.0")
  def getObjectiveValue() = getObjValue

  /**
    * Returns the objective value of the best remaining node.
    *
    * @return the objective value of the best remaining node
    */
  def getBestObjValue() = cplex.getBestObjValue

  /**
   * Returns the relative gap.
   *
   * @return the relative gap
   */
  def getMIPRelativeGap(): Double = cplex.getMIPRelativeGap

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

  //
  // Annotations
  //

  /**
   * Creates a new annotation of type long.
   *
   * @param annotation is the name of the annotation
   * @param value is the value of the annotation
   * @return
   */
  def newLongAnnotation(annotation: String, value: Long = 0): IloCplex.LongAnnotation =
    cplex.newLongAnnotation(annotation, value)

  /**
   * Returns the annotation value for an instance of IloNumVar.
   *
   * @param annotation is the annotation
   * @param v is the numeric variable
   * @return the annotation value
   */
  def getAnnotation(annotation: LongAnnotation, v: NumVar): Long = cplex.getAnnotation(annotation, v.getIloNumVar())

  /**
   * Returns the annotation value for an instance of IloNumVar.
   *
   * @param annotation is the annotation
   * @param v is the numeric variable
   * @return the annotation value
   */
  def getAnnotation(annotation: LongAnnotation, v: IntVar): Long = cplex.getAnnotation(annotation, v.getIloIntVar())

  /**
   * Returns the annotation value for an objective function.
   *
   * @param annotation is the annotation
   * @param o is the objective
   * @return the annotation value
   */
  def getAnnotation(annotation: LongAnnotation, o: Objective): Long = cplex.getAnnotation(annotation, o.getIloObjective())

  /**
   * Returns the annotation value for a constraint.
   *
   * @param annotation is the annotation
   * @param c is the constraint
   * @return the annotation value
   */
  def getAnnotation(annotation: LongAnnotation, c: Constraint): Long = cplex.getAnnotation(annotation, c.getIloConstraint())

  /**
   * Returns the annotation values of an array of objects
   *
   * @param annotation is the annotation
   * @param items are the objects for which the annotation values are requested
   * @return the annotation values
   */
  def getAnnotations(annotation: LongAnnotation, items: Iterable[Addable]): Array[Long] =
    cplex.getAnnotation(annotation, items.map(a => a.getIloAddable()).toArray[IloAddable])

  /**
   * Sets the annotation value for a numeric variable
   *
   * @param annotation is the annotation
   * @param v is the numeric variable
   * @param value is the value to set
   */
  def setAnnotation(annotation: LongAnnotation, v: NumVar, value: Int): Unit = cplex.setAnnotation(annotation, v.getIloNumVar(), value)

  /**
   * Sets the annotation value for an integer variable
   *
   * @param annotation is the annotation
   * @param v is the integer variable
   * @param value is the value to set
   */
  def setAnnotation(annotation: LongAnnotation, v: IntVar, value: Int): Unit = cplex.setAnnotation(annotation, v.getIloIntVar(), value)

  /**
   * Sets the annotation value for an objective function
   *
   * @param annotation is the annotation
   * @param o is the objective
   * @param value is the value to set
   */
  def setAnnotation(annotation: LongAnnotation, o: Objective, value: Int): Unit = cplex.setAnnotation(annotation, o.getIloObjective(), value)

  /**
   * Sets the annotation value for a constraint
   *
   * @param annotation is the annotation
   * @param c is the constraint
   * @param value is the value to set
   */
  def setAnnotation(annotation: LongAnnotation, c: Constraint, value: Long): Unit = cplex.setAnnotation(annotation, c.getIloConstraint(), value)

  /**
   * Returns the annotation values of an array of objects
   *
   * @param annotation is the annotation
   * @param items are the objects to set annotation values
   */
  def setAnnotations(annotation: LongAnnotation, items: Iterable[Addable], values: Iterable[Long]): Unit =
    cplex.setAnnotation(annotation, items.map(a => a.getIloAddable()).toArray[IloAddable], values.toArray)

  /**
   *
   * Reads annotations from a file.
   *
   * See the topic Annotating a model for CPLEX in the CPLEX User's Manual for a sample of the header of an annotation
   * file. See the sample UFL_25_35_1 distributed with the product for an example of an annotated model.
   * Important: CPLEX deletes existing annotations before CPLEX attempts to read filename. Consequently, all currently
   * existing annotations are deleted even if reading fails.
   *
   * Because indicator constraints are not top-level modeling objects in Concert, this method ignores annotations for
   * indicator constraints in filename. More specifically, upon input, this method resets the annotation for any
   * indicator constraint to its default value.
   *
   * @param filename is the name of the file
   */
  def readAnnotations(filename: String): Unit = cplex.readAnnotations(filename)

  /**
   * Writes all annotations to a file.
   *
   * This method writes the annotations of all types currently stored in this instance to filename.
   *
   * See the topic Annotating a model for CPLEX in the CPLEX User's Manual for a sample of the header of an annotation
   * file. See the sample UFL_25_35_1 distributed with the product for an example of an annotated model.
   *
   * @param filename is the name of the file
   */

  def writeAnnotations(filename: String): Unit = cplex.writeAnnotations(filename)
  /**
   * Writes the annotation that CPLEX automatically generates for a Benders decomposition to the specified file.
   *
   * @param filename is the name of the file
   */
  def writeBendersAnnotation(filename: String): Unit = cplex.writeBendersAnnotation(filename)

  //
  // Import & Export
  //

  /**
    * Writes the active model to a file named <em>name</em>. The file format is determined by the extension of the filename.
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
    * @param filename is the name of the file to which the model is written. The extension of the filename determines
   *                 the format in which to write the model file.
    */
  def exportModel(filename: String) = cplex.exportModel(filename)

  /**
   * Reads a mathematical programming model from the file specified by <em>filename</em> into the active model.
   * The format of the file is specified by the extension of the filename. The following extensions are recognized:
   *
   * <ul>
   *   <li>.sav</li>
   *   <li>.mps</li>
   *   <li>.lp</li>
   *   <li>.sav.gz</li>
   *   <li>.mps.gz</li>
   *   <li>.lp.gz</li>
   *   <li>.bz2</li>
   * </ul>
   *
   * When CPLEX reads a file, the existing active model is first cleaned out and then new modeling objects, as
   * specified by the input file, are added to it. In particular, one IloObjective object and one IloLPMatrix object
   * are always added to the active model. The IloLPMatrix object will contain all the constraints of the imported
   * model. IloSOS1 and IloSOS2 objects are added as needed.
   *
   * @param filename
   */
  def importModel(filename: String): Unit = cplex.importModel(filename)

  //
  // KPI
  //

  /**
   * Add a named key performance indicator (KPI) i.e.  a value which can be associated with a solution which represents
   * an interesting measure of some aspect of the solution.
   *
   * @param expr is the KPI
   * @param name is the name of the KPI
   * @return
   */
  def addKPI(expr: NumExpr, name: String = null) : Unit =
  // TODO: unfortunately CPLEX MIP does not have this KPI.
  // cplex.addKPI(expr.getIloNumExpr(), name)
    None

  //
  // User defined callbacks
  //

  /**
   * Add a MIP Info call back.
   *
   * @param callback is the function that is called by CPLEX
   * @return the optimization model
   */
  def use(callback: (MIPInfo) => Unit): MIPInfoCallback = {
    val mipInfoCallback = new MIPInfo {
      override def main(): Unit = callback(this)
    }
    cplex.use(mipInfoCallback)
    mipInfoCallback
  }

  /**
   * Remove a user defined CPLEX callback.
   *
   * @param callback is the user defined CPLEX callback to remove
   */
  def remove(callback: IloCplex.Callback) = cplex.remove(callback)

  /**
   * Remove all user defined CPLEX callbacks.
   */
  def clearCallbacks() = cplex.clearCallbacks()

  //
  // This is the End (in reference to the Doors)
  //

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
  type LongAnnotation = IloCplex.LongAnnotation
  type DoubleAnnotation = IloCplex.DoubleAnnotation
  type Aborter = IloCplex.Aborter
  type MIPStartEffort = IloCplex.MIPStartEffort

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

/**
 * This class allows to access to the CPLEX MIP information. An instance of this class is given as argument to the MIP
 * information callback.
 *
 * @see MpModel.addMIPCallback
 */
abstract class MIPInfo extends MIPInfoCallback {
  override def hasIncumbent: Boolean = super.hasIncumbent
  override def getIncumbentObjValue: Double = super.getIncumbentObjValue
  override def getBestObjValue: Double = super.getBestObjValue
  override def getCplexTime: Double = super.getCplexTime
  override def getMIPRelativeGap: Double = super.getMIPRelativeGap
  override def getNnodes: Int = super.getNnodes
  override def getNnodes64: Long = super.getNnodes64
  override def getNremainingNodes: Int = super.getNremainingNodes
  override def getNremainingNodes64: Long = super.getNremainingNodes64
  override def getNiterations: Int = super.getNiterations
  override def getNiterations64: Long = super.getNiterations64
}

