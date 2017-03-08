/*
 * Source file provided under Apache License, Version 2.0, January 2004,
 * http://www.apache.org/licenses/
 * (c) Copyright DecisionBrain SAS 2016,2017
 */

package com.decisionbrain.cplex.mp

import com.decisionbrain.cplex.Addable
import com.decisionbrain.cplex.mp.NumExpr.{LinearIntExpr, LinearNumExpr}
import ilog.cplex.IloCplex


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
    * Creates and returns an integer linear expression initialized as zero.
    *
    * @return
    */
  def linearIntExpr(): LinearIntExpr = new LinearIntExpr(0)(implicitly(this))

  /**
    * Creates and returns an integer linear expression initialized as a constant.
    *
    * @param value is the constant
    * @return
    */
  def linearIntExpr(value: Int): LinearIntExpr = new LinearIntExpr(value)(implicitly(this))

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
    Constraint(cplex.eq(expr1.getIloNumExpr, expr2.getIloNumExpr, name))
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
    * Creates and returns an objective object to maximize the expression <em>expr</em>.
    *
    * @param expr is the expression to maximize
    * @return An objective object the objective to maximize
    */
  def maximize(expr: NumExpr): Objective = Objective(cplex.maximize(expr.getIloNumExpr))(implicitly(this))

  /**
    * Solves the active model.
    *
    * @return A Boolean value indicating whether a feasible solution has been found. This solution is not
    *         necessarily optimal. If <em>false</em> is returned, a feasible solution may still be present,
    *         but IloCplex has not been able to prove its feasibility.
    */
  def solve() = cplex.solve()

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

  /**
    * Create and return a new mathematical programming model.
    *
    * @param name is the name of the model
    * @return a mathematical programming model
    */
  def apply(name: String=null) = new MpModel(name)

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
}