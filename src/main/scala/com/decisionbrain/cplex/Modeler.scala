/*
 *  Source file provided under Apache License, Version 2.0, January 2004,
 *  http://www.apache.org/licenses/
 *  (c) Copyright DecisionBrain SAS 2016,2019
 */

package com.decisionbrain.cplex

import com.decisionbrain.cplex.Modeler._
import com.decisionbrain.cplex.cp.CpModel
import ilog.concert._

import scala.reflect.ClassTag

/**
  * This class is for building optimization models and provides methods for constructing variables, expressions,
  * constraints and objectives.
  *
  */
abstract class Modeler {

  private def modeler = getIloModeler()

  /**
    * Returns the CPLEX modeler i.e the interface for building optimization models.
    *
    * @return the CPLEX modeler
    */
  def getIloModeler(): IloModeler

  /**
    * Returns the name of the optimization model
    *
    * @return the name of the optimization model
    */
  def getName(): Option[String]

  /**
    * Create a numeric variable.
    *
    * @param lb is the lower bound
    * @param ub is the upper bound
    * @param name is the name of the variable
    * @return a numeric variable
    */
  def numVar(lb: Double=0.0, ub: Double=Double.MaxValue, name: String=null): NumVar =
    NumVar(modeler.numVar(lb, ub, name))(implicitly(this))


  /**
   * Create a numeric variable for each element in the set and add it in a dictionary
   * where the key is element of the set and the value is the numeric variable.
   *
   * @param set is the set
   * @param lb is the lowver bound
   * @param ub is the upper bound
   * @param defaultValue is the numeric variable returns if the key is not found in the map
   * @param namer is a function that is used to set the name of a numeric variable
   * @tparam T it the type of the elements in the set
   * @return a dictionary of numeric variables indexed by the element of the set
   */
  def numVars[T](set: Iterable[T],
                 lb: Double,
                 ub: Double,
                 defaultValue: Option[NumVar],
                 namer: (T) => String) : Map[T, NumVar] = {
    val dict = set.map(t => {
      val v: NumVar = NumVar(modeler.numVar(lb, ub, namer(t)))(implicitly(this))
      (t, v)
    })
    if (defaultValue.isDefined)
      dict.toMap.withDefaultValue(defaultValue.get)
    else
      dict.toMap
  }

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
    numVars(set, lb, ub, None, namer)
  }

  /**
    * Create an integer variable.
    *
    * @param min is the lower bound
    * @param max is the upper bound
    * @param name is the name of the variable
    * @return a numeric variable
    */
  def intVar(min: Int=0, max: Int=Int.MaxValue, name: String=null): IntVar =
    IntVar(modeler.intVar(min, max, name))(implicitly(this))

  /**
   * Create a numeric variable for each element in the set and add it in a dictionary
   * where the key is element of the set and the value is the numeric variable.
   *
   * @param set is the set
   * @param min is the minimum value
   * @param max is the maximum value
   * @param defaultValue is the numeric variable return if the key is not found
   * @param namer is a function that is used to set the name of a numeric variable
   * @tparam T it the type of the elements in the set
   * @return a dictionary of numeric variables indexed by the element of the set
   */
  def intVars[T](set: Iterable[T],
                 min: Int,
                 max: Int,
                 defaultValue: Option[IntVar],
                 namer: (T) => String) : Map[T, IntVar] = {
    val dict = set.map(t => {
      val v: IntVar = IntVar(modeler.intVar(min, max, namer(t)))(implicitly(this))
      (t, v)
    })
    if (defaultValue.isDefined)
      dict.toMap.withDefaultValue(defaultValue.get)
    else
      dict.toMap
  }

  /**
    * Create a numeric variable for each element in the set and add it in a dictionary
    * where the key is element of the set and the value is the numeric variable.
    *
    * @param set is the set
    * @param min is the minimum value
    * @param max is the maximum value
    * @param namer is a function that is used to set the name of a numeric variable
    * @tparam T it the type of the elements in the set
    * @return a dictionary of numeric variables indexed by the element of the set
    */
  def intVars[T](set: Iterable[T],
                 min: Int = 0,
                 max: Int= Int.MaxValue,
                 namer: (T) => String = (t: T) => "") : Map[T, IntVar] = {
    intVars(set, min, max, None, namer)
  }

  /**
    * Creates and returns a list of integer variables.
    *
    * @param count is the number of integer variables to be created
    * @param min is the minimum value of the integer variables
    * @param max is the maximum value of the integer variables
    * @param namer is a function to set the name of the integer variables
    * @return a list of integer variables
    */
  def intVars(count: Int,
              min: Int,
              max: Int,
              namer: (Int) => String) : List[IntVar] = {
    val vars = for (i <- 0 until count)
      yield IntVar(modeler.intVar(min, max, namer(i)))(implicitly(this))
    vars.toList
  }

  /**
    * Creates and returns a list of integer variables.
    *
    * @param count is the number of integer variables to be created
    * @param min is the minimum value of the integer variables
    * @param max is the maximum value of the integer variables
    * @return a list of integer variables
    */
  def intVars(count: Int,
              min: Int,
              max: Int) : List[IntVar] = {
    val vars = for (i <- 0 until count)
      yield IntVar(modeler.intVar(min, max))(implicitly(this))
    vars.toList
  }

  /**
    * Create a boolean variable.
    *
    * @param name is the name of the variable
    * @return a numeric variable
    */
  def boolVar(name: String=null): NumVar =
    NumVar(modeler.boolVar(name))(implicitly(this))

  /**
    * Creates and returns a map of binary variables.
    *
    * @param keys is an iterable representing the keys
    * @param defaultValue the default variable returned if the key is not found
    * @param namer is a function that is used to give a name to the variables
    * @return a map of binary variables
    */
  def boolVars[T](keys: Iterable[T], defaultValue: Option[NumVar], namer: (T) => String) : Map[T, NumVar] = {
    val tmp = (for (t <- keys) yield {
      val v: NumVar = NumVar(modeler.boolVar())(implicitly(this))
      v.setName(namer(t))
      t -> v
    })
    if (defaultValue.isDefined)
         tmp.toMap.withDefaultValue(defaultValue.get)
    else
      tmp.toMap
  }

  /**
   * Creates and returns a map of binary variables.
   *
   * @param keys is an iterable representing the keys
   * @param namer is a function that is used to give a name to the variables
   * @return a map of binary variables
   */
  def boolVars[T](keys: Iterable[T], namer: (T) => String) : Map[T, NumVar] = {
    boolVars(keys, None, namer)
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
  @deprecated("Use the method with one array of iterable", "decisionbrain-cplex-scala-1.7.0")
  def boolVars[T, U](keys1: Iterable[T], keys2: Iterable[U], namer: (T, U) => String) : Map[(T,U), NumVar] = {
    (for (t <- keys1; u <- keys2) yield {
      val v: NumVar = NumVar(modeler.boolVar())(implicitly(this))
      v.setName(namer(t, u))
      (t,u) -> v
    }).toMap
  }

  /**
    * Creates and return a linear expression initialized with the value of the integer given as argument
    *
    * @param value the value of the linear integer expression
    * @return the linear integer expression
    */
  def linearIntExpr(value: Int = 0): IntExpr =
    IntExpr(modeler.linearIntExpr(value))(implicitly(this))

  /**
    *
    * @param value
    * @return
    */
  def linearNumExpr(value: Double= 0): NumExpr =
    NumExpr(modeler.linearNumExpr(value))(implicitly(this))

  /**
    * Returns a new integer linear expression representing the scalar product of the given integer values
    * with the given integer variables.
    *
    * @param values is the sequence of values
    * @param vars is the sequence of variables
    * @return the scalar product of integer values with integer variables
    */
  def scalProd(values: IntArray, vars: IntVarArray): IntExpr =
    IntExpr(modeler.scalProd(values.toArray, vars.toIloArray))(implicitly(this))

  @deprecated("Replaced by method scalProd", "decisionbrain-cplex-scala-1.5.0")
  def scalarProduct(values: IntArray, vars: IntVarArray): IntExpr =
    IntExpr(modeler.scalProd(vars.toIloArray, values.toArray))(implicitly(this))

  /**
    * Returns a new integer linear expression representing the scalar product of the given integer values
    * with the given integer variables.
    *
    * @param values is the sequence of values
    * @param vars is the sequence of variables
    * @return the scalar product of integer values with integer variables
    */
  def scalProd(vars: IntVarArray, values: IntArray): IntExpr =
    IntExpr(modeler.scalProd(vars.toIloArray, values.toArray))(implicitly(this))

  @deprecated("Replaced by method scalProd", "decisionbrain-cplex-scala-1.5.0")
  def scalarProduct(vars: IntVarArray, values: IntArray): IntExpr =
    IntExpr(modeler.scalProd(vars.toIloArray, values.toArray))(implicitly(this))

  /**
    * Returns a new integer linear expression representing the scalar product of the given integer values
    * with the given integer variables.
    *
    * @param values is the sequence of values
    * @param vars is the sequence of variables
    * @return the scalar product of integer values with integer variables
    */
  def scalProd(values: Array[Int], vars: Array[IntVar]): IntExpr =
    IntExpr(modeler.scalProd(values, vars.map(v => v.getIloIntVar())))(implicitly(this))

  @deprecated("Replaced by method scalProd", "decisionbrain-cplex-scala-1.5.0")
  def scalarProduct(values: Array[Int], vars: Array[IntVar]): IntExpr =
    IntExpr(modeler.scalProd(values, vars.map(v => v.getIloIntVar())))(implicitly(this))

  /**
    * Returns a new integer linear expression representing the scalar product of the given integer values
    * with the given integer variables.
    *
    * @param values is the sequence of values
    * @param vars is the sequence of variables
    * @return the scalar product of integer values with integer variables
    */
  def scalProd(vars: Array[IntVar], values: Array[Int]): IntExpr =
    IntExpr(modeler.scalProd(vars.map(v => v.getIloIntVar()), values))(implicitly(this))

  @deprecated("Replaced by method scalProd", "decisionbrain-cplex-scala-1.5.0")
  def scalarProduct(vars: Array[IntVar], values: Array[Int]): IntExpr =
    IntExpr(modeler.scalProd(vars.map(v => v.getIloIntVar()), values))(implicitly(this))

  /**
    * Returns a new numeric expression representing the scalar product of the numeric values
    * with the given numeric variables.
    *
    * @param values is the sequence of values
    * @param vars is the sequence of variables
    * @return the scalar product of numeric values with numerci variables
    */
  def scalProd(values: NumArray, vars: NumVarArray): NumExpr =
    NumExpr(modeler.scalProd(values.toArray, vars.toIloArray))(implicitly(this))

  @deprecated("Replaced by method scalProd", "decisionbrain-cplex-scala-1.5.0")
  def scalarProduct(values: NumArray, vars: NumVarArray): NumExpr =
    NumExpr(modeler.scalProd(values.toArray, vars.toIloArray))(implicitly(this))
  /**
    * Returns a new numeric expression representing the scalar product of the numeric values
    * with the given numeric variables.
    *
    * @param values is the sequence of values
    * @param vars is the sequence of variables
    * @return the scalar product of numeric values with numerci variables
    */
  def scalProd(vars: NumVarArray, values: NumArray): NumExpr =
    NumExpr(modeler.scalProd(vars.toIloArray, values.toArray))(implicitly(this))

  @deprecated("Replaced by method scalProd", "decisionbrain-cplex-scala-1.5.0")
  def scalarProduct(vars: NumVarArray, values: NumArray): NumExpr =
    NumExpr(modeler.scalProd(vars.toIloArray, values.toArray))(implicitly(this))

  /**
    * Returns a new integer linear expression representing the scalar product of the given integer values
    * with the given integer variables.
    *
    * @param values is the sequence of values
    * @param vars is the sequence of variables
    * @return the scalar product of integer values with integer variables
    */
  def scalProd(values: Array[Double], vars: Array[NumVar]): NumExpr =
    NumExpr(modeler.scalProd(vars.map(v => v.getIloNumVar()), values))(implicitly(this))

  @deprecated("Replaced by method scalProd", "decisionbrain-cplex-scala-1.5.0")
  def scalarProduct(values: Array[Double], vars: Array[NumVar]): NumExpr =
    NumExpr(modeler.scalProd(vars.map(v => v.getIloNumVar()), values))(implicitly(this))

  /**
    * Returns a new integer linear expression representing the scalar product of the given integer values
    * with the given integer variables.
    *
    * @param values is the sequence of values
    * @param vars is the sequence of variables
    * @return the scalar product of integer values with integer variables
    */
  def scalProd(vars: Array[NumVar], values: Array[Double]): NumExpr =
    NumExpr(modeler.scalProd(vars.map(v => v.getIloNumVar()), values))(implicitly(this))

  @deprecated("Replaced by method scalProd", "decisionbrain-cplex-scala-1.5.0")
  def scalarProduct(vars: Array[NumVar], values: Array[Double]): NumExpr =
    NumExpr(modeler.scalProd(vars.map(v => v.getIloNumVar()), values))(implicitly(this))

  /**
    * Returns a new integer linear expression representing the scalar product of the given integer values
    * with the given integer variables.
    *
    * @param values is the sequence of values
    * @param vars is the sequence of variables
    * @return the scalar product of integer values with integer variables
    */
  def scalProd(values: Iterable[Double], vars: Iterable[NumVar]): NumExpr =
    NumExpr(modeler.scalProd(values.toArray, vars.map(v => v.getIloNumVar()).toArray))(implicitly(this))

  @deprecated("Replaced by method scalProd", "decisionbrain-cplex-scala-1.5.0")
  def scalarProduct(values: Iterable[Double], vars: Iterable[NumVar]): NumExpr =
    NumExpr(modeler.scalProd(values.toArray, vars.map(v => v.getIloNumVar()).toArray))(implicitly(this))

  /**
    * Return a new numeric expression that is the sum of numeric expressions.
    *
    * @param exprs is a sequence of numeric expressions
    * @return a numeric expression that represents the sum of numeric expressions
    */
  def sum(exprs: NumExpr*): NumExpr = {
    NumExpr(modeler.sum(exprs.map(e => e.getIloNumExpr).toArray))(implicitly(this))
  }

  /**
    * Returns a new integer expression that is the sum of integer expressions.
    *
    * @param exprs is a sequence of integer expressions
    * @return a numeric expression that represents the sum of numeric expressions
    */
  def sum(exprs: IntExpr*): IntExpr = {
    IntExpr(modeler.sum(exprs.map(e => e.getIloIntExpr).toArray))(implicitly(this))
  }

  /**
    * Return a new numeric expression that is the sum of numeric expressions.
    *
    * @param exprs is a sequence of numeric variables
    * @return a numeric expression that represents the sum of the numeric expressions
    */
  def sum(exprs: Iterable[NumExpr]) : NumExpr = {
    NumExpr(modeler.sum(exprs.map(e => e.getIloNumExpr).toArray))(implicitly(this))
  }

  /**
    * Returns a new integer expression that is the sum of numeric expressions.
    *
    * @param exprs is a sequence of numeric variables
    * @return a numeric expression that represents the sum of the numeric expressions
    */
  def sum(exprs: Iterable[IntExpr]) : IntExpr = {
    IntExpr(modeler.sum(exprs.map(e => e.getIloIntExpr).toArray))(implicitly(this))
  }

  /**
    * Return a new numeric expression that is the sum of numeric expressions.
    *
    * @param exprs is a sequence of numeric variables
    * @return a numeric expression that represents the sum of the numeric expressions
    */
  def sum(exprs: NumExprArray) : NumExpr = {
    NumExpr(modeler.sum(exprs.toIloArray))(implicitly(this))
  }

  /**
    * Returns a new integer expression that is the sum of integer expressions.
    *
    * @param exprs is an array of integer expressions
    * @return an integer expression that represents the sum of the integer expressions
    */
  def sum(exprs: IntExprArray) : IntExpr = {
    IntExpr(modeler.sum(exprs.toIloArray))(implicitly(this))
  }

  /**
    * Returns a new numeric expression that is the sum of a numeric expression and a double value.
    *
    * @param expr is the numeric expression
    * @param v is the value
    * @return a numeric expression that is the sum of a numeric expression and a double value
    */
  def sum(expr: NumExpr, v: Double): NumExpr =
    NumExpr(modeler.sum(expr.getIloNumExpr(), v))(implicitly(this))

  /**
    * Returns a new integer expression that is the sum of an integer expression and an integer value.
    *
    * @param expr is the integer expression
    * @param v is the integer value
    * @return a integer expression that is the sum of a numeric expression and a double value
    */
  def sum(expr: IntExpr, v: Int): IntExpr =
    IntExpr(modeler.sum(expr.getIloIntExpr(), v))(implicitly(this))

  /**
    * Returns a new numeric expression that is the difference between two numeric expressions.
    *
    * @param expr1 is the first numeric expression
    * @param expr2 is the second numeric expression
    * @return the difference between two numeric expressions
    */
  def diff(expr1: NumExpr, expr2: NumExpr): NumExpr =
    NumExpr(modeler.diff(expr1.getIloNumExpr(), expr2.getIloNumExpr()))(implicitly(this))

  /**
    * Returns a new numeric expression that is the difference between a numeric expressions and a numeric value.
    *
    * @param expr is the numeric expression
    * @param v is the numeric value
    * @return the difference between the numeric expression and the numeric value
    */
  def diff(expr: NumExpr, v: Double): NumExpr =
    NumExpr(modeler.diff(expr.getIloNumExpr(), v))(implicitly(this))

  /**
    * Returns a new numeric expression that is the difference between a numeric value and a numeric expression.
    *
    * @param v is the numeric value
    * @param expr is the numeric expression
    * @return the difference between the numeric value and the numeric expression
    */
  def diff(v: Double, expr: NumExpr): NumExpr =
    NumExpr(modeler.diff(expr.getIloNumExpr(), v))(implicitly(this))

  /**
    * Returns a new integer expression that is the difference between two integer expressions.
    *
    * @param expr1 is the first integer expression
    * @param expr2 is the second integer expression
    * @return the difference between two integer expressions
    */
  def diff(expr1: IntExpr, expr2: IntExpr): IntExpr =
    IntExpr(modeler.diff(expr1.getIloIntExpr(), expr2.getIloIntExpr()))(implicitly(this))

  /**
    * Returns a new integer experssion that is the difference between an integer expression and a integer value.
    *
    * @param expr is the integer expression
    * @param v is the integer value
    * @return the difference between an integer expression and an integer value
    */
  def diff(expr: IntExpr, v: Int): IntExpr =
    IntExpr(modeler.diff(expr.getIloIntExpr(), v))(implicitly(this))

  /**
    * Returns a new integer expression that is the difference between an integer value and a integer expressions.
    *
    * @param v is the integer value
    * @param expr is the integer expression
    * @return the difference between an integer value and an integer expression
    */
  def diff(v: Int, expr: IntExpr): IntExpr =
    IntExpr(modeler.diff(v, expr.getIloIntExpr()))(implicitly(this))

  /**
    * Returns a new numeric expression that is the negation of a numeric expression.
    *
    * @param expr is the numeric expression
    * @return the negation of the numeric expression
    */
  def negative(expr: NumExpr): NumExpr =
    NumExpr(modeler.negative(expr.getIloNumExpr()))(implicitly(this))

  /**
    * Returns a new numeric expression that is the negation of an integer expression.
    *
    * @param expr is the integer expression
    * @return the negation of the integer expression
    */
  def negative(expr: IntExpr): IntExpr =
    IntExpr(modeler.negative(expr.getIloIntExpr()))(implicitly(this))

  /**
    * Returns a new numeric expression representing the product of two numeric expressions.
    *
    * @param expr1 is the first numeric expression
    * @param expr2 is the second numeric expression
    * @return the product of two numeric expressions
    */
  def prod(expr1: NumExpr, expr2: NumExpr): NumExpr =
    NumExpr(modeler.prod(expr1.getIloNumExpr(), expr2.getIloNumExpr()))(implicitly(this))

  /**
    * Returns a new numeric expression representing the product of a numeric expression and a numeric value.
    *
    * @param expr is the numeric expression
    * @param v is the numeric value
    * @return the product of two numeric expressions
    */
  def prod(expr: NumExpr, v: Double): NumExpr =
    NumExpr(modeler.prod(expr.getIloNumExpr(), v))(implicitly(this))

  /**
    * Returns a new numeric expression representing the product of a numeric value and a numeric expression.
    *
    * @param v is the numeric value
    * @param expr is the numeric expression
    * @return the product of two numeric expressions
    */
  def prod(v: Double, expr: NumExpr): NumExpr =
    NumExpr(modeler.prod(v, expr.getIloNumExpr()))(implicitly(this))

  /**
    * Returns a new integer expression representing the product of two integer expressions.
    *
    * @param expr1 is the first integer expression
    * @param expr2 is the second integer expression
    * @return the product of two integer expressions
    */
  def prod(expr1: IntExpr, expr2: IntExpr): IntExpr =
    IntExpr(modeler.prod(expr1.getIloIntExpr(), expr2.getIloIntExpr()))(implicitly(this))

  /**
    * Returns a new integer expression representing the product of an integer expression and an integer value.
    *
    * @param expr is the integer expression
    * @param v is the integer value
    * @return the product of the integer expression and the integer value
    */
  def prod(expr: IntExpr, v: Int): IntExpr =
    IntExpr(modeler.prod(expr.getIloIntExpr(), v))(implicitly(this))

  /**
    * Returns a new integer expression representing the product of an integer value and an integer expression.
    *
    * @param v is the integer value
    * @param expr is the integer expression
    * @return the product of the integer value and the integer expression
    */
  def prod(v: Int, expr: IntExpr): IntExpr =
    IntExpr(modeler.prod(v, expr.getIloIntExpr()))(implicitly(this))

  /**
    * Returns a new numeric expression that is the square of the given numeric expression.
    *
    * @param expr is the numeric expression
    * @return the square of the numeric expression
    */
  def square(expr: NumExpr): NumExpr = NumExpr(modeler.square(expr.getIloNumExpr))(implicitly(this))

  /**
    * Returns a new numeric expression that is the square of the given numeric expression.
    *
    * @param expr is the numeric expression
    * @return the square of the numeric expression
    */
  def square(expr: IntExpr): IntExpr = IntExpr(modeler.square(expr.getIloIntExpr))(implicitly(this))

  /**
    * Returns the maximum of a set of numeric expressions.
    *
    * @param exprs is an array of numeric expressions
    * @return a numeric expression that represents the maximum of the numeric expressions
    */
  def max(exprs: NumExprArray) : NumExpr = {
    NumExpr(modeler.max(exprs.toIloArray))(implicitly(this))
  }

  /**
    * Returns the maximum of set of integer expressions.
    *
    * @param exprs is an array of integer expressions
    * @return an integer expression that represents the maximum of the integer expressions
    */
  def max(exprs: IntExprArray) : IntExpr = {
    IntExpr(modeler.max(exprs.toIloArray))(implicitly(this))
  }

  /**
    * Returns the maximum of a set of numeric expressions.
    *
    * @param exprs is an array of numeric expressions
    * @return a numeric expression that represents the maximum of the numeric expressions
    */
  def max(exprs: Iterable[NumExpr]) : NumExpr = {
    NumExpr(modeler.max(exprs.map(e => e.getIloNumExpr).toArray))(implicitly(this))
  }

  /**
    * Returns the maximum of a set of numeric expressions.
    *
    * @param exprs is an array of numeric expressions
    * @return a numeric expression that represents the maximum of the numeric expressions
    */
  def max(exprs: Iterable[IntExpr]) : IntExpr = {
    IntExpr(modeler.max(exprs.map(e => e.getIloIntExpr).toArray))(implicitly(this))
  }

  /**
    * Returns the maximum of a set of numeric expressions.
    *
    * @param exprs is an array of numeric expressions
    * @return a numeric expression that represents the maximum of the numeric expressions
    */
  def max(exprs: Array[NumExpr]) : NumExpr = {
    NumExpr(modeler.max(exprs.map(e => e.getIloNumExpr)))(implicitly(this))
  }

  /**
    * Returns the maximum numeric expressions.
    *
    * @param exprs is a variable number of numeric expressions
    * @return a numeric expression that represents the maximum of the numeric expressions
    */
  def max(exprs: NumExpr*) : NumExpr = {
    NumExpr(modeler.max(exprs.map(e => e.getIloNumExpr).toArray))(implicitly(this))
  }

  /**
    * Returns the minimum of numeric expressions.
    *
    * @param exprs is an array of numeric expressions
    * @return a numeric expression that represents the minimum of the numeric expressions
    */
  def min(exprs: NumExprArray) : NumExpr = {
    NumExpr(modeler.min(exprs.toIloArray))(implicitly(this))
  }

  /**
    * Returns the minimum of a set of integer expressions.
    *
    * @param exprs is an array of integer expressions
    * @return an integer expression that represents the minimum of the integer expressions
    */
  def min(exprs: IntExprArray) : IntExpr = {
    IntExpr(modeler.min(exprs.toIloArray))(implicitly(this))
  }

  /**
    * Returns the minimum of numeric expressions.
    *
    * @param exprs is an array of numeric expressions
    * @return a numeric expression that represents the minimum of the numeric expressions
    */
  def min(exprs: Array[NumExpr]) : NumExpr = {
    NumExpr(modeler.min(exprs.map(e => e.getIloNumExpr)))(implicitly(this))
  }

  /**
    * Returns the minimum of numeric expressions.
    *
    * @param exprs is an array of numeric expressions
    * @return a numeric expression that represents the minimum of the numeric expressions
    */
  def min(exprs: Array[IntExpr]) : IntExpr = {
    IntExpr(modeler.min(exprs.map(e => e.getIloIntExpr)))(implicitly(this))
  }

  /**
    * Returns the minimum of numeric expressions.
    *
    * @param exprs is an array of numeric expressions
    * @return a numeric expression that represents the minimum of the numeric expressions
    */
  def min(exprs: Iterable[NumExpr]) : NumExpr = {
    NumExpr(modeler.min(exprs.map(e => e.getIloNumExpr).toArray))(implicitly(this))
  }

  /**
    * Returns the minimum of integer expressions.
    *
    * @param exprs is an array of integer expressions
    * @return a numeric expression that represents the minimum of the numeric expressions
    */
  def min(exprs: Iterable[IntExpr]) : IntExpr = {
    IntExpr(modeler.min(exprs.map(e => e.getIloIntExpr).toArray))(implicitly(this))
  }

  /**
    * Returns the minimum of a numeric expressions.
    *
    * @param exprs is a variable number of numeric variables
    * @return a numeric expression that represents the minimum of the numeric expressions
    */
  def min(exprs: NumExpr*) : NumExpr = {
    NumExpr(modeler.min(exprs.map(e => e.getIloNumExpr).toArray))(implicitly(this))
  }

  /**
    * Returns the minimum of a numeric expressions.
    *
    * @param exprs is a variable number of numeric variables
    * @return a numeric expression that represents the minimum of the numeric expressions
    */
  def min(exprs: IntExpr*) : IntExpr = {
    IntExpr(modeler.min(exprs.map(e => e.getIloIntExpr).toArray))(implicitly(this))
  }

  /**
    * Returns a new constraint <i>greater-than-or-equal-to</i> between numeric expressions.
    *
    * @param expr1 is the lefthand side numeric expression
    * @param expr2 is the righthand side numeric expression
    * @return a constraint <code>expr1 >= expr2</code>
    */
  def ge(expr1: NumExpr, expr2: NumExpr): Constraint =
    Constraint(modeler.ge(expr1.getIloNumExpr(), expr2.getIloNumExpr()))(implicitly(this))

  /**
    * Returns a new constraint <i>greater-than-or-equal-to</i> constraint between a numeric expression and a numeric value.
    *
    * @param expr is the lefthand side numeric expression
    * @param v is the righthand side numeric value
    * @return a constraint <code>expr >= v</code>
    */
  def ge(expr: NumExpr, v: Double): Range =
    Range(modeler.ge(expr.getIloNumExpr(), v))(implicitly(this))

  /**
    * Returns a new constraint <i>greater-than-or-equal-to</i> between a numeric value and a numeric expression.
    *
    * @param v is the righthand side numeric value
    * @param expr is the lefthand side numeric expression
    * @return a constraint <code>v >= expr</code>
    */
  def ge(v: Double, expr: NumExpr): Range =
    Range(modeler.ge(v, expr.getIloNumExpr()))(implicitly(this))

  /**
    * Returns a new constraint <i>less-than-or-equal-to</i> between two numeric expressions.
    *
    * @param expr1 is the righthand side numeric expression
    * @param expr2 is the lefthand side numeric expression
    * @return a constraint <code>expr1 <= expr2</code>
    */
  def le(expr1: NumExpr, expr2: NumExpr): Constraint =
    Constraint(modeler.le(expr1.getIloNumExpr(), expr2.getIloNumExpr()))(implicitly(this))

  /**
    * Returns a new constraint <i>less-than-or-equal-to</i> between a numeric expression and a numeric value.
    *
    * @param expr is the righthand side numeric expression
    * @param v is the lefthand side numeric value
    * @return a constraint <code>expr <= v</code>
    */
  def le(expr: NumExpr, v: Double): Range =
    Range(modeler.le(expr.getIloNumExpr(), v))(implicitly(this))

  /**
    * Returns a new constraint <i>less-than-or-equal-to</i> between a numeric value and a numeric expression.
    *
    * @param v is the righthand side numeric value
    * @param expr is the lefthand side numeric expression
    * @return a constraint <code>v <= expr</code>
    */
  def le(v: Double, expr: NumExpr): Range =
    Range(modeler.le(expr.getIloNumExpr(), v))(implicitly(this))

  /**
    * Returns a new constraint <i>equal-to</i> between two numeric expressions.
    *
    * @param expr1 is the righthand side numeric expression
    * @param expr2 is the lefthand side numeric expression
    * @return a constraint <code>expr1 == expr2</code>
    */
  def eq(expr1: NumExpr, expr2: NumExpr): Constraint =
    Constraint(modeler.eq(expr1.getIloNumExpr(), expr2.getIloNumExpr()))(implicitly(this))

  /**
    * Returns a new constraint <i>equal-to</i> between two numeric expressions.
    *
    * @param expr1 is the righthand side numeric expression
    * @param expr2 is the lefthand side numeric expression
    * @param name is the name of the constraint
    * @return the constraint <code>expr1 == expr</code>
    */
  def eq(expr1: NumExpr, expr2: NumExpr, name: String): Constraint =
    Constraint(modeler.eq(expr1.getIloNumExpr(), expr2.getIloNumExpr(), name))(implicitly(this))

  /**
    * Returns a new constraint equal-to between a numeric expression and a numeric value.
    *
    * @param expr is the righthand side numeric expression
    * @param v is the lefthand side numeric value
    * @return the constraint <code>expr == v</code>
    */
  def eq(expr: NumExpr, v: Double): Range =
    Range(modeler.eq(expr.getIloNumExpr(), v))(implicitly(this))

  /**
    * Returns a new constraint <i>equal-to</i> between a numeric value and a numeric expression.
    *
    * @param v is the lefthand side numeric value
    * @param expr is the righthand side numeric expression
    * @return the constraint <code>v == expr/code>
    */
  def eq(v: Double, expr: NumExpr): Range =
    Range(modeler.eq(expr.getIloNumExpr(), v))(implicitly(this))

  /**
    * Returns a new constraint indicating that at least one of the two constraints is true.
    *
    * @param ct1 is the first constraint
    * @param ct2 is the second constraint
    * @return the logical-or of two constraints
    */
  def or(ct1: Constraint, ct2: Constraint): Constraint =
    Constraint(modeler.or(ct1.getIloConstraint(), ct2.getIloConstraint()))(implicitly(this))

  /**
    * Returns a new constraint indicating that both constraints are true.
    *
    * @param ct1 is the first constraint
    * @param ct2 is the second constraint
    * @return the logical-and of two constraints
    */
  def and(ct1: Constraint, ct2: Constraint): Constraint =
    Constraint(modeler.and(ct1.getIloConstraint(), ct2.getIloConstraint()))(implicitly(this))

  /**
    * Returns a new constraint that is the negation of the given constraint.
    *
    * @param ct is the constraint
    * @return the logical negation of the constraint
    */
  def not(ct: Constraint): Constraint =
    Constraint(modeler.not(ct.getIloConstraint()))(implicitly(this))

  /**
    * Returns a new constraint that if constraint <i>ct1</i> is true, then constraint <i>ct2</i> must also be true.
    *
    * @param ct1 is the first constraint
    * @param ct2 is the second constraint
    * @return the conditional constraint
    */
  def ifThen(ct1: Constraint, ct2: Constraint): Constraint =
    Constraint(modeler.ifThen(ct1.getIloConstraint(), ct2.getIloConstraint()))(implicitly(this))

  /**
    * Add an addable object in the model.
    *
    * @param a is the object to add to the model
    * @return the model
    */
  def add(a: Addable, name: String=null): Modeler = {
    a.setName(name)
    modeler.add(a.getIloAddable())
    this
  }

  /**
    * Add addable objects in the model.
    *
    * @param addables are the object to add to the model
    * @return the model
    */
  def add(addables: Addable*): Modeler = {
    modeler.add(addables.map(a => a.getIloAddable()).toArray)
    this
  }

  /**
    * Add addable objects in the model.
    *
    * @param addables are the object to add to the model
    * @return the model
    */
  def add(addables: Iterable[Addable]): Modeler = {
    modeler.add(addables.map(a => a.getIloAddable()).toArray)
    this
  }

  /**
    * Remove an object from the model.
    *
    * @param a is the object to remove
    * @return the optimization model
    */
  def remove(a: Addable): Modeler = {
    modeler.remove(a.getIloAddable())
    this
  }

  /**
    * Remove a set of objects from the model.
    *
    * @param addables are the objects to remove
    * @return the optimization model
    */
  def remove(addables: Addable*): Modeler = {
    modeler.remove(addables.map(a => a.getIloAddable()).toArray)
    this
  }

  /**
    * Remove a set of objects from the model.
    *
    * @param addables are the objects to remove
    * @return the optimization model
    */
  def remove(addables: Iterable[Addable]): Modeler = {
    modeler.remove(addables.map(a => a.getIloAddable()).toArray)
    this
  }

  /**
    * Creates and returns an objective object to minimize the expression <em>expr</em>.
    *
    * @param expr is the expression to minimize
    * @return An objective object representing the objective to minimize
    */
  def minimize(expr: NumExpr): Objective = Objective(modeler.minimize(expr.getIloNumExpr))(implicitly(this))

  /**
    * Creates and returns an objective object to maximize the expression <em>expr</em>.
    *
    * @param expr is the expression to maximize
    * @return An objective object the objective to maximize
    */
  def maximize(expr: NumExpr): Objective = Objective(modeler.maximize(expr.getIloNumExpr))(implicitly(this))

  //
  // Scala Numeric
  //


  private val numExprNumeric = NumExprNumeric(this)
  private val intExprNumeric = IntExprNumeric(this)

  /**
    * Returns the numeric for numeric expressions. This allows to do things such as calling method sum on list of
    * numeric expressions. For instance:
    * <pre>
    *   <code>
    *     implicit val num = model.getNumExprNumeric()
    *     val exprs = List(model.numVar(), model.numVar())
    *     val sumExpr = exprs.sum
    *   </code>
    * </pre>
    * @return the numeric for numeric expression
    */
  def getNumExprNumeric(): NumExprNumeric = numExprNumeric

  /**
    * Returns the numeric for numeric expressions. This allows to do things such as calling method sum on list of
    * numeric expressions. For instance:
    * <pre>
    *   <code>
    *     implicit val num = model.getIntExprNumeric()
    *     val exprs = List(model.intVar(), model.intVar())
    *     val sumExpr = exprs.sum
    *   </code>
    * </pre>
    * @return the numeric for integer expression
    */
  def getIntExprNumeric(): IntExprNumeric = intExprNumeric

}

object Modeler {

  //
  // Implicit conversion
  //

  /**
    *  Implicit conversion of set of numeric expressions
    *
    * @param exprs are the integer expressions
    * @param modeler is the constraint programming model
    */
  implicit class NumExprArray(val exprs: Iterable[NumExpr])(implicit modeler: Modeler) {

    /**
      * Converts to scala array
      */
    def toArray: Array[NumExpr] = exprs.toArray

    /**
      * Convert to CPLEX array
      */
    def toIloArray: Array[IloNumExpr] = exprs.map(e => e.getIloNumExpr()).toArray
  }

  /**
    *  Implicit conversion of set of integer expressions: add behavior
    *
    * @param exprs are the integer expressions
    * @param modeler is the constraint programming model
    */
  implicit class IntExprArray(val exprs: Iterable[IntExpr])(implicit modeler: Modeler)
    extends Iterable[IntExpr] {

    /**
      * Method get creates and returns a new integer expression equal to exprs[index] where index is an integer
      * expression.
      *
      * @param expr is the integer expression for the index
      * @return an new integer expression
      */
    def element(expr: IntExpr): IntExpr = modeler match {
      case model: CpModel => model.element(this, expr)
      case _ => throw new UnsupportedOperationException("Method \'element\' only supported on CpModel")
    }


    /**
      * Method get creates and returns a new integer expression equal to exprs[index] where index is an integer
      * expression.
      *
      * @param expr is the integer expression for the index
      * @return an new integer expression
      */
    def apply(expr: IntExpr): IntExpr = element(expr)

    /**
      * Converts to scala array
      */
    override def toArray[B >: IntExpr : ClassTag]: Array[B] = exprs.toArray[B]

    /**
      * Converts to scala array
      */
    def toIloArray: Array[IloIntExpr] = exprs.map(e => e.getIloIntExpr()).toArray

    /**
      * Returns an iterator.
      *
      * @return an iterator
      */
    override def iterator: Iterator[IntExpr] = exprs.iterator
  }

  /**
    *  Class IntVarArray gives additional behavior such as implicit conversion to search phase
    *
    * @param vars are the integer variables
    * @param modeler is the optimization model
    */
  implicit class NumVarArray(val vars: Iterable[NumVar])(implicit modeler: Modeler) extends Iterable[NumVar] {

    /**
      * Converts to scala array
      */
    override def toArray[B >: NumVar: ClassTag]: Array[B] = vars.toArray[B]

    /**
      * Converts to CPLEX array
      */
    def toIloArray[B >: IloNumVar: ClassTag]: Array[B] = vars.map(v => v.getIloNumVar()).toArray

    /**
      * Returns a numeric expression that is the scalar product of theses numeric variables with the given numeric
      * values.
      *
      * @param values are the numeric values
      * @return the scalar product of theses numeric variables with the given numeric values
      */
    def *(values: NumArray): NumExpr = modeler.scalProd(this, values)

    /**
      * Returns an iterator.
      *
      * @return an iterator
      */
    override def iterator: Iterator[NumVar] = vars.iterator
  }

  /**
    *  Class IntVarArray gives additional behavior such as implicit conversion to search phase
    *
    * @param vars are the integer variables
    * @param modeler is the optimization model
    */
  implicit class IntVarArray(val vars: Iterable[IntVar])(implicit modeler: Modeler) extends Iterable[IntVar] {

    /**
      * Converts to search phase.
      *
      * @return a search phase
      */
    def toSearchPhase: SearchPhase = modeler match {
      case model: CpModel => model.searchPhase(this)
      case _ => throw new UnsupportedOperationException("Method \'toSearchPhase\' only supported on CpModel")
    }

    /**
      * Returns an integer expression that is the scalar product of theses integer variables with the given integer
      * values.
      *
      * @param values are the integer values
      * @return the scalar product of theses integer variables with the given integer values
      */
    def *(values: IntArray): IntExpr = modeler.scalProd(this, values)

    /**
      * Converts to scala array
      */
    override def toArray[B >: IntVar: ClassTag]: Array[B] = vars.toArray[B]

    /**
      * Converts to CPLEX array
      */
    def toIloArray[B >: IloIntVar: ClassTag]: Array[B] = vars.map(v => v.getIloIntVar()).toArray

    /**
      * Returns an iterator.
      *
      * @return an iterator
      */
    override def iterator: Iterator[IntVar] = vars.iterator
  }

  /**
    *  Class IterableInt give additional behavior such as a method get to on a set of integer values.
    *
    * @param values are the integer values
    * @param modeler is the optimization model
    */
  implicit class NumArray(val values: Iterable[Double])(implicit modeler: Modeler)
    extends Iterable[Double] {

    /**
      * Method get creates and returns a new integer expression equal to exprs[index] where index is an integer
      * expression.
      *
      * @param expr is the integer expression for the index
      * @return an new integer expression
      */
    def element(expr: IntExpr): NumExpr = modeler match {
      case model: CpModel => model.element(this, expr)
      case _ => throw new UnsupportedOperationException("Method \'element\' only supported on CpModel")
    }


    /**
      * Method get creates and returns a new integer expression equal to exprs[index] where index is an integer
      * expression.
      *
      * @param expr is the integer expression for the index
      * @return an new integer expression
      */
    def apply(expr: IntExpr): NumExpr = element(expr)

    /**
      * Returns an numeric expression that is the scalar product of these values with the given numeric variables.
      *
      * @param vars are the numeric variables
      * @return the scalar product of these numeric values with the numeric variables
      */
    def *(vars: NumVarArray): NumExpr = modeler.scalProd(this, vars)

    /**
      * Converts to scala array
      */
    def toArray: Array[Double] = values.toArray

    /**
      * Returns an iterator.
      *
      * @return an iterator
      */
    override def iterator: Iterator[Double] = values.iterator
  }

  /**
    *  Class IterableInt give additional behavior such as a method get to on a set of integer values.
    *
    * @param values are the integer values
    * @param modeler is the constraint programming model
    */
  implicit class IntArray(val values: Iterable[Int])(implicit modeler: Modeler)
    extends Iterable[Int] {

    /**
      * Method get creates and returns a new integer expression equal to exprs[index] where index is an integer
      * expression.
      *
      * @param expr is the integer expression for the index
      * @return an new integer expression
      */
    def element(expr: IntExpr): IntExpr = modeler match {
      case model: CpModel => model.element(values, expr)
      case _ => throw new UnsupportedOperationException("Method \'element\' only supported on CpModel")
    }

    /**
      * Method get creates and returns a new integer expression equal to exprs[index] where index is an integer
      * expression.
      *
      * @param expr is the integer expression for the index
      * @return an new integer expression
      */
    def apply(expr: IntExpr): IntExpr = element(expr)

    /**
      * Returns an integer expression that is the scalar product of these values with the given integer variables.
      *
      * @param vars are the integer variables
      * @return the scalar product of these integer values with the integer variables
      */
    def *(vars: IntVarArray): IntExpr = modeler.scalProd(this, vars)

    /**
      * Converts to scala array
      */
    def toArray: Array[Int] = values.toArray

    /**
      * Returns an iterator.
      *
      * @return an iterator
      */
    override def iterator: Iterator[Int] = values.iterator

  }

  /**
    * Creates and returns an integer linear expression representing the scalar product of the given integer values
    * with the given integer variables.
    *
    * @param values is the sequence of values
    * @param vars is the sequence of variables
    * @return the scalar product of integer values with integer variables
    */
  def scalProd(values: IntArray, vars: IntVarArray)(implicit modeler: Modeler): IntExpr =
    modeler.scalProd(values, vars)

  @deprecated("Replaced by method scalProd", "decisionbrain-cplex-scala-1.5.0")
  def scalarProduct(values: IntArray, vars: IntVarArray)(implicit modeler: Modeler): IntExpr =
    modeler.scalProd(values, vars)

  /**
    * Creates and returns an integer linear expression representing the scalar product of the given integer values
    * with the given integer variables.
    *
    * @param values is the sequence of values
    * @param vars is the sequence of variables
    * @return the scalar product of integer values with integer variables
    */
  def scalProd(vars: IntVarArray, values: IntArray)(implicit modeler: Modeler): IntExpr =
    modeler.scalProd(values, vars)

  @deprecated("Replaced by method scalProd", "decisionbrain-cplex-scala-1.5.0")
  def scalarProduct(vars: IntVarArray, values: IntArray)(implicit modeler: Modeler): IntExpr =
    modeler.scalProd(values, vars)

  /**
    * Creates and returns an integer linear expression representing the scalar product of the given integer values
    * with the given integer variables.
    *
    * @param values is the sequence of values
    * @param vars is the sequence of variables
    * @return the scalar product of integer values with integer variables
    */
  def scalProd(values: Array[Int], vars: Array[IntVar])(implicit modeler: Modeler): IntExpr =
    modeler.scalProd(values, vars)

  @deprecated("Replaced by method scalProd", "decisionbrain-cplex-scala-1.5.0")
  def scalarProduct(values: Array[Int], vars: Array[IntVar])(implicit modeler: Modeler): IntExpr =
    modeler.scalProd(values, vars)

  /**
    * Creates and returns an integer linear expression representing the scalar product of the given integer values
    * with the given integer variables.
    *
    * @param values is the sequence of values
    * @param vars is the sequence of variables
    * @return the scalar product of integer values with integer variables
    */
  def scalProd(vars: Array[IntVar], values: Array[Int])(implicit modeler: Modeler): IntExpr =
    modeler.scalProd(vars, values)

  @deprecated("Replaced by method scalProd", "decisionbrain-cplex-scala-1.5.0")
  def scalarProduct(vars: Array[IntVar], values: Array[Int])(implicit modeler: Modeler): IntExpr =
    modeler.scalProd(vars, values)

  /**
    * Creates and returns an linear expression representing the scalar product of the numeric values
    * with the given numeric variables.
    *
    * @param values is the sequence of values
    * @param vars is the sequence of variables
    * @return the scalar product of numeric values with numerci variables
    */
  def scalProd(values: NumArray, vars: NumVarArray)(implicit modeler: Modeler): NumExpr =
    modeler.scalProd(values, vars)

  @deprecated("Replaced by method scalProd", "decisionbrain-cplex-scala-1.5.0")
  def scalarProduct(values: NumArray, vars: NumVarArray)(implicit modeler: Modeler): NumExpr =
    modeler.scalProd(values, vars)

  /**
    * Creates and returns an linear expression representing the scalar product of the numeric values
    * with the given numeric variables.
    *
    * @param values is the sequence of values
    * @param vars is the sequence of variables
    * @return the scalar product of numeric values with numerci variables
    */
  def scalProd(vars: NumVarArray, values: NumArray)(implicit modeler: Modeler): NumExpr =
    modeler.scalProd(vars, values)

  @deprecated("Replaced by method scalProd", "decisionbrain-cplex-scala-1.5.0")
  def scalarProduct(vars: NumVarArray, values: NumArray)(implicit modeler: Modeler): NumExpr =
    modeler.scalProd(vars, values)

  /**
    * Creates and returns an integer linear expression representing the scalar product of the given integer values
    * with the given integer variables.
    *
    * @param values is the sequence of values
    * @param vars is the sequence of variables
    * @return the scalar product of integer values with integer variables
    */
  def scalProd(values: Array[Double], vars: Array[NumVar])(implicit modeler: Modeler): NumExpr =
    modeler.scalProd(vars, values)

  @deprecated("Replaced by method scalProd", "decisionbrain-cplex-scala-1.5.0")
  def scalarProduct(values: Array[Double], vars: Array[NumVar])(implicit modeler: Modeler): NumExpr =
    modeler.scalProd(vars, values)

  /**
    * Creates and returns an integer linear expression representing the scalar product of the given integer values
    * with the given integer variables.
    *
    * @param values is the sequence of values
    * @param vars is the sequence of variables
    * @return the scalar product of integer values with integer variables
    */
  def scalProd(vars: Array[NumVar], values: Array[Double])(implicit modeler: Modeler): NumExpr =
    modeler.scalProd(vars, values)

  @deprecated("Replaced by method scalProd", "decisionbrain-cplex-scala-1.5.0")
  def scalarProduct(vars: Array[NumVar], values: Array[Double])(implicit modeler: Modeler): NumExpr =
    modeler.scalProd(vars, values)

  /**
    * Creates and returns an integer linear expression representing the scalar product of the given integer values
    * with the given integer variables.
    *
    * @param values is the sequence of values
    * @param vars is the sequence of variables
    * @return the scalar product of integer values with integer variables
    */
  def scalProd(values: Iterable[Double], vars: Iterable[NumVar])(implicit modeler: Modeler): NumExpr =
    modeler.scalProd(values, vars)

  @deprecated("Replaced by method scalProd", "decisionbrain-cplex-scala-1.5.0")
  def scalarProduct(values: Iterable[Double], vars: Iterable[NumVar])(implicit modeler: Modeler): NumExpr =
    modeler.scalProd(values, vars)

  /**
    * Return the sum of a set of numeric expressions.
    *
    * @param exprs is a sequence of numeric variables
    * @return a numeric expression that represents the sum of the numeric expressions
    */
  def sum(exprs: NumExprArray)(implicit modeler: Modeler): NumExpr = modeler.sum(exprs)

  /**
    * Returns the integer sum of a set of integer expressions.
    *
    * @param exprs is an array of integer expressions
    * @return a integer expression that represents the sum of the integer expressions
    */
  def sum(exprs: IntExprArray)(implicit modeler: Modeler): IntExpr = modeler.sum(exprs)

  /**
    * Return the sum of a set of numeric expressions.
    *
    * @param exprs is a sequence of numeric variables
    * @return a numeric expression that represents the sum of the numeric expressions
    */
  def sum(exprs: NumExpr*)(implicit modeler: Modeler): NumExpr = modeler.sum(exprs)

  /**
    * Return the sum of a set of numeric expressions.
    *
    * @param exprs is a sequence of numeric variables
    * @return a numeric expression that represents the sum of the numeric expressions
    */
  def sum(exprs: IntExpr*)(implicit modeler: Modeler): IntExpr = modeler.sum(exprs)

  /**
    * Return the sum of numeric expressions.
    *
    * @param exprs is a sequence of numeric variables
    * @return a numeric expression that represents the sum of the numeric expressions
    */
  def sum(exprs: Iterable[NumExpr])(implicit modeler: Modeler) : NumExpr = modeler.sum(exprs)

  /**
    * Return the sum of numeric expressions.
    *
    * @param exprs is a sequence of numeric variables
    * @return a numeric expression that represents the sum of the numeric expressions
    */
  def sum(exprs: Iterable[IntExpr])(implicit modeler: Modeler) : IntExpr = modeler.sum(exprs)

  /**
    * Returns a new numeric expression that is the sum of a numeric expression and a double value.
    *
    * @param expr is the numeric expression
    * @param v is the value
    * @return a numeric expression that is the sum of a numeric expression and a double value
    */
  def sum(expr: NumExpr, v: Double)(implicit modeler: Modeler): NumExpr = modeler.sum(expr, v)

  /**
    * Returns a new integer expression that is the sum of an integer expression and an integer value.
    *
    * @param expr is the integer expression
    * @param v is the integer value
    * @return a integer expression that is the sum of a numeric expression and a double value
    */
  def sum(expr: IntExpr, v: Int)(implicit modeler: Modeler): IntExpr = modeler.sum(expr, v)

  /**
    * Returns a new numeric expression that is the difference between two numeric expressions.
    *
    * @param expr1 is the first numeric expression
    * @param expr2 is the second numeric expression
    * @return the difference between two numeric expressions
    */
  def diff(expr1: NumExpr, expr2: NumExpr)(implicit modeler: Modeler): NumExpr = modeler.diff(expr1, expr2)

  /**
    * Returns a new numeric expression that is the difference between a numeric expressions and a numeric value.
    *
    * @param expr is the numeric expression
    * @param v is the numeric value
    * @return the difference between the numeric expression and the numeric value
    */
  def diff(expr: NumExpr, v: Double)(implicit modeler: Modeler): NumExpr = modeler.diff(expr, v)

  /**
    * Returns a new numeric expression that is the difference between a numeric value and a numeric expression.
    *
    * @param v is the numeric value
    * @param expr is the numeric expression
    * @return the difference between the numeric value and the numeric expression
    */
  def diff(v: Double, expr: NumExpr)(implicit modeler: Modeler): NumExpr = modeler.diff(expr, v)

  /**
    * Returns a new integer expression that is the difference between two integer expressions.
    *
    * @param expr1 is the first integer expression
    * @param expr2 is the second integer expression
    * @return the difference between two integer expressions
    */
  def diff(expr1: IntExpr, expr2: IntExpr)(implicit modeler: Modeler): IntExpr = modeler.diff(expr1, expr2)

  /**
    * Returns a new integer experssion that is the difference between an integer expression and a integer value.
    *
    * @param expr is the integer expression
    * @param v is the integer value
    * @return the difference between an integer expression and an integer value
    */
  def diff(expr: IntExpr, v: Int)(implicit modeler: Modeler): IntExpr = modeler.diff(expr, v)

  /**
    * Returns a new integer expression that is the difference between an integer value and a integer expressions.
    *
    * @param v is the integer value
    * @param expr is the integer expression
    * @return the difference between an integer value and an integer expression
    */
  def diff(v: Int, expr: IntExpr)(implicit modeler: Modeler): IntExpr = modeler.diff(v, expr)

  /**
    * Returns a new numeric expression that is the negation of a numeric expression.
    *
    * @param expr is the numeric expression
    * @return the negation of the numeric expression
    */
  def negative(expr: NumExpr)(implicit modeler: Modeler): NumExpr = modeler.negative(expr)

  /**
    * Returns a new numeric expression that is the negation of an integer expression.
    *
    * @param expr is the integer expression
    * @return the negation of the integer expression
    */
  def negative(expr: IntExpr)(implicit modeler: Modeler): IntExpr = modeler.negative(expr)

  /**
    * Returns a new numeric expression representing the product of two numeric expressions.
    *
    * @param expr1 is the first numeric expression
    * @param expr2 is the second numeric expression
    * @return the product of two numeric expressions
    */
  def prod(expr1: NumExpr, expr2: NumExpr)(implicit modeler: Modeler): NumExpr = modeler.prod(expr1, expr2)

  /**
    * Returns a new numeric expression representing the product of a numeric expression and a numeric value.
    *
    * @param expr is the numeric expression
    * @param v is the numeric value
    * @return the product of two numeric expressions
    */
  def prod(expr: NumExpr, v: Double)(implicit modeler: Modeler): NumExpr = modeler.prod(expr, v)

  /**
    * Returns a new numeric expression representing the product of a numeric value and a numeric expression.
    *
    * @param v is the numeric value
    * @param expr is the numeric expression
    * @return the product of two numeric expressions
    */
  def prod(v: Double, expr: NumExpr)(implicit modeler: Modeler): NumExpr = modeler.prod(v, expr)

  /**
    * Returns a new integer expression representing the product of two integer expressions.
    *
    * @param expr1 is the first integer expression
    * @param expr2 is the second integer expression
    * @return the product of two integer expressions
    */
  def prod(expr1: IntExpr, expr2: IntExpr)(implicit modeler: Modeler): IntExpr = modeler.prod(expr1, expr2)

  /**
    * Returns a new integer expression representing the product of an integer expression and an integer value.
    *
    * @param expr is the integer expression
    * @param v is the integer value
    * @return the product of the integer expression and the integer value
    */
  def prod(expr: IntExpr, v: Int)(implicit modeler: Modeler): IntExpr = modeler.prod(expr, v)

  /**
    * Returns a new integer expression representing the product of an integer value and an integer expression.
    *
    * @param v is the integer value
    * @param expr is the integer expression
    * @return the product of the integer value and the integer expression
    */
  def prod(v: Int, expr: IntExpr)(implicit modeler: Modeler): IntExpr = modeler.prod(v, expr)

  /**
    * Returns a new numeric expression that is the square of the given numeric expression.
    *
    * @param expr is the numeric expression
    * @return the square of the numeric expression
    */
  def square(expr: NumExpr)(implicit modeler: Modeler): NumExpr = modeler.square(expr)

  /**
    * Returns a new numeric expression that is the square of the given numeric expression.
    *
    * @param expr is the numeric expression
    * @return the square of the numeric expression
    */
  def square(expr: IntExpr)(implicit modeler: Modeler): IntExpr = modeler.square(expr)

  /**
    * Returns a new constraint <i>greater-than-or-equal-to</i> between numeric expressions.
    *
    * @param expr1 is the lefthand side numeric expression
    * @param expr2 is the righthand side numeric expression
    * @return a constraint <code>expr1 >= expr2</code>
    */
  def ge(expr1: NumExpr, expr2: NumExpr)(implicit modeler: Modeler): Constraint = modeler.ge(expr1, expr2)

  /**
    * Returns a new constraint <i>greater-than-or-equal-to</i> constraint between a numeric expression and a numeric value.
    *
    * @param expr is the lefthand side numeric expression
    * @param v is the righthand side numeric value
    * @return a constraint <code>expr >= v</code>
    */
  def ge(expr: NumExpr, v: Double)(implicit modeler: Modeler): Range = modeler.ge(expr, v)

  /**
    * Returns a new constraint <i>greater-than-or-equal-to</i> between a numeric value and a numeric expression.
    *
    * @param v is the righthand side numeric value
    * @param expr is the lefthand side numeric expression
    * @return a constraint <code>v >= expr</code>
    */
  def ge(v: Double, expr: NumExpr)(implicit modeler: Modeler): Range = modeler.ge(v, expr)

  /**
    * Returns a new constraint <i>less-than-or-equal-to</i> between two numeric expressions.
    *
    * @param expr1 is the righthand side numeric expression
    * @param expr2 is the lefthand side numeric expression
    * @return a constraint <code>expr1 <= expr2</code>
    */
  def le(expr1: NumExpr, expr2: NumExpr)(implicit modeler: Modeler): Constraint = modeler.le(expr1, expr2)

  /**
    * Returns a new constraint <i>less-than-or-equal-to</i> between a numeric expression and a numeric value.
    *
    * @param expr is the righthand side numeric expression
    * @param v is the lefthand side numeric value
    * @return a constraint <code>expr <= v</code>
    */
  def le(expr: NumExpr, v: Double)(implicit modeler: Modeler): Range = modeler.le(expr, v)

  /**
    * Returns a new constraint <i>less-than-or-equal-to</i> between a numeric value and a numeric expression.
    *
    * @param v is the righthand side numeric value
    * @param expr is the lefthand side numeric expression
    * @return a constraint <code>v <= expr</code>
    */
  def le(v: Double, expr: NumExpr)(implicit modeler: Modeler): Range = modeler.le(v, expr)

  /**
    * Returns a new constraint <i>equal-to</i> between two numeric expressions.
    *
    * @param expr1 is the righthand side numeric expression
    * @param expr2 is the lefthand side numeric expression
    * @return a constraint <code>expr1 == expr2</code>
    */
  def eq(expr1: NumExpr, expr2: NumExpr)(implicit modeler: Modeler): Constraint = modeler.eq(expr1, expr2)

  /**
    * Returns a new constraint <i>equal-to</i> between two numeric expressions.
    *
    * @param expr1 is the righthand side numeric expression
    * @param expr2 is the lefthand side numeric expression
    * @param name is the name of the constraint
    * @return the constraint <code>expr1 == expr</code>
    */
  def eq(expr1: NumExpr, expr2: NumExpr, name: String)(implicit modeler: Modeler): Constraint =
    modeler.eq(expr1, expr2, name)

  /**
    * Returns a new constraint equal-to between a numeric expression and a numeric value.
    *
    * @param expr is the righthand side numeric expression
    * @param v is the lefthand side numeric value
    * @return the constraint <code>expr == v</code>
    */
  def eq(expr: NumExpr, v: Double)(implicit modeler: Modeler): Range = modeler.eq(expr, v)

  /**
    * Returns a new constraint <i>equal-to</i> between a numeric value and a numeric expression.
    *
    * @param v is the lefthand side numeric value
    * @param expr is the righthand side numeric expression
    * @return the constraint <code>v == expr/code>
    */
  def eq(v: Double, expr: NumExpr)(implicit modeler: Modeler): Range = modeler.eq(expr, v)

  /**
    * Returns a new constraint indicating that at least one of the two constraints is true.
    *
    * @param ct1 is the first constraint
    * @param ct2 is the second constraint
    * @return the logical-or of two constraints
    */
  def or(ct1: Constraint, ct2: Constraint)(implicit modeler: Modeler): Constraint = modeler.or(ct1, ct2)

  /**
    * Returns a new constraint indicating that both constraints are true.
    *
    * @param ct1 is the first constraint
    * @param ct2 is the second constraint
    * @return the logical-and of two constraints
    */
  def and(ct1: Constraint, ct2: Constraint)(implicit modeler: Modeler): Constraint = modeler.and(ct1, ct2)

  /**
    * Returns a new constraint that is the negation of the given constraint.
    *
    * @param ct is the constraint
    * @return the logical negation of the constraint
    */
  def not(ct: Constraint)(implicit modeler: Modeler): Constraint = modeler.not(ct)

  /**
    * Returns a new constraint that if constraint <i>ct1</i> is true, then constraint <i>ct2</i> must also be true.
    *
    * @param ct1 is the first constraint
    * @param ct2 is the second constraint
    * @return the conditional constraint
    */
  def ifThen(ct1: Constraint, ct2: Constraint)(implicit modeler: Modeler): Constraint =
    modeler.ifThen(ct1, ct2)

  /**
    * Returns the maximum of a set of numeric expressions.
    *
    * @param exprs is a sequence of numeric variables
    * @return a numeric expression that represents the maximum of the numeric expressions
    */
  def max(exprs: NumExprArray)(implicit modeler: Modeler): NumExpr = modeler.max(exprs)

  /**
    * Returns the maximum of a set of numeric expressions.
    *
    * @param exprs is a sequence of numeric variables
    * @return a numeric expression that represents the maximum of the numeric expressions
    */
  def max(exprs: IntExprArray)(implicit modeler: Modeler): IntExpr = modeler.max(exprs)

  /**
    * Returns the maximum of a set of numeric expressions.
    *
    * @param exprs is an array of numeric expressions
    * @return a numeric expression that represents the maximum of the numeric expressions
    */
  def max(exprs: Iterable[NumExpr])(implicit modeler: Modeler): NumExpr = modeler.max(exprs)

  /**
    * Returns the maximum of a set of integer expressions.
    *
    * @param exprs is an array of integer expressions
    * @return a numeric expression that represents the maximum of the numeric expressions
    */
  def max(exprs: Iterable[IntExpr])(implicit modeler: Modeler): IntExpr = modeler.max(exprs)

  /**
    * Returns the maximum of a set of numeric expressions.
    *
    * @param exprs is an array of numeric expressions
    * @return a numeric expression that represents the maximum of the numeric expressions
    */
  def max(exprs: Array[NumExpr])(implicit modeler: Modeler): NumExpr = modeler.max(exprs)

  /**
    * Returns the maximum of a set of integer expressions.
    *
    * @param exprs is an array of integer expressions
    * @return a numeric expression that represents the maximum of the numeric expressions
    */
  def max(exprs: Array[IntExpr])(implicit modeler: Modeler): IntExpr = modeler.max(exprs)

  /**
    * Returns the maximum of a numeric expressions.
    *
    * @param exprs is a variable number of numeric variables
    * @return a numeric expression that represents the maximum of the numeric expressions
    */
  def max(exprs: NumExpr*)(implicit modeler: Modeler): NumExpr = modeler.max(exprs)

  /**
    * Returns the maximum of a numeric expressions.
    *
    * @param exprs is a variable number of numeric variables
    * @return a numeric expression that represents the maximum of the numeric expressions
    */
  def max(exprs: IntExpr*)(implicit modeler: Modeler): IntExpr = modeler.max(exprs)

  /**
    * Returns the minimum of a set of numeric expressions.
    *
    * @param exprs is a sequence of numeric variables
    * @return a numeric expression that represents the minimum of the numeric expressions
    */
  def min(exprs: NumExprArray)(implicit modeler: Modeler): NumExpr = modeler.min(exprs)

  /**
    * Returns the minimum of a set of integer expressions.
    *
    * @param exprs is an array of integer expressions
    * @return an integer expression that represents the minimum of the numeric expressions
    */
  def min(exprs: IntExprArray)(implicit modeler: Modeler): IntExpr = modeler.min(exprs)

  /**
    * Returns the minimum of a set of numeric expressions.
    *
    * @param exprs is an array of numeric variables
    * @return a numeric expression that represents the minimum of the numeric expressions
    */
  def min(exprs: Iterable[NumExpr])(implicit modeler: Modeler): NumExpr = modeler.min(exprs)

  /**
    * Returns the minimum of a numeric expressions.
    *
    * @param exprs is an array of integer expressions
    * @return an integer expression that represents the minimum of the integer expressions
    */
  def min(exprs: Iterable[IntExpr])(implicit modeler: Modeler): IntExpr = modeler.min(exprs)

  /**
    * Returns the minimum of a set of numeric expressions.
    *
    * @param exprs is an array of numeric variables
    * @return a numeric expression that represents the minimum of the numeric expressions
    */
  def min(exprs: Array[NumExpr])(implicit modeler: Modeler): NumExpr = modeler.min(exprs)

  /**
    * Returns the minimum of a numeric expressions.
    *
    * @param exprs is an array of integer expressions
    * @return an integer expression that represents the minimum of the integer expressions
    */
  def min(exprs: Array[IntExpr])(implicit modeler: Modeler): IntExpr = modeler.min(exprs)

  /**
    * Returns the minimum of a numeric expressions.
    *
    * @param exprs is a variable number of numeric variables
    * @return a numeric expression that represents the minimum of the numeric expressions
    */
  def min(exprs: NumExpr*)(implicit modeler: Modeler): NumExpr = modeler.min(exprs)

  /**
    * Returns the minimum of a numeric expressions.
    *
    * @param exprs is a variable number of numeric variables
    * @return a numeric expression that represents the minimum of the numeric expressions
    */
  def min(exprs: IntExpr*)(implicit modeler: Modeler): IntExpr = modeler.min(exprs)

  /**
    * Creates and returns an objective object to minimize the expression <em>expr</em>.
    *
    * @param expr is the expression to minimize
    * @return An objective object representing the objective to minimize
    */
  def minimize(expr: NumExpr)(implicit model: Modeler): Objective = model.minimize(expr)

  /**
    * Creates and returns an objective object to maximize the expression <em>expr</em>.
    *
    * @param expr is the expression to minimize
    * @return An objective object representing the objective to maximize
    */
  def maximize(expr: NumExpr)(implicit model: Modeler): Objective = model.maximize(expr)


}
