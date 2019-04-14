package com.decisionbrain.cplex

import com.decisionbrain.cplex.Modeler._
import ilog.concert.{IloIntExpr, IloIntVar, IloModeler, IloNumExpr, IloNumVar}
import ilog.cp.IloCP
import ilog.cplex.IloCplex

import scala.reflect.ClassTag

/**
  * This class is for building optimization models and provides methods for constructing variables, expressions,
  * constraints and objectives.
  *
  * @param modeler is the CPLEX modeler.
  */
abstract class Modeler(modeler: IloModeler) {

  /**
    * Constructor for the Modeler
    *
    * @param name is the name of the optimization model
    * @param modeler is the CPLEX modeler
    */
  def this(name: String, modeler: IloModeler) {
    this(modeler)
    modeler.setName(name)
  }

  /**
    * Returns the name of the optimization model
    *
    * @return the name of the optimization model
    */
  def getName(): Option[String] = Option(modeler.getName())

  /**
    * Returns the CPLEX modeler i.e the interface for building optimization models.
    *
    * @return the CPLEX modeler
    */
  def getIloModeler(): IloModeler = modeler

  /**
    * Returns the CPLEX object to solve the mathematical programming model
    *
    * TODO: move to MpModel
    *
    * @return
    */
  def toIloCplex: IloCplex = getIloModeler().asInstanceOf[IloCplex]

  /**
    * Returns the CPLEX CP Optimizer
    *
    * TODO: move to CpModel
    *
    * @return
    */
  def toIloCP: IloCP = getIloModeler().asInstanceOf[IloCP]

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
    * @param namer is a function that is used to set the name of a numeric variable
    * @tparam T it the type of the elements in the set
    * @return a dictionary of numeric variables indexed by the element of the set
    */
  def numVars[T](set: Iterable[T],
                 lb: Double = 0.0,
                 ub: Double = Double.MaxValue,
                 namer: (T) => String = (t: T) => "") : Map[T, NumVar] = {
    val dict: Map[T, NumVar] = set.map(t => {
      val v: NumVar = NumVar(modeler.numVar(lb, ub, namer(t)))(implicitly(this))
      (t, v)
    })(collection.breakOut)
    dict
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
    * @param namer is a function that is used to set the name of a numeric variable
    * @tparam T it the type of the elements in the set
    * @return a dictionary of numeric variables indexed by the element of the set
    */
  def intVars[T](set: Iterable[T],
                 min: Int = 0,
                 max: Int= Int.MaxValue,
                 namer: (T) => String = (t: T) => "") : Map[T, IntVar] = {
    val dict: Map[T, IntVar] = set.map(t => {
      val v: IntVar = IntVar(modeler.intVar(min, max, namer(t)))(implicitly(this))
      (t, v)
    })(collection.breakOut)
    dict
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
    * @param namer is a function that is used to give a name to the variables
    * @return a map of binary variables
    */
  def boolVars[T](keys: Iterable[T], namer: (T) => String) : Map[T, NumVar] = {
    (for (t <- keys) yield {
      val v: NumVar = NumVar(modeler.boolVar())(implicitly(this))
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
      val v: NumVar = NumVar(modeler.boolVar())(implicitly(this))
      v.setName(namer(t, u))
      (t,u) -> v
    })(collection.breakOut)
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
    * Creates and returns an integer linear expression representing the scalar product of the given integer values
    * with the given integer variables.
    *
    * @param values is the sequence of values
    * @param vars is the sequence of variables
    * @return the scalar product of integer values with integer variables
    */
  def scalProd(values: IntArray, vars: IntVarArray): IntExpr =
    IntExpr(modeler.scalProd(vars.toIloArray, values.toArray))(implicitly(this))

  @deprecated("Replaced by method scalProd", "decisionbrain-cplex-scala-1.5.0")
  def scalarProduct(values: IntArray, vars: IntVarArray): IntExpr =
    IntExpr(modeler.scalProd(vars.toIloArray, values.toArray))(implicitly(this))

  /**
    * Creates and returns an integer linear expression representing the scalar product of the given integer values
    * with the given integer variables.
    *
    * @param values is the sequence of values
    * @param vars is the sequence of variables
    * @return the scalar product of integer values with integer variables
    */
  def scalProd(vars: IntVarArray, values: IntArray): IntExpr =
    IntExpr(modeler.scalProd(values.toArray, vars.toIloArray))(implicitly(this))

  @deprecated("Replaced by method scalProd", "decisionbrain-cplex-scala-1.5.0")
  def scalarProduct(vars: IntVarArray, values: IntArray): IntExpr =
    IntExpr(modeler.scalProd(values.toArray, vars.toIloArray))(implicitly(this))

  /**
    * Creates and returns an integer linear expression representing the scalar product of the given integer values
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
    * Creates and returns an integer linear expression representing the scalar product of the given integer values
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
    * Creates and returns an linear expression representing the scalar product of the numeric values
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
    * Creates and returns an linear expression representing the scalar product of the numeric values
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
    * Creates and returns an integer linear expression representing the scalar product of the given integer values
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
    * Creates and returns an integer linear expression representing the scalar product of the given integer values
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
    * Creates and returns an integer linear expression representing the scalar product of the given integer values
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
    * Return the sum of numeric expressions.
    *
    * @param exprs is a sequence of numeric expressions
    * @return a numeric expression that represents the sum of numeric expressions
    */
  def sum(exprs: NumExpr*): NumExpr = {
    NumExpr(modeler.sum(exprs.map(e => e.getIloNumExpr).toArray))(implicitly(this))
  }

  /**
    * Return the sum of integer expressions.
    *
    * @param exprs is a sequence of integer expressions
    * @return a numeric expression that represents the sum of numeric expressions
    */
  def sum(exprs: IntExpr*): IntExpr = {
    IntExpr(modeler.sum(exprs.map(e => e.getIloIntExpr).toArray))(implicitly(this))
  }

  /**
    * Return the sum of numeric expressions.
    *
    * @param exprs is a sequence of numeric variables
    * @return a numeric expression that represents the sum of the numeric expressions
    */
  def sum(exprs: Iterable[NumExpr]) : NumExpr = {
    NumExpr(modeler.sum(exprs.map(e => e.getIloNumExpr).toArray))(implicitly(this))
  }

  /**
    * Return the sum of numeric expressions.
    *
    * @param exprs is a sequence of numeric variables
    * @return a numeric expression that represents the sum of the numeric expressions
    */
  def sum(exprs: Iterable[IntExpr]) : IntExpr = {
    IntExpr(modeler.sum(exprs.map(e => e.getIloIntExpr).toArray))(implicitly(this))
  }

  /**
    * Return the sum of numeric expressions.
    *
    * @param exprs is a sequence of numeric variables
    * @return a numeric expression that represents the sum of the numeric expressions
    */
  def sum(exprs: NumExprArray) : NumExpr = {
    NumExpr(modeler.sum(exprs.toIloArray))(implicitly(this))
  }

  /**
    * Returns the integer sum of integer expressions.
    *
    * @param exprs is an array of integer expressions
    * @return an integer expression that represents the sum of the integer expressions
    */
  def sum(exprs: IntExprArray) : IntExpr = {
    IntExpr(modeler.sum(exprs.toIloArray))(implicitly(this))
  }

  /**
    * Returns the sum of a numeric expression and a double value.
    *
    * @param expr is the numeric expression
    * @param v is the value
    * @return a numeric expression that is the sum of a numeric expression and a double value
    */
  def sum(expr: NumExpr, v: Double): NumExpr =
    NumExpr(modeler.sum(expr.getIloNumExpr(), v))(implicitly(this))

  /**
    * Returns the sum of an integer expression and an integer value.
    *
    * @param expr is the integer expression
    * @param v is the integer value
    * @return a integer expression that is the sum of a numeric expression and a double value
    */
  def sum(expr: IntExpr, v: Int): IntExpr =
    IntExpr(modeler.sum(expr.getIloIntExpr(), v))(implicitly(this))

  def diff(expr1: NumExpr, expr2: NumExpr): NumExpr =
    NumExpr(modeler.diff(expr1.getIloNumExpr(), expr2.getIloNumExpr()))(implicitly(this))

  def diff(expr1: NumExpr, v: Double): NumExpr =
    NumExpr(modeler.diff(expr1.getIloNumExpr(), v))(implicitly(this))

  def diff(expr1: IntExpr, expr2: IntExpr): IntExpr =
    IntExpr(modeler.diff(expr1.getIloIntExpr(), expr2.getIloIntExpr()))(implicitly(this))

  def diff(expr1: IntExpr, v: Int): IntExpr =
    IntExpr(modeler.diff(expr1.getIloIntExpr(), v))(implicitly(this))

  def negative(expr: NumExpr): NumExpr =
    NumExpr(modeler.negative(expr.getIloNumExpr()))(implicitly(this))

  def negative(expr: IntExpr): IntExpr =
    IntExpr(modeler.negative(expr.getIloIntExpr()))(implicitly(this))

  def prod(expr1: NumExpr, expr2: NumExpr): NumExpr =
    NumExpr(modeler.prod(expr1.getIloNumExpr(), expr2.getIloNumExpr()))(implicitly(this))

  def prod(expr: NumExpr, v: Double): NumExpr =
    NumExpr(modeler.prod(expr.getIloNumExpr(), v))(implicitly(this))

  def prod(expr1: IntExpr, expr2: IntExpr): IntExpr =
    IntExpr(modeler.prod(expr1.getIloIntExpr(), expr2.getIloIntExpr()))(implicitly(this))

  def prod(expr: IntExpr, v: Int): IntExpr =
    IntExpr(modeler.prod(expr.getIloIntExpr(), v))(implicitly(this))

  def div(expr1: IntExpr, expr2: IntExpr): IntExpr =
    IntExpr(toIloCP.div(expr1.getIloIntExpr(), expr2.getIloIntExpr()))(implicitly(this))

  def div(expr: IntExpr, v: Int): IntExpr =
    IntExpr(toIloCP.div(expr.getIloIntExpr(), v))(implicitly(this))

  def ge(expr1: NumExpr, expr2: NumExpr): Constraint =
    Constraint(modeler.ge(expr1.getIloNumExpr(), expr2.getIloNumExpr()))(implicitly(this))

  def ge(expr: NumExpr, v: Double): Range =
    Range(modeler.ge(expr.getIloNumExpr(), v))(implicitly(this))

  def le(expr1: NumExpr, expr2: NumExpr): Constraint =
    Constraint(modeler.le(expr1.getIloNumExpr(), expr2.getIloNumExpr()))(implicitly(this))

  def le(expr: NumExpr, v: Double): Range =
    Range(modeler.le(expr.getIloNumExpr(), v))(implicitly(this))

  def eq(expr1: NumExpr, expr2: NumExpr): Constraint =
    Constraint(modeler.eq(expr1.getIloNumExpr(), expr2.getIloNumExpr()))(implicitly(this))

  def eq(expr1: NumExpr, expr2: NumExpr, name: String): Constraint =
    Constraint(modeler.eq(expr1.getIloNumExpr(), expr2.getIloNumExpr(), name))(implicitly(this))

  def eq(expr: NumExpr, v: Double): Range =
    Range(modeler.eq(expr.getIloNumExpr(), v))(implicitly(this))

  def gt(expr1: IntExpr, expr2: IntExpr): Constraint =
    Constraint(toIloCP.gt(expr1.getIloIntExpr(), expr2.getIloIntExpr()))(implicitly(this))

  def gt(expr: IntExpr, value: Int): Constraint =
    Constraint(toIloCP.gt(expr.getIloIntExpr(), value))(implicitly(this))

  def lt(expr1: IntExpr, expr2: IntExpr): Constraint =
    Constraint(toIloCP.lt(expr1.getIloIntExpr(), expr2.getIloIntExpr()))(implicitly(this))

  def lt(expr: IntExpr, value: Int): Constraint =
    Constraint(toIloCP.lt(expr.getIloIntExpr(), value))(implicitly(this))

  def neq(expr1: IntExpr, expr2: IntExpr): Constraint =
    Constraint(toIloCP.neq(expr1.getIloIntExpr(), expr2.getIloIntExpr()))(implicitly(this))

  def neq(expr: IntExpr, v: Int): Constraint =
    Constraint(toIloCP.neq(expr.getIloIntExpr(), v))(implicitly(this))

  def or(ct1: Constraint, ct2: Constraint): Constraint =
    Constraint(modeler.or(ct1.getIloConstraint(), ct2.getIloConstraint()))(implicitly(this))

  def and(ct1: Constraint, ct2: Constraint): Constraint =
    Constraint(modeler.and(ct1.getIloConstraint(), ct2.getIloConstraint()))(implicitly(this))

  def not(ct: Constraint): Constraint =
    Constraint(modeler.not(ct.getIloConstraint()))(implicitly(this))

  def ifThen(ct1: Constraint, ct2: Constraint): Constraint =
    Constraint(modeler.ifThen(ct1.getIloConstraint(), ct2.getIloConstraint()))(implicitly(this))

  def ifThenElse(ct: Constraint, ct1: Constraint, ct2: Constraint): Constraint =
    Constraint(toIloCP.ifThenElse(ct.getIloConstraint(), ct1.getIloConstraint(), ct2.getIloConstraint()))(implicitly(this))


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
  def getNumExprNumeric(): Numeric[NumExpr] = numExprNumeric

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
  def getIntExprNumeric(): Numeric[IntExpr] = intExprNumeric

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
    def element(expr: IntExpr): IntExpr =
      IntExpr(modeler.toIloCP.element(this.toIloArray, expr.getIloIntExpr()))(implicitly(modeler))

    /**
      * Method get creates and returns a new integer expression equal to exprs[index] where index is an integer
      * expression.
      *
      * @param expr is the integer expression for the index
      * @return an new integer expression
      */
    def apply(expr: IntExpr): IntExpr =
      IntExpr(modeler.toIloCP.element(this.toIloArray, expr.getIloIntExpr()))(implicitly(modeler))

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
    def toSearchPhase: SearchPhase = SearchPhase(modeler.toIloCP.searchPhase(vars.map(v => v.getIloIntVar()).toArray))(implicitly(modeler))

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
    def element(expr: IntExpr): NumExpr =
      NumExpr(modeler.toIloCP.element(this.toArray, expr.getIloIntExpr()))(implicitly(modeler))

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
    def element(expr: IntExpr): IntExpr =
      IntExpr(modeler.toIloCP.element(values.toArray, expr.getIloIntExpr()))(implicitly(modeler))

    /**
      * Method get creates and returns a new integer expression equal to exprs[index] where index is an integer
      * expression.
      *
      * @param expr is the integer expression for the index
      * @return an new integer expression
      */
    def apply(expr: IntExpr): IntExpr =
      IntExpr(modeler.toIloCP.element(values.toArray, expr.getIloIntExpr()))(implicitly(modeler))

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
}
