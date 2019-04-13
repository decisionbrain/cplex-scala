package com.decisionbrain.cplex

import com.decisionbrain.cplex.Modeler._
import com.decisionbrain.cplex.cp.CpModel
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
class Modeler(modeler: IloModeler) {

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
    * @return
    */
  def toIloCplex: IloCplex = getIloModeler().asInstanceOf[IloCplex]

  /**
    * Returns the CPLEX CP Optimizer
    *
    * @return
    */
  def toIloCP: IloCP = getIloModeler().asInstanceOf[IloCP]

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
    def *(values: NumArray): NumExpr = modeler.scalarProduct(this, values)

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
    def *(values: IntArray): IntExpr = modeler.scalarProduct(this, values)

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
    def *(vars: NumVarArray): NumExpr = modeler.scalarProduct(this, vars)

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
    def *(vars: IntVarArray): IntExpr = modeler.scalarProduct(this, vars)

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
  def scalarProduct(values: IntArray, vars: IntVarArray)(implicit modeler: Modeler): IntExpr =
    modeler.scalarProduct(values, vars)

  /**
    * Creates and returns an integer linear expression representing the scalar product of the given integer values
    * with the given integer variables.
    *
    * @param values is the sequence of values
    * @param vars is the sequence of variables
    * @return the scalar product of integer values with integer variables
    */
  def scalarProduct(vars: IntVarArray, values: IntArray)(implicit modeler: Modeler): IntExpr =
    modeler.scalarProduct(values, vars)

  /**
    * Creates and returns an integer linear expression representing the scalar product of the given integer values
    * with the given integer variables.
    *
    * @param values is the sequence of values
    * @param vars is the sequence of variables
    * @return the scalar product of integer values with integer variables
    */
  def scalarProduct(values: Array[Int], vars: Array[IntVar])(implicit modeler: Modeler): IntExpr =
    modeler.scalarProduct(values, vars)

  /**
    * Creates and returns an integer linear expression representing the scalar product of the given integer values
    * with the given integer variables.
    *
    * @param values is the sequence of values
    * @param vars is the sequence of variables
    * @return the scalar product of integer values with integer variables
    */
  def scalarProduct(vars: Array[IntVar], values: Array[Int])(implicit modeler: Modeler): IntExpr =
    modeler.scalarProduct(vars, values)

  /**
    * Creates and returns an linear expression representing the scalar product of the numeric values
    * with the given numeric variables.
    *
    * @param values is the sequence of values
    * @param vars is the sequence of variables
    * @return the scalar product of numeric values with numerci variables
    */
  def scalarProduct(values: NumArray, vars: NumVarArray)(implicit modeler: Modeler): NumExpr =
    modeler.scalarProduct(values, vars)

  /**
    * Creates and returns an linear expression representing the scalar product of the numeric values
    * with the given numeric variables.
    *
    * @param values is the sequence of values
    * @param vars is the sequence of variables
    * @return the scalar product of numeric values with numerci variables
    */
  def scalarProduct(vars: NumVarArray, values: NumArray)(implicit modeler: Modeler): NumExpr =
    modeler.scalarProduct(vars, values)

  /**
    * Creates and returns an integer linear expression representing the scalar product of the given integer values
    * with the given integer variables.
    *
    * @param values is the sequence of values
    * @param vars is the sequence of variables
    * @return the scalar product of integer values with integer variables
    */
  def scalarProduct(values: Array[Double], vars: Array[NumVar])(implicit modeler: Modeler): NumExpr =
    modeler.scalarProduct(vars, values)

  /**
    * Creates and returns an integer linear expression representing the scalar product of the given integer values
    * with the given integer variables.
    *
    * @param values is the sequence of values
    * @param vars is the sequence of variables
    * @return the scalar product of integer values with integer variables
    */
  def scalarProduct(vars: Array[NumVar], values: Array[Double])(implicit modeler: Modeler): NumExpr =
    modeler.scalarProduct(vars, values)

  /**
    * Creates and returns an integer linear expression representing the scalar product of the given integer values
    * with the given integer variables.
    *
    * @param values is the sequence of values
    * @param vars is the sequence of variables
    * @return the scalar product of integer values with integer variables
    */
  def scalarProduct(values: Iterable[Double], vars: Iterable[NumVar])(implicit modeler: Modeler): NumExpr =
    modeler.scalarProduct(values, vars)

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
