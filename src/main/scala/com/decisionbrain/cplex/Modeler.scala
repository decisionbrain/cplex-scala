package com.decisionbrain.cplex

import com.decisionbrain.cplex.Modeler.{IntExprArray, NumExprArray}
import ilog.concert.{IloIntExpr, IloModeler, IloNumExpr}
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
    * Return the sum of numeric expressions.
    *
    * @param exprs is a sequence of numeric expressions
    * @return a numeric expression that represents the sum of numeric expressions
    */
  def sum(exprs: NumExpr*): NumExpr = {
    NumExpr(modeler.sum(exprs.map(e => e.getIloNumExpr).toArray))(implicitly(this))
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
    * Return the sum of integer expressions.
    *
    * @param exprs is a sequence of integer variables
    * @return a numeric expression that represents the sum of the integer expressions
    */
  def sum(exprs: Array[IntExpr]) : IntExpr = {
    IntExpr(modeler.sum(exprs.map(e => e.getIloIntExpr)))(implicitly(this))
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
  def sumi(exprs: IntExprArray) : IntExpr = {
    IntExpr(modeler.sum(exprs.toIloArray))(implicitly(this))
  }

  /**
    * Return the sum of numeric expressions.
    *
    * @param exprs is a sequence of numeric variables
    * @return a numeric expression that represents the sum of the numeric expressions
    */
  def sum(exprs: Array[NumExpr]) : NumExpr = {
    NumExpr(modeler.sum(exprs.map(e => e.getIloNumExpr())))(implicitly(this))
  }


  def sum(expr1: NumExpr, expr2: NumExpr): NumExpr =
    NumExpr(modeler.sum(expr1.getIloNumExpr(), expr2.getIloNumExpr()))(implicitly(this))

  def sum(expr1: NumExpr, v: Double): NumExpr =
    NumExpr(modeler.sum(expr1.getIloNumExpr(), v))(implicitly(this))

  def sum(expr1: IntExpr, expr2: IntExpr): IntExpr =
    IntExpr(modeler.sum(expr1.getIloIntExpr(), expr2.getIloIntExpr()))(implicitly(this))

  def sum(expr1: IntExpr, v: Int): IntExpr =
    IntExpr(modeler.sum(expr1.getIloIntExpr(), v))(implicitly(this))

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
  def sumi(exprs: IntExprArray)(implicit modeler: Modeler): IntExpr = modeler.sumi(exprs)

  /**
    * Return the sum of a set of numeric expressions.
    *
    * @param exprs is a sequence of numeric variables
    * @return a numeric expression that represents the sum of the numeric expressions
    */
  def sum(exprs: Array[NumExpr])(implicit modeler: Modeler): NumExpr = modeler.sum(exprs)

  /**
    * Return the integer sum of a sequence of integer expressions.
    *
    * @param exprs is a sequence of integer variables
    * @return a integer expression that represents the sum of the integer expressions
    */
  def sum(exprs: Array[IntExpr])(implicit modeler: Modeler): IntExpr = modeler.sum(exprs)

  /**
    * Return the sum of a set of numeric expressions.
    *
    * @param exprs is a sequence of numeric variables
    * @return a numeric expression that represents the sum of the numeric expressions
    */
  def sum(exprs: NumExpr*)(implicit modeler: Modeler): NumExpr = modeler.sum(exprs)

  /**
    * Return the sum of numeric expressions.
    *
    * @param exprs is a sequence of numeric variables
    * @return a numeric expression that represents the sum of the numeric expressions
    */
  def sum(exprs: Iterable[NumExpr])(implicit modeler: Modeler) : NumExpr = modeler.sum(exprs)

}
