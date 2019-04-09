package com.decisionbrain.cplex

import com.decisionbrain.cplex.cp.CumulFunctionExpr
import ilog.concert.IloModeler
import ilog.cp.IloCP
import ilog.cplex.IloCplex

class Modeler(name: String=null, modeler: IloModeler) {

  def getIloModeler(): IloModeler = modeler

  def toIloCplex: IloCplex = getIloModeler().asInstanceOf[IloCplex]

  def toIloCP: IloCP = getIloModeler().asInstanceOf[IloCP]


  def linearIntExpr(value: Int = 0): IntExpr =
    IntExpr(modeler.linearIntExpr(value))(implicitly(this))

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

  def le(expr: IntExpr, f: CumulFunctionExpr): Constraint =
    Constraint(toIloCP.le(expr.getIloIntExpr(), f.getIloCumulFunctionExpr()))(implicitly(this))

  def ge(expr: IntExpr, f: CumulFunctionExpr): Constraint =
    Constraint(toIloCP.ge(expr.getIloIntExpr(), f.getIloCumulFunctionExpr()))(implicitly(this))

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
