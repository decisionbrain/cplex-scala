/*
 * Source file provided under Apache License, Version 2.0, January 2004,
 * http://www.apache.org/licenses/
 * (c) Copyright DecisionBrain SAS 2016,2018
 */

package com.decisionbrain.cplex

import com.decisionbrain.cplex.cp.CpModel
import ilog.concert.{IloAddable, IloConstraint}

/**
  * Class for constraints.
  *
  * @param c is the CPLEX constraint
  */
class Constraint(c: IloConstraint)(implicit modeler: Modeler) extends IntExpr(c) with Addable {

  /**
    * Return the CPLEX constraint.
    *
    * @return the CPLEX constraint
    */
  def getIloConstraint(): IloConstraint = c

  /**
    * Returns the name of model addable object.
    *
    * @return the name of the object
    */
  override def getName(): Option[String] = Option(c.getName())

  /**
    * Set the name of the model addable object.
    *
    * @param name is the name of the object
    */
  override def setName(name: String): Unit = c.setName(name)

  /**
    * Returns the CPLEX addable object.
    *
    * @return the CPLEX addable object
    */
  override def getIloAddable(): IloAddable = c

  /**
    * Creates and return a logical or constraint between two constraints.
    *
    * @param ct is the other constraint of the logical or
    * @return a new constraint
    */
  def ||(ct: Constraint): Constraint = modeler.or(this, ct)

  /**
    * Creates and return a logical and constraint between two constraints.
    *
    * @param ct is the other constraint of the logical and
    * @return a new constraint
    */
  def &&(ct: Constraint): Constraint = modeler.and(this, ct)

  /**
    * Creates and returns a logical not constraint.
    *
    * @return a new constraint
    */
  def unary_!(): Constraint = modeler.not(this)

  /**
    * Creates and returns a logical imply constraint.
    *
    * @param ct is the other constraint
    * @return a new constraint
    */
  def <=(ct: Constraint): Constraint = modeler.ifThen(this, ct)

  /**
    * Creates and returns a logical if-then-else constraint.
    *
    * @param ct1 is the 'then' constraint
    * @param ct2 is the 'else' constraint
    * @return a new constraint
    */
  def ?(ct1: Constraint, ct2: Constraint): Constraint = {
    modeler match {
      case model: CpModel => model.ifThenElse(this, ct1, ct2)
      case _ => throw new UnsupportedOperationException("Operator \'?\' only supported on CpModel")
    }
  }
}

object Constraint {

  /**
    * Converts a CPLEX constraint to a constraint
    *
    * @param c is the CPLEX constraint
    * @return a constraint
    */
  def apply(c: IloConstraint)(implicit modeler: Modeler): Constraint = new Constraint(c)

}
