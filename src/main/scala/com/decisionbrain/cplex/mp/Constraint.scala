/*
 * Source file provided under Apache License, Version 2.0, January 2004,
 * http://www.apache.org/licenses/
 * (c) Copyright DecisionBrain SAS 2016,2018
 */

package com.decisionbrain.cplex.mp

import com.decisionbrain.cplex.Addable
import ilog.concert.{IloAddable, IloConstraint}

/**
  * Class for constraints.
  *
  * @param c is the CPLEX constraint
  */
class Constraint(c: IloConstraint)(implicit model: MpModel) extends Addable {
  /**
    * Returns the name of the constraint.
    *
    * @return the name of the constraint
    */
  override def getName: Option[String] = Option(c.getName())

  /**
    * Set the name of the constraint.
    *
    * @param name is the name of the constraint
    */
  override def setName(name: String): Unit = c.setName(name)

  /**
    * Returns the CPLEX constraint
    *
    * @return the CPLEX constraint
    */
  override def getIloAddable(): IloAddable = c

  /**
    * Creates and return a logical or constraint between two constraints.
    *
    * @param ct is the other constraint of the logical or
    * @return a new constraint
    */
  def ||(ct: Constraint): Constraint = {
    Constraint(model.cplex.or(this.getIloConstraint(), ct.getIloConstraint))
  }

  /**
    * Creates and return a logical and constraint between two constraints.
    *
    * @param ct is the other constraint of the logical and
    * @return a new constraint
    */
  def &&(ct: Constraint): Constraint = {
    Constraint(model.cplex.and(this.getIloConstraint(), ct.getIloConstraint))
  }

  /**
    * Creates and returns a logical not constraint.
    *
    * @return a new constraint
    */
  def unary_!(): Constraint = {
    Constraint(model.cplex.not(this.getIloConstraint()))
  }

  /**
    * Creates and returns a logical imply constraint.
    *
    * @param ct is the other constraint
    * @return a new constraint
    */
  def <=(ct: Constraint): Constraint = {
    Constraint(model.cplex.ifThen(this.getIloConstraint(), ct.getIloConstraint))
  }

  /**
    * Return the CPLEX constraint.
    *
    * @return the CPLEX constraint
    */
  def getIloConstraint(): IloConstraint = c
}

object Constraint {

  /**
    * Converts a CPLEX constraint to a constraint
    *
    * @param c is the CPLEX constraint
    * @return a constraint
    */
  def apply(c: IloConstraint)(implicit model: MpModel) = new Constraint(c)
}
