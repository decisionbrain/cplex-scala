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
class Constraint(c: IloConstraint) extends Addable {
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
    * Return the CPLEX constraint.
    *
    * @return the CPLEX constraint
    */
  def getIloConstraint(): IloConstraint = c
}

object Constraint {
  def apply(c: IloConstraint) = new Constraint(c)
}
