/*
 * Source file provided under Apache License, Version 2.0, January 2004,
 * http://www.apache.org/licenses/
 * (c) Copyright DecisionBrain SAS 2016,2018
 */

package com.decisionbrain.cplex

import ilog.concert._

/**
  * Constructor of class Objective.
  *
  * @param o is the CPLEX objective
  * @param modeler is the constraint programming model
  */
class Objective(o: IloObjective)(implicit modeler: Modeler) extends Addable {

  /**
    * Returns the name of model addable object.
    *
    * @return the name of the object
    */
  override def getName(): Option[String] = Option(o.getName())

  /**
    * Set the name of the model addable object.
    *
    * @param name is the name of the object
    */
  override def setName(name: String): Unit = o.setName(name)

  /**
    * Returns the CPLEX addable object
    *
    * @return the CPLEX addable object
    */
  override def getIloAddable(): IloAddable = o

  /**
    * Returns the CPLEX objective.
    *
    * @return the CPLEX objective
    */
  def getIloObjective(): IloObjective = o

  /**
    * Returns the numeric expression of the objective.
    *
    * @return return the numeric expression of the objective
    */
  def getNumExpr(): NumExpr = NumExpr(o.getExpr)(implicitly(modeler))

  @deprecated("Replaced by method getNumExpr", "decisionbrain-cplex-scala-1.0.0")
  def getExpr(): IloNumExpr = o.getExpr

  /**
    * Sets the numeric expression of the objective.
    *
    * @param expr is the numeric expression
    */
  def setNumExpr(expr: NumExpr) = o.setExpr(expr.getIloNumExpr)

  /**
    * Clear the numeric expression.
    */
  def clearExpr() = o.clearExpr()

  /**
    * Returns the sense of the objective.
    *
    * @return the sense of the objective
    */
  def getSense(): ObjectiveSense = ObjectiveSense.fromInt(o.getSense.getValue)

  /**
    * Sets the sense of the objective i.e. either Minimize or Maximize.
    *
    * @param sense is the sense of the objective
    */
  def setSense(sense: ObjectiveSense) = o.setSense(sense.toIloObjectiveSense)

  /**
    * Convert the objective to a character string.
    *
    * @return a character string
    */
  override def toString: String = o.toString
}

object Objective {
  def apply(o: IloObjective)(implicit model: Modeler) = new Objective(o)
}