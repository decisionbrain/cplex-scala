/*
 * Source file provided under Apache License, Version 2.0, January 2004,
 * http://www.apache.org/licenses/
 * (c) Copyright DecisionBrain SAS 2016,2018
 */

package com.decisionbrain.cplex.cp

import com.decisionbrain.cplex.Addable
import ilog.concert.{IloAddable, IloCumulFunctionExpr, IloIntervalSequenceVar}

/**
  * Iterator on sequence variable .
  *
  * @param seq is the sequence variable
  * @param model is the constraint programming model
  */
class IntervalSequenceVarIterator(seq: IntervalSequenceVar)(implicit model: CpModel) extends Iterator[IntervalVar] {

  var cursor = model.getFirst(seq)

  override def hasNext: Boolean =
    !cursor.getIloIntervalVar().equals(model.getLast(seq).getIloIntervalVar())

  override def next(): IntervalVar = {
    cursor = model.getNext(seq, cursor)
    cursor
  }
}

/**
  * Class for numeric expressions
  *
  * @param expr  is the numeric expression
  * @param model is the constraint programming model
  */
class IntervalSequenceVar(val expr: IloIntervalSequenceVar)(implicit model: CpModel) extends Addable
  with Iterable[IntervalVar] {

  /**
    * Returns the constraint programming model
    *
    * @return the constraint programming model
    */
  def getCpModel(): CpModel = model

  /**
    * Return the CPLEX numeric expression.
    *
    * @return the numeric expression
    */
  def getIloIntervalSequenceVar(): IloIntervalSequenceVar = expr

  /**
    * Return a character string that represents the numeric expression.
    *
    * @return a character string
    */
  override def toString: String = expr.toString()

  /**
    * Returns the name of model addable object.
    *
    * @return the name of the object
    */
  override def getName: Option[String] = Option(expr.getName())

  /**
    * Set the name of the model addable object.
    *
    * @param name is the name of the object
    */
  override def setName(name: String): Unit = expr.setName(name)

  /**
    * Returns the CPLEX addable object.
    *
    * @return the CPLEX addable object
    */
  override def getIloAddable(): IloAddable = expr

  /**
    * Returns an iterator on the sequencce variable. This member function assumes that the sequence variable is
    * fixed.
    *
    * @return an iterator on the sequence variable
    */
  def iterator(): IntervalSequenceVarIterator = new IntervalSequenceVarIterator(this)

}

object IntervalSequenceVar {
  /**
    * Converts a CPLEX cumul function expression to a cumul function expression.
    *
    * @param e     is the CPLEX cumul funciton expression
    * @param model is the constraint programming model the cumul function expression belongs to
    * @return a cumul function expression
    */
  def apply(e: IloIntervalSequenceVar)(implicit model: CpModel) = new IntervalSequenceVar(e)
}

