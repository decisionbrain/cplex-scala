/*
 * Source file provided under Apache License, Version 2.0, January 2004,
 * http://www.apache.org/licenses/
 * (c) Copyright DecisionBrain SAS 2016,2017
 */

package com.decisionbrain.cplex.cp

import ilog.concert.IloStateFunction

/**
  * Iterator on state function.
  *
  * @param f is the state function
  * @param model is the constraint programming model
  */
class StateFunctionIterator(f: StateFunction)(implicit model: CpModel) extends Iterator[Segment[Int]] {

  var cursor: Int = 0

  override def hasNext: Boolean = cursor < model.getNumberOfSegments(f)

  override def next(): Segment[Int] = {
    val segment = new Segment[Int](model.getSegmentStart(f, cursor),
      model.getSegmentEnd(f, cursor),
      model.getSegmentValue(f, cursor))
    cursor += 1
    segment
  }
}

/**
  * Constructor of class NumToNumStepFunction.
  *
  * @param f is a CPLEX step function
  * @param model is the constraint programming model
  */
class StateFunction(f: IloStateFunction)(implicit model: CpModel) extends Iterable[Segment[Int]] {

  /**
    * Returns the CPLEX step function
    *
    * @return the CPLEX step function
    */
  def getIloStateFunction(): IloStateFunction = f

  /**
    * Returns an iterator on the sate function. This member function assumes that the state function expression f is
    * fixed.
    *
    * @return an iterator on the state function expression
    */
  override def iterator: Iterator[Segment[Int]] = new StateFunctionIterator(this)
}

object StateFunction {
  /**
    * Converts a CPLEX state function to a state function.
    *
    * @param f     is the CPLEX state funciton
    * @param model is the constraint programming model the state function expression belongs to
    * @return a state function
    */
  def apply(f: IloStateFunction)(implicit model: CpModel) = new StateFunction(f)
}