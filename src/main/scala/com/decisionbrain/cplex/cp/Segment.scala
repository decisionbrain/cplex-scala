/*
 * Source file provided under Apache License, Version 2.0, January 2004,
 * http://www.apache.org/licenses/
 * (c) Copyright DecisionBrain SAS 2016,2017
 */

package com.decisionbrain.cplex.cp

/**
  * This class represents a segment of a function.
  *
  * @param start is the start of the segment
  * @param end is the end of the segment
  * @param valueLeft is the value of the function at the left-most point of the segment
  * @param valueRight is the value of the function at the right-most point of the segment
  * @tparam T is the type of the value
  */
class Segment[T](val start: T, val end: T, val valueLeft: T, val valueRight: T) {

  /**
    * Constructor of a segment for a step function
    *
    * @param start is the start of the segment
    * @param end is the end of the segment
    * @param value is the value of the function on the segment
    */
  def this(start: T, end: T, value: T) = {
    this(start, end, value, value)
  }

  /**
    * Returns the value of the step function on the segment. Throws an exceptoin if it is not a segment of a step
    * function
    *
    * @return
    */
  def value = {
    if (valueLeft != valueRight) throw new RuntimeException("Not a step function")
    valueLeft
  }

  /**
    * Converts the segment in a human readable format.
    *
    * @return a character string
    */
  override def toString(): String = {
    val builder = new StringBuilder
    if (valueLeft == valueRight)
      builder.append("[").append(start).append(" .. ").append(end).append(") -> ").append(valueLeft)
    else
      builder.append("[").append(start).append(" .. ").append(end).append(") -> [").append(valueLeft).append(" .. ").append(valueRight).append(")")
    builder.toString
  }
}