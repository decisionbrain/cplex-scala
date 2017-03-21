/*
 * Source file provided under Apache License, Version 2.0, January 2004,
 * http://www.apache.org/licenses/
 * (c) Copyright DecisionBrain SAS 2016,2017
 */

package com.decisionbrain.cplex.cp

/**
  * This class represents a segment of a piecewise linear function.
  *
  * @param start is the start of the segment
  * @param end is the end of the segment
  * @param valueLeft is the value of the function at the left-most point of the segment
  * @param valueRight is the value of the function at the right-most point of the segment
  * @tparam T is the type of the value
  */
class Segment[T <: AnyVal](val start: T, val end: T, val valueLeft: T, val valueRight: T) {

  /**
    * Converts the segment in a human readable format.
    *
    * @return a character string
    */
  override def toString(): String = {
    val builder = new StringBuilder
    builder.append("[").append(start).append(" .. ").append(end).append(") -> [").append(valueLeft).append(" .. ").append(valueRight).append(")")
    builder.toString
  }
}

/**
  * This class represent a segment of a step function, a cumul function or a state function.
  *
  * @param start is the start of the segment
  * @param end is the end of the segment
  * @param value is the value of the function on the segment
  * @tparam T is the type of the value
  */
class ConstantSegment[T <: AnyVal](start: T, end: T, val value: T) extends Segment[T](start, end, value, value) {
  /**
    * Converts the segment in a human readable format.
    *
    * @return a character string
    */
  override def toString(): String = {
    val builder = new StringBuilder
    builder.append("[").append(start).append(" .. ").append(end).append(") -> ").append(valueLeft)
    builder.toString
  }
}