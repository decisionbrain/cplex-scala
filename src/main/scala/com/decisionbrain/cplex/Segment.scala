/*
 * Source file provided under Apache License, Version 2.0, January 2004,
 *  http://www.apache.org/licenses/
 *  (c) Copyright DecisionBrain SAS 2016,2020
 *
 */

package com.decisionbrain.cplex

/**
  * This class represents a numeric segment of a piecewise linear function.
  *
  * @param start is the start of the segment
  * @param end is the end of the segment
  * @param valueLeft is the value of the function at the left-most point of the segment
  * @param valueRight is the value of the function at the right-most point of the segment
  */
case class NumSegment(start: Double, end: Double, valueLeft: Double, valueRight: Double) {

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
  * This class represent a numeric step of a step function
  *
  * @param start is the start of the segment
  * @param end is the end of the segment
  * @param value is the value of the function on the segment
  */
case class NumStep(start: Double, end: Double, value: Double) {
  /**
    * Converts the segment in a human readable format.
    *
    * @return a character string
    */
  override def toString(): String = {
    val builder = new StringBuilder
    builder.append("[").append(start).append(" .. ").append(end).append(") -> ").append(value)
    builder.toString
  }
}

/**
  * This class represent a step of a cumul function or a state function.
  *
  * @param start is the start of the segment
  * @param end is the end of the segment
  * @param value is the value of the function on the segment
  */
case class IntStep(start: Int, end: Int, value: Int) {
  /**
    * Converts the segment in a human readable format.
    *
    * @return a character string
    */
  override def toString(): String = {
    val builder = new StringBuilder
    builder.append("[").append(start).append(" .. ").append(end).append(") -> ").append(value)
    builder.toString
  }
}
