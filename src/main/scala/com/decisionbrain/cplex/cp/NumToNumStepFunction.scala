/*
 * Source file provided under Apache License, Version 2.0, January 2004,
 * http://www.apache.org/licenses/
 * (c) Copyright DecisionBrain SAS 2016,2017
 */

package com.decisionbrain.cplex.cp

import com.decisionbrain.cplex.cp.CpModel.NumArray
import ilog.concert.IloNumToNumStepFunction
import ilog.concert.cppimpl.IloConcertUtils

/**
  * Constructor of class NumToNumStepFunction.
  *
  * @param f is a CPLEX step function
  * @param model is the constraint programming model
  */
class NumToNumStepFunction(f: IloNumToNumStepFunction)(implicit model: CpModel) {

  /**
    * Returns the CPLEX step function
    *
    * @return the CPLEX step function
    */
  def getIloNumToNumStepFunction(): IloNumToNumStepFunction = f

  /**
    *
    * @return
    */
  def getDefinitionIntervalMin(): Double = f.getDefinitionIntervalMin

  /**
    *
    * @return
    */
  def getDefinitionIntervalMax(): Double = f.getDefinitionIntervalMax

  /**
    * This member function returns the value of the invoking step function at x.
    *
    * @return the value of the step function at the given point
    */
  def getValue(x: Double): Double = f.getValue(x)

  def getMin(x1: Double, x2: Double): Double = f.getMin(x1, x2)

  def getMax(x1: Double, x2: Double): Double = f.getMax(x1, x2)

  /**
    * This member function returns the sum of the invoking step function on the interval [x1, x2).
    *
    * @param x1 is the start of the interval
    * @param x2 is the end of the interval
    * @return
    */
  def getArea(x1: Double, x2: Double): Double = f.getArea(x1, x2)

  /**
    * This member function sets the value of the invoking step function to be v on the interval [x1, x2).
    *
    * @param x1 is the start of the interval
    * @param x2 the end of the interval
    * @param v is the value on the interval
    */
  def setValue(x1: Double, x2: Double, v: Double): Unit = f.setValue(x1, x2, v)

  def addValue(x1: Double, x2: Double, v: Double): Unit = f.addValue(x1, x2, v)

  def add(f: NumToNumStepFunction): Unit = this.f.add(f.getIloNumToNumStepFunction())

  def setMin(f: NumToNumStepFunction): Unit = this.f.setMin(f.getIloNumToNumStepFunction())
  def setMin(x1: Double, x2: Double, v: Double): Unit = f.setMin(x1, x2, v)

  def setMax(f: NumToNumStepFunction): Unit = this.f.setMax(f.getIloNumToNumStepFunction())
  def setMax(x1: Double, x2: Double, v: Double): Unit  = f.setMin(x1, x2, v)

  def dilate(k: Double): Unit = f.dilate(k)

  def setSteps(x: NumArray, v: NumArray): Unit = {
    setSteps(x.toArray, v.toArray)
  }

  def setSteps(x: Array[Double], v: Array[Double]): Unit = {
    val xa = IloConcertUtils.ToCppIloNumArray(model.cp.getEnvImpl, x)
    val va = IloConcertUtils.ToCppIloNumArray(model.cp.getEnvImpl, v)
    f.setSteps(xa, va)
  }

  def setPeriodic(f: IloNumToNumStepFunction, x0: Double, n: Double, dval: Double): Unit =
    this.f.setPeriodic(f, x0, n, dval)

  def setPeriodic(f: IloNumToNumStepFunction, x0: Double, n: Double): Unit =
    this.f.setPeriodic(f, x0, n)

  def setPeriodic(f: IloNumToNumStepFunction, x0: Double): Unit =
    this.f.setPeriodic(f, x0)

  def setPeriodicValue(x1: Double, x2: Double, f: NumToNumStepFunction, offset: Double): Unit =
    this.f.setPeriodicValue(x1, x2, f.getIloNumToNumStepFunction(), offset)

  def setPeriodicValue(x1: Double, x2: Double, f: NumToNumStepFunction): Unit =
    this.f.setPeriodicValue(x1, x2, f.getIloNumToNumStepFunction())

  def sub(f: NumToNumStepFunction): Unit = this.f.sub(f.getIloNumToNumStepFunction())

  def prod(k: Double): Unit = this.f.prod(k)

  def shift(dx: Double): Unit = this.f.shift(dx)

  def shift(dx: Double, dval: Double): Unit = f.shift(dx, dval)

  /**
    *
    * @return
    */
  override def toString() = {
    val xmin = f.getDefinitionIntervalMin
    val xmax = f.getDefinitionIntervalMax
    var prevX = xmin
    var prevV = .0
    val builder = new StringBuilder
    builder.append("NumToNuMStepFunction: [").append(xmin).append(" .. ").append(xmax).append(") {")
    val cursor = model.cp.numToNumStepFunctionCursor(f, f.getDefinitionIntervalMin)
    while (cursor.ok()) {
      val x = cursor.getSegmentMin
      val v = cursor.getValue
      if (prevX < x) builder.append("[").append(prevX).append(" .. ").append(x).append(") -> ").append(prevV).append(" ; ")
      prevX = x
      prevV = v
      cursor.next()
    }
    builder.append("[").append(prevX).append(" .. ").append(xmax).append(") -> ").append(prevV).append(" ; ")
    builder.append("}")
    builder.toString
  }
}

object NumToNumStepFunction {
  /**
    * Converts a CPLEX step function to a step function.
    *
    * @param f is the CPLEX step function
    * @param model is the constraint programming model
    * @return step function
    */
  def apply(f: IloNumToNumStepFunction)(implicit model: CpModel): NumToNumStepFunction = new NumToNumStepFunction(f)
}