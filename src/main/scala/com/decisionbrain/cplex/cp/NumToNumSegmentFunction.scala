/*
 * Source file provided under Apache License, Version 2.0, January 2004,
 * http://www.apache.org/licenses/
 * (c) Copyright DecisionBrain SAS 2016,2018
 */

package com.decisionbrain.cplex.cp

import ilog.concert.IloNumToNumSegmentFunction
import ilog.cp.IloCP


/**
  * Iterator on piecewise linear function.
  *
  * @param f is the piecewise linear function
  * @param model is the constraint programming model
  */
class NumToNumSegmentFunctionIterator(f: NumToNumSegmentFunction)(implicit model: CpModel) extends Iterator[NumSegment] {

  val cursor = model.numToNumSegmentFunctionCursor(f, f.getDefinitionIntervalMin())

  override def hasNext: Boolean = cursor.ok()

  override def next(): NumSegment = {
    val segment = NumSegment(cursor.getSegmentMin,
      cursor.getSegmentMax,
      cursor.getValueLeft,
      cursor.getValueRight)
    cursor.next
    segment
  }
}

/**
  * Constructor of class NumToNumStepFunction.
  *
  * @param f is a CPLEX piecewise linear function
  * @param model is the constraint programming model
  */
class NumToNumSegmentFunction(f: IloNumToNumSegmentFunction)(implicit model: CpModel) extends Iterable[NumSegment] {

  /**
    * Returns the CPLEX piecewise linear function
    *
    * @return the CPLEX piecewise linear function
    */
  def getIloNumToNumSegmentFunction(): IloNumToNumSegmentFunction = f

  /**
    * This member function returns the left-most point of the definition interval of the invoking piecewise linear function.
    *
    * @return the first point of the definition interval
    */
  def getDefinitionIntervalMin(): Double = f.getDefinitionIntervalMin

  /**
    * This member function returns the right-most point of the definition interval of the invoking piecewise linear function.
    *
    * @return the last point of the definition interval
    */
  def getDefinitionIntervalMax(): Double = f.getDefinitionIntervalMax

  /**
    * This member function returns the value of the invoking piecewise linear function at x.
    *
    * @return the value of the piecewise linear function at the given point
    */
  def getValue(x: Double): Double = f.getValue(x)

  /**
    * This member function returns the minimal value of the invoking piecewise linear function on the interval [x1, x2).
    *
    * @param x1 is the start of the interval
    * @param x2 is the end of the interval
    * @return the minimal value of the piecewise linear function on the given interval
    */
  def getMin(x1: Double, x2: Double): Double = f.getMin(x1, x2)

  /**
    * This member function returns the maximal value of the invoking piecewise linear function on the interval [x1, x2).
    *
    * @param x1 is the start of the interval
    * @param x2 is the end of the interval
    * @return the maximal value of the piecewise linear function on the given interval
    */
  def getMax(x1: Double, x2: Double): Double = f.getMax(x1, x2)

  /**
    * This member function returns the sum of the invoking piecewise linear function on the interval [x1, x2).
    *
    * @param x1 is the start of the interval
    * @param x2 is the end of the interval
    * @return
    */
  def getArea(x1: Double, x2: Double): Double = f.getArea(x1, x2)

  /**
    * This member function sets the value of the invoking piecewise linear function to be v on the interval [x1, x2).
    *
    * @param x1 is the start of the interval
    * @param x2 the end of the interval
    * @param v is the value on the interval
    */
  def setValue(x1: Double, x2: Double, v: Double): Unit = f.setValue(x1, x2, v)

  /**
    * This member function adds v to the value of the invoking piecewise linear function everywhere on the interval [x1, x2).
    *
    * @param x1 is the start of the interval
    * @param x2 is the end of the interval
    * @param v is the value added to the piecewise linear function on the given interval
    */
  def addValue(x1: Double, x2: Double, v: Double): Unit = f.addValue(x1, x2, v)

  /**
    * This member function sets the value of the invoking piecewise linear function to be the minimum between the current value and
    * the value of <em>f</em> everywhere on the definition interval of the invoking function. The definition interval
    * of <em>f</em> must be the same as the one of the invoking piecewise linear function.
    *
    * @param f is the other piecewise linear function
    */
  def setMin(f: NumToNumSegmentFunction): Unit = this.f.setMin(f.getIloNumToNumSegmentFunction())

  /**
    * This member function sets the value of the invoking piecewise linear function to be the minimum between the current value and
    * the value of <em>f</em> everywhere on the definition interval of the invoking function. The definition interval
    * of <em>f</em> must be the same as the one of the invoking piecewise linear function.
    *
    * @param x1 is the start of the interval
    * @param x2 is the end of the interval
    * @param v is the minimal value
    */
  def setMin(x1: Double, x2: Double, v: Double): Unit = f.setMin(x1, x2, v)

  /**
    * This member function sets the value of the invoking piecewise linear function to be the maximum between the current value and
    * the value of <em>f</em> everywhere on the definition interval of the invoking function. The interval of definition
    * of <em>f</em> must be the same as that of the invoking piecewise linear function.
    *
    * @param f is the other piecewise linear function
    */
  def setMax(f: NumToNumSegmentFunction): Unit = this.f.setMax(f.getIloNumToNumSegmentFunction())

  /**
    * This member function sets the value of the invoking piecewise linear function to be the maximum between the current value and
    * <em>v</em> everywhere on the interval <em>[x1, x2)</em>.
    *
    * @param x1 is the start of the interval
    * @param x2 is the end of the interval
    * @param v is the maximal value
    */
  def setMax(x1: Double, x2: Double, v: Double): Unit  = f.setMin(x1, x2, v)

  /**
    * This member function multiplies by <em>k</em> the scale of x for the invoking piecewise linear function, <em>k</em> must be a
    * nonnegative numeric value. More precisely, if the invoking function was defined over an interval
    * <em>[xMin, xMax)</em>, it will be redefined over the interval <em>[k*xMin, k*xMax)</em> and the value at
    * <em>x</em> will be the former value at <em>x/k</em>.
    *
    * @param k is the scaling factor
    */
  def dilate(k: Double): Unit = f.dilate(k)

  /**
    * This member function initializes the invoking function as a piecewise linear function that repeats the piecewise linear function f, n
    * times after x0. More precisely, if f is defined on [xfpMin,xfpMax) and if the invoking function is defined on
    * [xMin,xMax), the value of the invoking function will be:
    * <ul>
    *   <li>dval on [xMin, x0),</li
    *   <li>f((x-x0) % (xfpMax-xfpMin)) for x in [x0, Min(x0+n*(xfpMax-xfpMin), xMax)),</li>
    *   <li>dval on [Min(x0+n*(xfpMax-xfpMin), xMax), xMax).</li>
    * </ul>
    *
    * @param f is the other piecewise linear function
    * @param x0 is the starting point
    * @param n is the number of repetitions
    * @param dval is the default value
    */
  def setPeriodic(f: NumToNumSegmentFunction, x0: Double, n: Double = IloCP.Infinity, dval: Double = 0): Unit =
    this.f.setPeriodic(f.getIloNumToNumSegmentFunction(), x0, n, dval)

  /**
    * This member function changes the value of the invoking function on the interval [x1,x2). On this interval, the
    * invoking function is set to equal a repetition of the pattern function f with an initial offset of offset. The
    * invoking function is not modified outside the interval [x1,x2). More precisely, if [min,max) denotes the
    * definition interval of f, for all t in [x1,x2), the invoking function at t is set to equal
    * f(min + (offset+t-x1)%(max-min))) where % denotes the modulo operator. By default, the offset is equal to 0.
    *
    * @param x1 is the start of the interval
    * @param x2 is the end of the interval
    * @param f is the other piecewise linear function
    * @param offset is the offset
    */
  def setPeriodicValue(x1: Double, x2: Double, f: NumToNumSegmentFunction, offset: Double = 0): Unit =
    this.f.setPeriodicValue(x1, x2, f.getIloNumToNumSegmentFunction(), offset)

  /**
    * This function adds the argument function fct to the invoking piecewise linear function.
    *
    * @param f
    */
  def +=(f: NumToNumSegmentFunction): Unit = this.f.add(f.getIloNumToNumSegmentFunction())

  /**
    * Returns a new piecewise linear function that is the sum of this piecewise linear function and and the piecewise linear function given as argument.
    *
    * @param f is the other piecewise linear function
    * @return a new piecewise linear function that is the sum of the two piecewise linear functions
    */
  def +(f: NumToNumSegmentFunction): NumToNumSegmentFunction = {
    val nf = getIloNumToNumSegmentFunction().copy()
    nf.add(f.getIloNumToNumSegmentFunction())
    NumToNumSegmentFunction(nf)
  }

  /**
    * Returns a new piecewise linear function that is the sum of this piecewise linear function and and the piecewise linear function given as argument.
    *
    * @param f is the other piecewise linear function
    * @return a new piecewise linear function that is the sum of the two piecewise linear functions
    */
  def -(f: NumToNumSegmentFunction): NumToNumSegmentFunction = {
    val nf = getIloNumToNumSegmentFunction().copy()
    nf.sub(f.getIloNumToNumSegmentFunction())
    NumToNumSegmentFunction(nf)
  }

  /**
    * This operator subtracts the argument function fct from the invoking piecewise linear function.
    *
    * @param f is the other piecewise linear function
    */
  def -=(f: NumToNumSegmentFunction): Unit = this.f.sub(f.getIloNumToNumSegmentFunction())

  /**
    * This operator multiplies by a factor k the value of the invoking piecewise linear function everywhere on the definition
    * interval.
    *
    * @param k is the factor
    */
  def *=(k: Double): Unit = this.f.prod(k)

  /**
    * This member function shifts the invoking function from dx to the right if dx > 0 or from -dx to the left if dx <
    * 0. It has no effect if dx = 0. More precisely, if the invoking function is defined on [xMin,xMax) and dx > 0, the
    * new value of the invoking function is:
    * <ul>
    *   <li>dval on the interval [xMin, xMin+dx),</li>
    *   <li>for all x in [xMin+dx, xMax), the former value at x-dx.</li>
    * </ul>
    * If dx < 0, the new value of the invoking function is:
    * <ul>
    *   <li>for all x in [xMin, xMax+dx), the former value at x-dx,</li>
    *   <li>dval on the interval [xMax+dx,xMax).</li>
    * </ul>
    *
    * @param dx is the shift on x
    * @param dval is the default value
    */
  def shift(dx: Double, dval: Double = 0): Unit = f.shift(dx, dval)

  /**
    * Returns true if the piecewise lienear function is semi-convex, false otherwise.
    *
    * @return true if function is semi-convex
    */
  def isSemiConvex(): Boolean = f.isSemiConvex

  /**
    * Converts the piecewise linear function to a character string
    *
    * @return a character string
    */
  override def toString() = {
    val xmin = f.getDefinitionIntervalMin
    val xmax = f.getDefinitionIntervalMax
    var prevEnd = xmin
    val builder = new StringBuilder
    builder.append("NumToNumSegmentFunction: [").append(xmin).append(" .. ").append(xmax).append(") {")
    for (s <- this) {
      builder.append(s.toString())
    }
    builder.append("}")
    builder.toString
  }

  override def iterator: Iterator[NumSegment] = new NumToNumSegmentFunctionIterator(this)
}

object NumToNumSegmentFunction {
  /**
    * Converts a CPLEX piecewise linear function to a piecewise linear function.
    *
    * @param f is the CPLEX piecewise linear function
    * @param model is the constraint programming model
    * @return a piecewise linear function
    */
  def apply(f: IloNumToNumSegmentFunction)(implicit model: CpModel): NumToNumSegmentFunction = new NumToNumSegmentFunction(f)
}
