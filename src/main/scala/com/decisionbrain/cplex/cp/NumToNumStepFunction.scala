/*
 *  Source file provided under Apache License, Version 2.0, January 2004,
 *  http://www.apache.org/licenses/
 *  (c) Copyright DecisionBrain SAS 2016,2019
 */

package com.decisionbrain.cplex.cp

import com.decisionbrain.cplex.Modeler.NumArray
import com.decisionbrain.cplex.NumStep
import ilog.concert.IloNumToNumStepFunction
import ilog.concert.cppimpl.IloConcertUtils
import ilog.cp.IloCP

/**
  * Iterator on step function.
  *
  * @param f is the step function
  * @param model is the constraint programming model
  */
class NumToNumStepFunctionIterator(f: NumToNumStepFunction)(implicit model: CpModel) extends Iterator[NumStep] {

  val cursor = model.numToNumStepFunctionCursor(f, f.getDefinitionIntervalMin())

  override def hasNext: Boolean = cursor.ok()

  override def next(): NumStep = {
    val segment = NumStep(cursor.getSegmentMin,
      cursor.getSegmentMax,
      cursor.getValue)
    cursor.next
    segment
  }
}

/**
  * Constructor of class NumToNumStepFunction.
  *
  * @param f is a CPLEX step function
  * @param model is the constraint programming model
  */
class NumToNumStepFunction(f: IloNumToNumStepFunction)(implicit model: CpModel) extends Iterable[NumStep] {

  /**
    * Returns the CPLEX step function
    *
    * @return the CPLEX step function
    */
  def getIloNumToNumStepFunction(): IloNumToNumStepFunction = f

  /**
    * This member function returns the left-most point of the definition interval of the invoking step function.
    *
    * @return the first point of the definition interval
    */
  def getDefinitionIntervalMin(): Double = f.getDefinitionIntervalMin

  /**
    * This member function returns the right-most point of the definition interval of the invoking step function.
    *
    * @return the last point of the definition interval
    */
  def getDefinitionIntervalMax(): Double = f.getDefinitionIntervalMax

  /**
    * This member function returns the value of the invoking step function at x.
    *
    * @return the value of the step function at the given point
    */
  def getValue(x: Double): Double = f.getValue(x)

  /**
    * This member function returns the minimal value of the invoking step function on the interval [x1, x2).
    *
    * @param x1 is the start of the interval
    * @param x2 is the end of the interval
    * @return the minimal value of the step function on the given interval
    */
  def getMin(x1: Double, x2: Double): Double = f.getMin(x1, x2)

  /**
    * This member function returns the maximal value of the invoking step function on the interval [x1, x2).
    *
    * @param x1 is the start of the interval
    * @param x2 is the end of the interval
    * @return the maximal value of the step function on the given interval
    */
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

  /**
    * This member function adds v to the value of the invoking step function everywhere on the interval [x1, x2).
    *
    * @param x1 is the start of the interval
    * @param x2 is the end of the interval
    * @param v is the value added to the step function on the given interval
    */
  def addValue(x1: Double, x2: Double, v: Double): Unit = f.addValue(x1, x2, v)

  /**
    * This member function sets the value of the invoking step function to be the minimum between the current value and
    * the value of <em>f</em> everywhere on the definition interval of the invoking function. The definition interval
    * of <em>f</em> must be the same as the one of the invoking step function.
    *
    * @param f is the other step function
    */
  def setMin(f: NumToNumStepFunction): Unit = this.f.setMin(f.getIloNumToNumStepFunction())

  /**
    * This member function sets the value of the invoking step function to be the minimum between the current value and
    * the value of <em>f</em> everywhere on the definition interval of the invoking function. The definition interval
    * of <em>f</em> must be the same as the one of the invoking step function.
    *
    * @param x1 is the start of the interval
    * @param x2 is the end of the interval
    * @param v is the minimal value
    */
  def setMin(x1: Double, x2: Double, v: Double): Unit = f.setMin(x1, x2, v)

  /**
    * This member function sets the value of the invoking step function to be the maximum between the current value and
    * the value of <em>f</em> everywhere on the definition interval of the invoking function. The interval of definition
    * of <em>f</em> must be the same as that of the invoking step function.
    *
    * @param f is the other step function
    */
  def setMax(f: NumToNumStepFunction): Unit = this.f.setMax(f.getIloNumToNumStepFunction())

  /**
    * This member function sets the value of the invoking step function to be the maximum between the current value and
    * <em>v</em> everywhere on the interval <em>[x1, x2)</em>.
    *
    * @param x1 is the start of the interval
    * @param x2 is the end of the interval
    * @param v is the maximal value
    */
  def setMax(x1: Double, x2: Double, v: Double): Unit  = f.setMin(x1, x2, v)

  /**
    * This member function multiplies by <em>k</em> the scale of x for the invoking step function, <em>k</em> must be a
    * nonnegative numeric value. More precisely, if the invoking function was defined over an interval
    * <em>[xMin, xMax)</em>, it will be redefined over the interval <em>[k*xMin, k*xMax)</em> and the value at
    * <em>x</em> will be the former value at <em>x/k</em>.
    *
    * @param k is the scaling factor
    */
  def dilate(k: Double): Unit = f.dilate(k)

  /**
    * This member function initializes the invoking function as a step function whose steps are defined by the two
    * arguments arrays <em>x</em> and <em>v</em>. More precisely, if <em>n</em> is the size of array <em>x</em>, size
    * of array <em>v<em> must be <em>n+1</em> and, if the invoking function is defined on the interval
    * <em>[xMin,xMax)</em>, its values will be:
    * <ul>
    *   <li>v[0] on interval [xMin,x[0]),</li>
    *   <li>v[i] on interval [x[i-1],x[i]) for all i in [0,n-1],</li>
    *   <li>v[n] on interval [x[n-1],xMax).</li>
    *
    * @param x is the array of points
    * @param v is the array of values
    */
  def setSteps(x: NumArray, v: NumArray): Unit = {
    setSteps(x.toArray, v.toArray)
  }

  /**
    * This member function initializes the invoking function as a step function whose steps are defined by the two
    * arguments arrays <em>x</em> and <em>v</em>. More precisely, if <em>n</em> is the size of array <em>x</em>, size
    * of array <em>v<em> must be <em>n+1</em> and, if the invoking function is defined on the interval
    * <em>[xMin,xMax)</em>, its values will be:
    * <ul>
    *   <li>v[0] on interval [xMin,x[0]),</li>
    *   <li>v[i] on interval [x[i-1],x[i]) for all i in [0,n-1],</li>
    *   <li>v[n] on interval [x[n-1],xMax).</li>
    * </ul>
    *
    * @param x is the array of points
    * @param v is the array of values
    */
  def setSteps(x: Array[Double], v: Array[Double]): Unit = {
    val xa = IloConcertUtils.ToCppIloNumArray(model.cp.getEnvImpl, x)
    val va = IloConcertUtils.ToCppIloNumArray(model.cp.getEnvImpl, v)
    f.setSteps(xa, va)
  }

  /**
    * This member function initializes the invoking function as a step function that repeats the step function f, n
    * times after x0. More precisely, if f is defined on [xfpMin,xfpMax) and if the invoking function is defined on
    * [xMin,xMax), the value of the invoking function will be:
    * <ul>
    *   <li>dval on [xMin, x0),</li
    *   <li>f((x-x0) % (xfpMax-xfpMin)) for x in [x0, Min(x0+n*(xfpMax-xfpMin), xMax)),</li>
    *   <li>dval on [Min(x0+n*(xfpMax-xfpMin), xMax), xMax).</li>
    * </ul>
    *
    * @param f is the other step function
    * @param x0 is the starting point
    * @param n is the number of repetitions
    * @param dval is the default value
    */
  def setPeriodic(f: IloNumToNumStepFunction, x0: Double, n: Double = IloCP.Infinity, dval: Double = 0): Unit =
    this.f.setPeriodic(f, x0, n, dval)

  /**
    * This member function changes the value of the invoking function on the interval [x1,x2). On this interval, the
    * invoking function is set to equal a repetition of the pattern function f with an initial offset of offset. The
    * invoking function is not modified outside the interval [x1,x2). More precisely, if [min,max) denotes the
    * definition interval of f, for all t in [x1,x2), the invoking function at t is set to equal
    * f(min + (offset+t-x1)%(max-min))) where % denotes the modulo operator. By default, the offset is equal to 0.
    *
    * @param x1 is the start of the interval
    * @param x2 is the end of the interval
    * @param f is the other step function
    * @param offset is the offset
    */
  def setPeriodicValue(x1: Double, x2: Double, f: NumToNumStepFunction, offset: Double = 0): Unit =
    this.f.setPeriodicValue(x1, x2, f.getIloNumToNumStepFunction(), offset)

  /**
    * This function adds the argument function fct to the invoking step function.
    *
    * @param f
    */
  def +=(f: NumToNumStepFunction): Unit = this.f.add(f.getIloNumToNumStepFunction())

  /**
    * Returns a new step function that is the sum of this step function and and the step function given as argument.
    *
    * @param f is the other step function
    * @return a new step function that is the sum of the two step functions
    */
  def +(f: NumToNumStepFunction): NumToNumStepFunction = {
    val nf = getIloNumToNumStepFunction().copy()
    nf.add(f.getIloNumToNumStepFunction())
    NumToNumStepFunction(nf)
  }

  /**
    * Returns a new step function that is the sum of this step function and and the step function given as argument.
    *
    * @param f is the other step function
    * @return a new step function that is the sum of the two step functions
    */
  def -(f: NumToNumStepFunction): NumToNumStepFunction = {
    val nf = getIloNumToNumStepFunction().copy()
    nf.sub(f.getIloNumToNumStepFunction())
    NumToNumStepFunction(nf)
  }

  /**
    * This operator subtracts the argument function fct from the invoking step function.
    *
    * @param f is the other step function
    */
  def -=(f: NumToNumStepFunction): Unit = this.f.sub(f.getIloNumToNumStepFunction())

  /**
    * This operator multiplies by a factor k the value of the invoking step function everywhere on the definition
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
    * Converts the step function to a character string?
    *
    * @return a character string
    */
  override def toString() = {
    val xmin = f.getDefinitionIntervalMin
    val xmax = f.getDefinitionIntervalMax
    var prevX = xmin
    var prevV = .0
    val builder = new StringBuilder
    builder.append("NumToNumStepFunction: [").append(xmin).append(" .. ").append(xmax).append(") {")
    for (s <- this) {
      val x = s.start
      val v = s.value
      if (prevX < x) builder.append("[").append(prevX).append(" .. ").append(x).append(") -> ").append(prevV).append(" ; ")
      prevX = x
      prevV = v
    }
    builder.append("[").append(prevX).append(" .. ").append(xmax).append(") -> ").append(prevV).append(" ; ")
    builder.append("}")
    builder.toString
  }

  override def iterator: Iterator[NumStep] = new NumToNumStepFunctionIterator(this)
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