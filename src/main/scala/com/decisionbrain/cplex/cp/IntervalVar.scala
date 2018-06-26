/*
 * Source file provided under Apache License, Version 2.0, January 2004,
 * http://www.apache.org/licenses/
 * (c) Copyright DecisionBrain SAS 2016,2018
 */

package com.decisionbrain.cplex.cp

import com.decisionbrain.cplex.Addable
import ilog.concert._

/**
  * Class for numeric variables.
  *
  * @param v     is the CPLEX interval variable
  * @param model is the constraint programming model
  */
class IntervalVar(v: IloIntervalVar)(implicit model: CpModel) extends Addable {

  /**
    * Returns the CPLEX integer variable.
    *
    * @return the CPLEX integer variable
    */
  def getIloIntervalVar(): IloIntervalVar = v


  /**
    * Return the name of the numeric variable
    * @return the name
    */
  def getName(): Option[String] = Option(v.getName())

  /**
    * Set the name of the numeric variable.
    *
    * @param name is the name of the numeric variable
    */
  def setName(name: String) = v.setName(name)

  /**
    * Returns the CPLEX numeric variable
    *
    * @return the CPLEX numeric variable
    */
  override def getIloAddable(): IloAddable = v

  /**
    * Returns the earliest start time of this interval variable
    *
    * @return the earliest start time
    */
  def getStartMin: Int = v.getStartMin

  /**
    * Returns the latest start time of this interval variable.
    *
    * @return the latest start time
    */
  def getStartMax: Int = v.getStartMax

  /**
    * Returns the earliest end time of this interval variable.
    *
    * @return the earliest end time
    */
  def getEndMin: Int = v.getEndMin

  /**
    * Returns the latest end time of this interval variable.
    *
    * @return the latest end time
    */
  def getEndMax: Int = v.getEndMax

  /**
    * Returns the minimum size of this interval variable.
    *
    * @return the minimum size
    */
  def getSizeMin: Int = v.getSizeMin

  /**
    * Returns the maximum size of this interval variable.
    *
    * @return the maximum size
    */
  def getSizeMax: Int = v.getSizeMax

  /**
    * Returns the minimum length of this interval variable.
    *
    * @return the minimum length
    */
  def getLengthMin: Int = v.getLengthMin

  /**
    * Returns the maximum length of this interval variable.
    *
    * @return the maximum length
    */
  def getLengthMax: Int = v.getLengthMax

  /**
    * Returns true if this interval variable is present.
    *
    * @return true if present
    */
  def isPresent: Boolean = v.isPresent

  /**
    * Returns true of this interval variable is absent.
    *
    * @return true if absent
    */
  def isAbsent: Boolean = v.isAbsent

  /**
    * Sets the earliest start time of this interval variable.
    *
    * @param value is the earliest start time
    */
  def setStartMin(value: Int) = v.setStartMin(value)

  /**
    * Sets the latest start time of this interval variable.
    *
    * @param value is the latest start time
    */
  def setStartMax(value: Int) = v.setStartMax(value)

  /**
    * Sets the earliest end time of this interval variable.
    *
    * @param value is the earliest end time
    */
  def setEndMin(value: Int) = v.setEndMin(value)

  /**
    * Sets the latest end time of this interval variable.
    *
    * @param value is the latest end time
    */
  def setEndMax(value: Int) = v.setEndMax(value)

  /**
    * Sets the minimum size of this interval variable.
    *
    * @param value is the minimum size
    */
  def setSizeMin(value: Int) = v.setSizeMin(value)

  /**
    * Sets the maximum size of this interval variable.
    *
    * @param value is the maximum size
    */
  def setSizeMax(value: Int) = v.setSizeMax(value)

  /**
    * Sets the maximum length of this interval variable.
    *
    * @param value is the maximum size
    */
  def setLengthMin(value: Int) = v.setLengthMin(value)

  /**
    * Sets the maximum length of this interval variable.
    *
    * @param value is the maximum length
    */
  def setLengthMax(value: Int) = v.setLengthMax(value)

  /**
    * Sets the interval variable present.
    */
  def setPresent() = v.setPresent()

  /**
    * Sets the interval variable absent.
    */
  def setAbsent() = v.setAbsent()

  /**
    * Sets the interval variable optional.
    */
  def setOptional() = v.setOptional()

  /**
    * Returns the intensity function of this interval variable.
    *
    * @return the intensity function
    */
  def getIntensity: IloNumToNumStepFunction = v.getIntensity

  /**
    * Returns the granularity of the intensity function of this interval variable.
    *
    * @return the granularity of the intensity function
    */
  def getGranularity: Int = v.getGranularity

  /**
    * Sets the intensity function of ths interval variable.
    *
    * @param intensity is the intensity function
    * @param granularity is the granularity of the intensity function
    */
  def setIntensity(intensity: NumToNumStepFunction, granularity: Int = 100) =
    v.setIntensity(intensity.getIloNumToNumStepFunction(), granularity)

  /**
    *
    * @param v
    * @return
    */
  def <= (v: IntervalVar): Constraint =
    model.startBeforeStart(this, v)

  /**
    *
    * @param v
    * @return
    */
  def < (v: IntervalVar): Constraint =
    model.endBeforeStart(this, v)
}

object IntervalVar {

  /**
    * Converts a CPLEX numeric variable to a numeric variable.
    *
    * @param v is the CPLEX numeric variable
    * @param model is the constraint programming model
    * @return a numeric variable
    */
  def apply(v: IloIntervalVar)(implicit model: CpModel): IntervalVar = new IntervalVar(v)
}
