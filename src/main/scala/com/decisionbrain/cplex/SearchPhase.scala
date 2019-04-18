/*
 *  Source file provided under Apache License, Version 2.0, January 2004,
 *  http://www.apache.org/licenses/
 *  (c) Copyright DecisionBrain SAS 2016,2019
 */

package com.decisionbrain.cplex

import ilog.cp.IloSearchPhase

/**
  * Constructor of class SearchPhase.
  *
  * @param s is the CPO search phase
  * @param modeler is the constraint programming model
  */
class SearchPhase(s: IloSearchPhase)(implicit modeler: Modeler) {

  /**
    * Returns the CPLEX search phase.
    *
    * @return the CPLEX search phase
    */
  def getIloSearchPhase(): IloSearchPhase = s

  /**
    * Convert the search phase to a character string.
    * @return
    */
  override def toString: String = s.toString
}

/**
  * Companion object for class SearchPhase
  */
object SearchPhase {

  /**
    * Creates and returns a new search phase.
    *
    * @param s is the CPLEX search phase
    * @param modeler is the constraint programming model
    * @return a new search phase
    */
  def apply(s: IloSearchPhase)(implicit modeler: Modeler) = new SearchPhase(s)
}