/*
 * Source file provided under Apache License, Version 2.0, January 2004,
 * http://www.apache.org/licenses/
 * (c) Copyright DecisionBrain SAS 2016,2017
 */

package com.decisionbrain.cplex.cp

import ilog.cp.IloSearchPhase

/**
  * Constructor of class Objective.
  *
  * @param s is the CPO search phase
  * @param model is the constraint programming model
  */
class SearchPhase(s: IloSearchPhase)(implicit model: CpModel) {

  /**
    * Returns the CPLEX objective.
    *
    * @return the CPLEX objective
    */
  def getIloSearchPhase(): IloSearchPhase = s

  /**
    * Convert the objective to a character string.
    * @return
    */
  override def toString: String = s.toString
}

object SearchPhase {
  def apply(s: IloSearchPhase)(implicit model: CpModel) = new SearchPhase(s)
}