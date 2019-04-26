/*
 *  Source file provided under Apache License, Version 2.0, January 2004,
 *  http://www.apache.org/licenses/
 *  (c) Copyright DecisionBrain SAS 2016,2019
 */

package com.decisionbrain.cplex

import ilog.concert.IloAddable


trait Addable {

  /**
    * Returns the name of model addable object.
    *
    * @return the name of the object
    */
  def getName: Option[String] = Option(getIloAddable().getName)

  /**
    * Set the name of the model addable object.
    *
    * @param name is the name of the object
    */
  def setName(name: String): Addable = {
    getIloAddable().setName(name)
    this
  }

  /**
    * Returns the CPLEX addable object.
    *
    * @return the CPLEX addable object
    */
  def getIloAddable(): IloAddable

}
