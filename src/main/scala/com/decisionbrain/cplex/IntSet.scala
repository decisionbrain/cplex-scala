/*
 *  Source file provided under Apache License, Version 2.0, January 2004,
 *  http://www.apache.org/licenses/
 *  (c) Copyright DecisionBrain SAS 2016,2019
 */

package com.decisionbrain.cplex

import java.util

import com.decisionbrain.cplex.cp.CpModel
import ilog.concert.IloIntSet

/**
  * Iterator on cumul function expression.
  *
  * @param f is the cumul function expression
  * @param modeler is the constraint programming model
  */
class IntSetIterator(f: IntSet)(implicit modeler: Modeler) extends Iterator[Int] {

  val iter: util.Iterator[_] = f.getIloIntSet().iterator()

  override def hasNext: Boolean = iter.hasNext

  override def next(): Int = iter.next().toString.toInt
}


class IntSet(val set: IloIntSet)(implicit model: Modeler) extends Iterable[Int] {

//  /**
//    * Returns the constraint programming model
//    *
//    * @return the constraint programming model
//    */
//  def getModeler(): Modeler = model

  /**
    * Return the CPLEX set of integer values.
    *
    * @return the set of integer values
    */
  def getIloIntSet(): IloIntSet = set

  /**
    * This method adds elt to the invoking set. Here, "adds" means that the invoking set becomes the union of its
    * former elements and the new elt.
    *
    * @param elt is the integer value added to the set
    */
  def add(elt: Int) = set.add(elt)

  /**
    * This method returns a true if the integer value `elt` is an element the invoking set and false otherwise.
    *
    * @param elt
    * @return
    */
  def contains(elt: Int): Boolean = set.contains(elt)

  /**
    * This method removes elt to the invoking set.
    *
    * @param elt is the integer value to remove from the set
    */
  def remove(elt: Int) = set.remove(elt)

  /**
    * Returns an iterator on the set of integer values.
    *
    * @return an iterator on the set of integer values
    */
  override def iterator: Iterator[Int] = new IntSetIterator(this)
}

object IntSet {
  /**
    * Converts a CPLEX set of integer values to an set of integer values.
    *
    * @param e     is the CPLEX set of integer values
    * @param model is the constraint programming model
    * @return a set of integer values
    */
  def apply(e: IloIntSet)(implicit model: CpModel) = new IntSet(e)
}

