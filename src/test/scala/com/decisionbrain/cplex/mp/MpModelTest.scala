/*
 * Source file provided under Apache License, Version 2.0, January 2004,
 * http://www.apache.org/licenses/
 * (c) Copyright DecisionBrain SAS 2016,2018
 */

package com.decisionbrain.cplex.mp

import java.io.{File, FileReader}

import ilog.concert.IloNumVarType
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FunSuite, Matchers}

/**
  * Created by dgodard on 03/02/2017.
  */
@RunWith(classOf[JUnitRunner])
class MpModelTest extends FunSuite with Matchers {


  //    @Before
  //    public void setUp() throws Exception {
  //
  //    }

  /**
    * Check the public API
    *
    * DO NOT MODIFY NOR REMOVE AN EXISTING API, MARK IT DEPRECATED WITH THE DATE / RELEASE NUMBER.
    */
  test("checkAPI") {

    val epsilon = 1e-6

    // constructor
    val anonymousModel = new MpModel()

    // apply method on companion object
    val model = MpModel("checkAPI")

    // getIloCplex
    anonymousModel.cplex should not equal(null)
    model.cplex should not equal(null)
    anonymousModel.cplex should not equal(model.cplex)
    model.cplex should equal(model.cplex)

    // getName() : returns ann option
    anonymousModel.getName() should equal (None)
    anonymousModel.getName().getOrElse("") should equal ("")
    model.getName should equal (Some("checkAPI"))
    model.getName().getOrElse("") should equal ("checkAPI")

    // numVar
    val anonymousNumVar = model.numVar(-10, 15)
    anonymousNumVar.getName() should equal(None)
    val numVar = model.numVar(-10, 15, "aNumVar")
    numVar.getName() should equal(Some("aNumVar"))
    numVar.getIloCplex() should not equal(null)
    numVar.getIloNumVar() should not equal(null)
    numVar.getLB() should equal(-10.0 +- epsilon)
    numVar.getUB() should equal(15.0 +- epsilon)
    numVar.getType() should equal(IloNumVarType.Float)
    numVar.getIloNumVar() should not equal(null)
    numVar.setLB(-20.0)
    numVar.getLB() should equal(-20.0 +- epsilon)
    numVar.setUB(10)
    numVar.getUB() should equal(10.0 +- epsilon)

    // intVar
    val anonymousIntVar = model.intVar(-10, 15)
    anonymousIntVar.getName() should equal(None)
    val intVar = model.intVar(-10, 15, "aIntVar")
    intVar.getName() should equal(Some("aIntVar"))
    numVar.getIloCplex() should not equal(null)
    numVar.getIloNumVar() should not equal(null)
    intVar.getLB() should equal(-10.0 +- epsilon)
    intVar.getUB() should equal(15.0 +- epsilon)
    intVar.getType() should equal(IloNumVarType.Int)
    intVar.getIloNumVar() should not equal(null)
    intVar.setLB(-20)
    intVar.getLB() should equal(-20.0 +- epsilon)
    intVar.setUB(10)
    intVar.getUB() should equal(10.0 +- epsilon)

    // boolVar
    val anonymousBoolVar = model.boolVar()
    anonymousBoolVar.getName() should equal(None)
    val boolVar = model.boolVar("aBoolVar")
    boolVar.getName() should equal(Some("aBoolVar"))
    numVar.getIloCplex() should not equal(null)
    numVar.getIloNumVar() should not equal(null)
    boolVar.getLB() should equal(.0 +- epsilon)
    boolVar.getUB() should equal(1.0 +- epsilon)
    boolVar.getType() should equal(IloNumVarType.Bool)
    boolVar.getIloNumVar() should not equal(null)
    boolVar.setLB(1.0)
    boolVar.getLB() should equal(1.0 +- epsilon)
    boolVar.setUB(0)
    boolVar.getUB() should equal(0.0 +- epsilon)
    boolVar.setLB(0.0)
    boolVar.getLB() should equal(0.0 +- epsilon)
    boolVar.setUB(1)
    boolVar.getUB() should equal(1.0 +- epsilon)

    // linearNumExpr
    val linearNumExpr = model.linearNumExpr(3.5)
    linearNumExpr.getIloCplex() should not equal(null)
    linearNumExpr.getIloNumExpr should not equal(null)

    // linearIntExpr
    val linearIntExpr = model.linearIntExpr(3)
    linearIntExpr.getIloCplex() should not equal(null)
    linearIntExpr.getIloNumExpr should not equal(null)

    // range
    val r1 = model.range(0, model.numVar(0, 10, "x"), 10)
    r1.getName should equal (None)
    r1.getName.getOrElse("") should equal ("")
    r1.getLB should equal (.0)
    r1.getUB should equal (10.0)
    r1.getNumExpr should not equal (null)
    r1.getNumExpr().toString should equal ("x ")

    val r2 = model.range(0, model.numVar(0, 10, "x1") + model.numVar(0, 10, "x2"), 18, "r2")
    r2.getName should equal (Some("r2"))
    r2.getName.getOrElse("") should equal ("r2")
    r2.getLB should equal (.0)
    r2.getUB should equal (18.0)
    r2.getNumExpr should not equal (null)

    // eq
    val eq1 = model.eq(model.numVar(0, 10, "x1"), model.numVar(5, 15))
    eq1.getName should equal(None)

    val eq2 = model.eq(model.numVar(0, 10, "x1"), 5)
    eq2.getName should equal(None)
    eq2.getLB should equal(5)
    eq2.getUB should equal(5)
    eq2.getNumExpr should not equal(null)

    val eq3 = model.eq(model.numVar(0, 10, "x1"), model.numVar(5, 15), "X1=X2")
    eq3.getName should equal(Some("X1=X2"))

    val eq4 = model.eq(model.numVar(0, 10, "x1"), 5)
    eq4.getName should equal(None)
    eq4.getLB should equal(5)
    eq4.getUB should equal(5)
    eq4.getNumExpr should not equal(null)

    // le
    val le1 = model.le(model.numVar(0, 10, "x1"), 5)
    le1.getName should equal(None)

    val le2 = model.le(model.numVar(0, 10, "x1"), 5, "x1 <= 5")
    le2.getName should equal(Some("x1 <= 5"))

    // ge
    val ge1 = model.le(model.numVar(0, 10, "x1"), 5)
    ge1.getName should equal(None)

    val ge2 = model.ge(model.numVar(0, 10, "x1"), 5, "x1 >= 5")
    ge2.getName should equal(Some("x1 >= 5"))

    // sum
    val sum1 = model.sum(List(model.numVar(0, 10), model.numVar(0, 20)))
    sum1.getIloNumExpr should not equal(null)

    val sum2 = model.sum(Set(model.numVar(0, 10), model.numVar(0, 20)))
    sum2.getIloNumExpr should not equal(null)

    val sum3 = model.sum(Map(1 -> model.numVar(0, 10), 2 -> model.numVar(0, 20)).map{case (k,v) => v})
    sum3.getIloNumExpr should not equal(null)

    val sum4 = model.sum(model.numVar(0, 10), model.numVar(0, 20))
    sum4.getIloNumExpr should not equal(null)

    // sum on companion object
    val cosum1 = MpModel.sum(List(model.numVar(0, 10), model.numVar(0, 20)))(implicitly(model))
    cosum1.getIloNumExpr should not equal null

    val cosum2 = MpModel.sum(model.numVar(0, 10), model.numVar(0, 20))(implicitly(model))
    cosum2.getIloNumExpr should not equal null

    // add
    model.add(model.eq(model.numVar(0, 10), 5)) should equal (model)

    // addRange
    model.addRange(0, model.numVar(0, 10, "x"), 10) should equal(model)

    // addEq
    model.addEq(model.numVar(0, 10), model.numVar(0, 20)) should equal(model)

    // minimize
    val model1 = MpModel("model1")
    val varModel1 = model1.numVar(0, 10)
    model1.add(model1.ge(varModel1, 5))
    model1.add(model1.minimize(varModel1))
//    model1.exportModel("model1.lp")

    // maximize
    val model2 = MpModel("model2")
    val varModel2 = model2.numVar(1, 10)
    model2.add(varModel2)
    model2.add(model2.le(varModel2, 5))
    model2.add(model2.maximize(varModel2))
//    model2.exportModel("model2.lp")

    // solve
    model1.solve() should equal(true)
    model2.solve() should equal(true)

    // getObjectiveValue
    model1.getObjectiveValue() should equal(5.0 +- epsilon)
    model2.getObjectiveValue() should equal(5.0 +- epsilon)

    // getBestObjValue
    model1.getBestObjValue() should equal (java.lang.Double.POSITIVE_INFINITY)
    model2.getBestObjValue() should equal (java.lang.Double.NEGATIVE_INFINITY)

    // getValue
    model1.getValue(varModel1) should equal (5.0 +- epsilon)
    model2.getValue(varModel2) should equal (5.0 +- epsilon)

    // exportModel
    val file1 = File.createTempFile("checkAPI_", ".lp")
    file1.exists() should equal(true)
    println("Exporting model " + model1.getName().getOrElse(this) + " to file " + file1.getAbsolutePath)
    model1.exportModel(file1.getAbsolutePath)
    val reader1 = new FileReader(file1.getAbsolutePath)
    reader1.read() should not equal(-1)
    file1.delete()

    val file2 = File.createTempFile("checkAPI_", ".lp")
    file2.exists() should equal(true)
    println("Exporting model " + model2.getName().getOrElse(this) + " to file " + file2.getAbsolutePath)
    model2.exportModel(file2.getAbsolutePath)
    val reader2 = new FileReader(file2.getAbsolutePath)
    reader2.read() should not equal(-1)
    file2.delete()

    // printInformation
    model1.printInformation()
    model2.printInformation()

    // end
    anonymousModel.end()
    model.end()
    model1.end()
    model2.end()

  }

}