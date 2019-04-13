/*
 * Source file provided under Apache License, Version 2.0, January 2004,
 * http://www.apache.org/licenses/
 * (c) Copyright DecisionBrain SAS 2016,2018
 */

package com.decisionbrain.cplex.cp

import com.decisionbrain.cplex.{IntExpr, IntVar}
import com.decisionbrain.cplex.Modeler._
import com.decisionbrain.cplex.cp.CpModel._
import ilog.cp.IloCP

/**
  * This example solves a scheduling problem on two alternative heterogeneous machines. A set of tasks {a_1,...,a_n}
  * has to be executed on either one of the two machines. There are different types of tasks, the type of task
  * a_i is denoted tp_i.
  *
  * The two machines are different: they process tasks with different speed. Furthermore, there is a learning curve
  * i.e. the longer is the production of the same type and the more efficient is the production process. The learning
  * curve depends on the current type, the previous type and the machine.
  *
  * The objective is to minimize the makespan.
  *
  * The model uses sequence variables and noOverlap constraints to model machines. The model uses also additional
  * integer variables, interval variables and sequence variables and constraints for the learning curve.
  */

object LearningCurve2 {

  val NbTypes = 5

  val NbTasks   = 50

  val TaskType = Vector(
    3, 3, 1, 1, 1, 1, 2, 0, 0, 2,
    4, 4, 3, 3, 2, 3, 1, 4, 4, 2,
    2, 1, 4, 2, 2, 0, 3, 3, 2, 1,
    2, 1, 4, 3, 3, 0, 2, 0, 0, 3,
    2, 0, 3, 2, 2, 4, 1, 2, 4, 3
  )

  val NbTaskTtypes = TaskType.max + 1

  val TaskIndex : Vector[Int]= (for (i <- 0 until NbTasks) yield i)(collection.breakOut)

  val TaskDurM1 = Vector(
    4, 17,  4,  7, 17, 14,  2, 14,  2,  8,
    11, 14,  4, 18,  3,  2,  9,  2,  9, 17,
    18, 19,  5,  8, 19, 12, 17, 11,  6,  3,
    13,  6, 19,  7,  1,  3, 13,  5,  3,  6,
    11, 16, 12, 14, 12, 17,  8,  8,  6,  6
  )

  val TaskDurM2 = Vector(
    12,  3, 12, 15,  4,  9, 14,  2,  5,  9,
    10, 14,  7,  1, 11,  3, 15, 19,  8,  2,
    18, 17, 19, 18, 15, 14,  6,  6,  1,  2,
    3, 19, 18,  2,  7, 16,  1, 18, 10, 14,
    2,  3, 14,  1,  1,  6, 19,  5, 17,  4
  )

  val LearningCurve = Vector(
    Vector((0, CpModel.IntervalMax, 100)),
    Vector((0, 5, 55), (5, 10, 65), (10, 15, 70), (15, 20, 73), (20, 25, 75), (25, 30, 77), (30, 35, 78), (35, 40, 80), (40, CpModel.IntervalMax, 100)),
    Vector((0, 5, 59), (5, 10, 69), (10, 15, 73), (15, 20, 76), (20, 25, 78), (25, 30, 80), (30, CpModel.IntervalMax, 100)),
    Vector((0, 5, 63), (5, 10, 73), (10, 15, 77), (15, 20, 79), (20, 25, 81), (25, CpModel.IntervalMax, 100)),
    Vector((0, 5, 68), (5, 10, 77), (10, 15, 80), (15, CpModel.IntervalMax, 100))
  )

  val NbLearningCurves = LearningCurve.length

  val LearningCurveM1 = Vector(
    Vector(0, 1, 3, 4, 1),
    Vector(1, 0, 1, 4, 1),
    Vector(3, 1, 0, 1, 2),
    Vector(4, 4, 1, 0, 3),
    Vector(1, 1, 3, 2, 0)
  )

  val LearningCurveM2 = Vector(
    Vector(0, 4, 1, 1, 4),
    Vector(4, 0, 1, 3, 2),
    Vector(1, 1, 0, 1, 1),
    Vector(1, 3, 1, 0, 1),
    Vector(4, 2, 1, 1, 0)
  )


  // Initial Type on Machine M1 and M2
  val InitialType1: Int = -1
  val InitialType2: Int = -1

  val LastProductionTime1: Int = 0
  val LastProductionTime2: Int = 0

  implicit var model: CpModel = _

  var s1: IntervalSequenceVar = _
  var s2: IntervalSequenceVar = _

  var si1: IntervalSequenceVar = _
  var si2: IntervalSequenceVar = _

  var o: List[IntVar] = _

  var IndexOfTask : Map[IntervalVar, Int] = _

  var lca1: Array[IntervalVar] = _
  var lca2: Array[IntervalVar] = _
  var lcpa1: Array[Array[IntervalVar]] = _
  var lcpa2: Array[Array[IntervalVar]] = _
  var lcm1: Array[IntExpr] = _
  var lcm2: Array[IntExpr] = _

  var prevType: List[IntVar] = _

  def build(): CpModel = {

    model = CpModel("LearningCurve2")

    var learningCurveStepFunction: Vector[NumToNumStepFunction] = (for (t <- 0 until NbLearningCurves) yield {
      val f = model.numToNumStepFunction
      f.setValue(0, IntervalMax, 100)
      for ((s, e, v) <- LearningCurve(t)) {
        f.setValue(s, e, v)
      }
      f
    })(collection.breakOut)

    // interval variables

    val a: Array[IntervalVar] = (for (i <- 0 until NbTasks) yield
      model.intervalVar(name="A" + i + "_TP" + TaskType(i)))(collection.breakOut)
    val a1: Array[IntervalVar] = (for (i <- 0 until NbTasks) yield
      model.intervalVar(name="A" + i + "_M1_TP" + TaskType(i)))(collection.breakOut)
    val a2: Array[IntervalVar] = (for (i <- 0 until NbTasks) yield
      model.intervalVar(name="A" + i + "_M2_TP" + TaskType(i)))(collection.breakOut)

    // interval variables for learning curve
    lca1 = (for (i <- 0 until NbTasks) yield
      model.intervalVar(name="LCA" + i + "_M1_TP" + TaskType(i)))(collection.breakOut)
    lca2 = (for (i <- 0 until NbTasks) yield
      model.intervalVar(name="LCA" + i + "_M2_TP" + TaskType(i)))(collection.breakOut)

    lcpa1 = Array.ofDim[IntervalVar](NbTasks, NbLearningCurves)
    lcpa2 = Array.ofDim[IntervalVar](NbTasks, NbLearningCurves)
    for (i <- 0 until NbTasks) {
      lcpa1(i) =   (for (c <- 0 until NbLearningCurves) yield
        model.intervalVar(name="LCA_" + i + "_" + c + "_M1_TP" + TaskType(i)))(collection.breakOut)
      lcpa2(i) =   (for (c <- 0 until NbLearningCurves) yield
        model.intervalVar(name="LCA_" + i + "_" + c + "_M2_TP" + TaskType(i)))(collection.breakOut)
    }

    // offset variables for learning curve
    o = (for (i <- 0 until NbTasks) yield model.intVar(name="O" + i + "_TP" + TaskType(i)))(collection.breakOut)

    prevType = (for (i <- 0 until NbTasks) yield model.intVar(max=NbTypes-1, name="PrevType" + i + "_TP" + TaskType(i)))(collection.breakOut)

    // interval variables on machines are optional

    for (i <- 0 until NbTasks) {
      a1(i).setSizeMin(TaskDurM1(i))
      a1(i).setOptional()
      a2(i).setSizeMin(TaskDurM2(i))
      a2(i).setOptional()
      model.add(alternative(a(i), Array(a1(i), a2(i))))

      // interval variables for learning curve: time offset, intensity and relationship with the corresponding interval
      // variables
      lca1(i).setSizeMin(TaskDurM1(i))
      lca1(i).setOptional()
      model.add(startAtStart(lca1(i), a1(i), o(i)))
      model.add(presenceOf(lca1(i)) == presenceOf(a1(i)))
      model.add(sizeOf(a1(i)) == lengthOf(lca1(i)))
      lca2(i).setSizeMin(TaskDurM2(i))
      lca2(i).setOptional()
      model.add(startAtStart(lca2(i), a2(i), o(i)))
      model.add(presenceOf(lca2(i)) == presenceOf(a2(i)))
      model.add(sizeOf(a2(i)) == lengthOf(lca2(i)))
      // learning curve interval variable is an alternative of learning curve pattern interval variables
      for (j <- 0 until NbLearningCurves) {
        lcpa1(i)(j).setSizeMin(TaskDurM1(i))
        lcpa1(i)(j).setSizeMax(TaskDurM1(i))
        lcpa1(i)(j).setOptional()
        lcpa1(i)(j).setIntensity(learningCurveStepFunction(j))
        model.add(startAtStart(lcpa1(i)(j), a1(i), o(i)))
        lcpa2(i)(j).setSizeMin(TaskDurM2(i))
        lcpa2(i)(j).setSizeMax(TaskDurM2(i))
        lcpa2(i)(j).setOptional()
        lcpa2(i)(j).setIntensity(learningCurveStepFunction(j))
        model.add(startAtStart(lcpa2(i)(j), a2(i), o(i)))
      }
      model.add(alternative(lca1(i), lcpa1(i)))
      model.add(alternative(lca2(i), lcpa2(i)))
    }

    IndexOfTask = (for (i <- 0 until NbTasks) yield (a1(i) -> i))(collection.breakOut)
    IndexOfTask = IndexOfTask ++ (for (i <- 0 until NbTasks) yield (a2(i) -> i))(collection.breakOut)

    // sequence variables (one per machine)
    s1 = model.intervalSequenceVar(a1, TaskType.toArray)
    s2 = model.intervalSequenceVar(a2, TaskType.toArray)

    // no overlap constraints with setup times
    model.add(noOverlap(s1))
    model.add(noOverlap(s2))

    // sequence variables with index as type
    si1 = model.intervalSequenceVar(a1, TaskIndex.toArray)
    si2 = model.intervalSequenceVar(a2, TaskIndex.toArray)
    model.add(noOverlap(si1))
    model.add(noOverlap(si2))
    model.add(sameSequence(s1, si1))
    model.add(sameSequence(s2, si2))


    // learning curve constraint model for time offsets
    lcm1 = Array.ofDim[IntExpr](NbTasks)
    lcm2 = Array.ofDim[IntExpr](NbTasks)
    for (i <- 0 until NbTasks) {

      model.add((typeOfPrevious(s1, a1(i), InitialType1, TaskType(i)) != TaskType(i)) <= (o(i) == startOf(a1(i))))
      val prevOffsetExpr1 =  o(typeOfPrevious(si1, a1(i), i, i))
      model.add((typeOfPrevious(s1, a1(i), InitialType1, -1) == TaskType(i)) <= (o(i) == prevOffsetExpr1 + startOf(a1(i)) - endOfPrevious(s1, a1(i), LastProductionTime1)))

      model.add((typeOfPrevious(s2, a2(i), InitialType2, TaskType(i)) != TaskType(i)) <= (o(i) == startOf(a2(i))))
      val prevOffsetExpr2 =  o(typeOfPrevious(si2, a2(i), i))
      model.add((typeOfPrevious(s2, a2(i), InitialType1, -1) == TaskType(i)) <= (o(i) == prevOffsetExpr2 + startOf(a2(i)) - endOfPrevious(s2, a2(i), LastProductionTime2)))

      model.add((typeOfPrevious(s1, a1(i), InitialType1, TaskType(i)) != TaskType(i)) <= (prevType(i) == typeOfPrevious(s1, a1(i), if (InitialType1 < 0) TaskType(i) else InitialType1, TaskType(i))))
      val prevTypeExpr1 =  prevType(typeOfPrevious(si1, a1(i), i, i))
      model.add((typeOfPrevious(s1, a1(i), InitialType1, -1) == TaskType(i)) <= (prevType(i) == prevTypeExpr1))

      model.add((typeOfPrevious(s2, a2(i), InitialType2, TaskType(i)) != TaskType(i)) <= (prevType(i) == typeOfPrevious(s2, a2(i), if (InitialType2 < 0) TaskType(i) else InitialType2, TaskType(i))))
      val prevTypeExpr2 =  prevType(typeOfPrevious(si2, a2(i), i, i))
      model.add((typeOfPrevious(s2, a2(i), InitialType2, -1) == TaskType(i)) <= (prevType(i) == prevTypeExpr2))

      val plcpa1: List[IntExpr] = (for (j <- 0 until NbLearningCurves) yield presenceOf(lcpa1(i)(j)))(collection.breakOut)
      val lcm1Indexes: List[Int] = (for (j <- 0 until NbLearningCurves) yield LearningCurveM1(j)(TaskType(i)))(collection.breakOut)
      lcm1(i) = lcm1Indexes(prevType(i))
      model.add(presenceOf(lca1(i)) <= (plcpa1(lcm1(i)) == 1))

      val plcpa2: List[IntExpr] = (for (j <- 0 until NbLearningCurves) yield presenceOf(lcpa2(i)(j)))(collection.breakOut)
      val lcm2Indexes: List[Int] = (for (j <- 0 until NbLearningCurves) yield LearningCurveM2(j)(TaskType(i)))(collection.breakOut)
      lcm2(i) = lcm2Indexes(prevType(i))
      model.add(presenceOf(lca2(i)) <= (plcpa2(lcm2(i)) == 1))
    }

    // minimize makespan
    model.add(minimize(max(for (v <- a) yield endOf(v))))

    // exclude from the search the learning curve interval variables
    model.setSearchPhases(searchPhase(a ++ a1 ++ a2))

    model
  }

  def solve(timeLimit: Double = Infinity, failLimit : Int = 0, solutionLimit: Int = IntMax, logPeriod: Int = IntMin): Boolean = {

    println(s"Solving model $model....")

//    model.exportModel("learningcurve2.cpo")

    val status = model.solve(timeLimit, failLimit, solutionLimit, logPeriod)

    if (status) {
      println(s"Solution status: $status")
      println("Solution with objective " + model.getObjectiveValue())

//      System.out.println("Machine 1: ")
//      for (x <- s1) {
//        System.out.println(model.getDomain(x))
//      }

      System.out.println("Machine 1: ")
      for (x <- si1) {
        System.out.print(model.getDomain(x))
        System.out.print("; nominal duration : ")
        System.out.print(TaskDurM1(IndexOfTask(x)))
        System.out.print("; type : ")
        System.out.print(TaskType(IndexOfTask(x)))
        System.out.print("; previous : ")
        System.out.print(model.getValue(typeOfPrevious(si1, x, IndexOfTask(x), IndexOfTask(x))))
        System.out.print("; previous type : ")
        val prevTypeVar = prevType(IndexOfTask(x))
        System.out.print(model.getMin(prevTypeVar) + ".." + model.getMax(prevTypeVar))
        System.out.print("; learning curve : ")
        System.out.print(model.getValue(lcm1(IndexOfTask(x))))
        System.out.print("; time offset : ")
        val v = o(IndexOfTask(x))
        System.out.print(model.getMin(v) + ".." + model.getMax(v))
        System.out.println()
      }

//      System.out.println("Machine 2: ")
//      for (x <- s2) {
//        System.out.println(model.getDomain(x))
//      }

      System.out.println("Machine 2: ")
      for (x <- si2) {
        System.out.print(model.getDomain(x))
        System.out.print("; nominal duration : ")
        System.out.print(TaskDurM2(IndexOfTask(x)))
        System.out.print("; type : ")
        System.out.print(TaskType(IndexOfTask(x)))
        System.out.print("; previous : ")
        System.out.print(model.getValue(typeOfPrevious(si2, x, IndexOfTask(x), IndexOfTask(x))))
        System.out.print("; previous type : ")
        val prevTypeVar = prevType(IndexOfTask(x))
        System.out.print(model.getMin(prevTypeVar) + ".." + model.getMax(prevTypeVar))
        System.out.print("; learning curve : ")
        System.out.print(model.getValue(lcm2(IndexOfTask(x))))
        System.out.print("; time offset : ")
        val v = o(IndexOfTask(x))
        System.out.print(model.getMin(v) + ".." + model.getMax(v))
        System.out.println()
      }

    } else {
      System.out.println("No solution found.")
    }

    true
  }

  def run(): Boolean = {
    val model = build()
    val status = solve()
    model.end()
    status
  }

  def main(args: Array[String]): Unit = {
    run()
  }

}

/**
  * Solution obtained with time limit set to 600s on a Lenovo T460s
  *
  * Solution with objective 190.0
  * Machine 1:
  * A25_M1_TP0[1: 0 -- (12)12 --> 12]; nominal duration : 12; type : 0; previous : 25; previous type : 0..0; learning curve : 0; time offset : 0..0
  * A38_M1_TP0[1: 12 -- (3)3 --> 15]; nominal duration : 3; type : 0; previous : 25; previous type : 0..0; learning curve : 0; time offset : 0..0
  * A35_M1_TP0[1: 15 -- (3)3 --> 18]; nominal duration : 3; type : 0; previous : 38; previous type : 0..0; learning curve : 0; time offset : 0..0
  * A6_M1_TP2[1: 18 -- (4)4 --> 22]; nominal duration : 2; type : 2; previous : 35; previous type : 0..0; learning curve : 3; time offset : 18..18
  * A24_M1_TP2[1: 22 -- (26)26 --> 48]; nominal duration : 19; type : 2; previous : 6; previous type : 0..0; learning curve : 3; time offset : 18..18
  * A20_M1_TP2[1: 48 -- (18)18 --> 66]; nominal duration : 18; type : 2; previous : 24; previous type : 0..0; learning curve : 3; time offset : 18..18
  * A14_M1_TP2[1: 66 -- (3)3 --> 69]; nominal duration : 3; type : 2; previous : 20; previous type : 0..0; learning curve : 3; time offset : 18..18
  * A23_M1_TP2[1: 69 -- (8)8 --> 77]; nominal duration : 8; type : 2; previous : 14; previous type : 0..0; learning curve : 3; time offset : 18..18
  * A15_M1_TP3[1: 77 -- (4)4 --> 81]; nominal duration : 2; type : 3; previous : 23; previous type : 2..2; learning curve : 1; time offset : 77..77
  * A8_M1_TP0[1: 81 -- (3)3 --> 84]; nominal duration : 2; type : 0; previous : 15; previous type : 3..3; learning curve : 4; time offset : 81..81
  * A37_M1_TP0[1: 84 -- (7)7 --> 91]; nominal duration : 5; type : 0; previous : 8; previous type : 3..3; learning curve : 4; time offset : 81..81
  * A39_M1_TP3[1: 91 -- (9)9 --> 100]; nominal duration : 6; type : 3; previous : 37; previous type : 0..0; learning curve : 4; time offset : 91..91
  * A42_M1_TP3[1: 100 -- (16)16 --> 116]; nominal duration : 12; type : 3; previous : 39; previous type : 0..0; learning curve : 4; time offset : 91..91
  * A0_M1_TP3[1: 116 -- (4)4 --> 120]; nominal duration : 4; type : 3; previous : 42; previous type : 0..0; learning curve : 4; time offset : 91..91
  * A12_M1_TP3[1: 120 -- (4)4 --> 124]; nominal duration : 4; type : 3; previous : 0; previous type : 0..0; learning curve : 4; time offset : 91..91
  * A34_M1_TP3[1: 124 -- (1)1 --> 125]; nominal duration : 1; type : 3; previous : 12; previous type : 0..0; learning curve : 4; time offset : 91..91
  * A48_M1_TP4[1: 125 -- (9)9 --> 134]; nominal duration : 6; type : 4; previous : 34; previous type : 3..3; learning curve : 3; time offset : 125..125
  * A22_M1_TP4[1: 134 -- (7)7 --> 141]; nominal duration : 5; type : 4; previous : 48; previous type : 3..3; learning curve : 3; time offset : 125..125
  * A17_M1_TP4[1: 141 -- (3)3 --> 144]; nominal duration : 2; type : 4; previous : 22; previous type : 3..3; learning curve : 3; time offset : 125..125
  * A31_M1_TP1[1: 144 -- (10)10 --> 154]; nominal duration : 6; type : 1; previous : 17; previous type : 4..4; learning curve : 1; time offset : 144..144
  * A3_M1_TP1[1: 154 -- (10)10 --> 164]; nominal duration : 7; type : 1; previous : 31; previous type : 4..4; learning curve : 1; time offset : 144..144
  * A16_M1_TP1[1: 164 -- (12)12 --> 176]; nominal duration : 9; type : 1; previous : 3; previous type : 4..4; learning curve : 1; time offset : 144..144
  * A46_M1_TP1[1: 176 -- (10)10 --> 186]; nominal duration : 8; type : 1; previous : 16; previous type : 4..4; learning curve : 1; time offset : 144..144
  * A2_M1_TP1[1: 186 -- (4)4 --> 190]; nominal duration : 4; type : 1; previous : 46; previous type : 4..4; learning curve : 1; time offset : 144..144
  * Machine 2:
  * A18_M2_TP4[1: 0 -- (11)11 --> 11]; nominal duration : 8; type : 4; previous : 18; previous type : 4..4; learning curve : 0; time offset : 0..0
  * A43_M2_TP2[1: 11 -- (2)2 --> 13]; nominal duration : 1; type : 2; previous : 18; previous type : 4..4; learning curve : 1; time offset : 11..11
  * A44_M2_TP2[1: 13 -- (2)2 --> 15]; nominal duration : 1; type : 2; previous : 43; previous type : 4..4; learning curve : 1; time offset : 11..11
  * A36_M2_TP2[1: 15 -- (2)2 --> 17]; nominal duration : 1; type : 2; previous : 44; previous type : 4..4; learning curve : 1; time offset : 11..11
  * A28_M2_TP2[1: 17 -- (2)2 --> 19]; nominal duration : 1; type : 2; previous : 36; previous type : 4..4; learning curve : 1; time offset : 11..11
  * A9_M2_TP2[1: 19 -- (13)13 --> 32]; nominal duration : 9; type : 2; previous : 28; previous type : 4..4; learning curve : 1; time offset : 11..11
  * A40_M2_TP2[1: 32 -- (3)3 --> 35]; nominal duration : 2; type : 2; previous : 9; previous type : 4..4; learning curve : 1; time offset : 11..11
  * A47_M2_TP2[1: 35 -- (7)7 --> 42]; nominal duration : 5; type : 2; previous : 40; previous type : 4..4; learning curve : 1; time offset : 11..11
  * A30_M2_TP2[1: 42 -- (4)4 --> 46]; nominal duration : 3; type : 2; previous : 47; previous type : 4..4; learning curve : 1; time offset : 11..11
  * A19_M2_TP2[1: 46 -- (3)3 --> 49]; nominal duration : 2; type : 2; previous : 30; previous type : 4..4; learning curve : 1; time offset : 11..11
  * A4_M2_TP1[1: 49 -- (7)7 --> 56]; nominal duration : 4; type : 1; previous : 19; previous type : 2..2; learning curve : 1; time offset : 49..49
  * A21_M2_TP1[1: 56 -- (24)24 --> 80]; nominal duration : 17; type : 1; previous : 4; previous type : 2..2; learning curve : 1; time offset : 49..49
  * A27_M2_TP3[1: 80 -- (9)9 --> 89]; nominal duration : 6; type : 3; previous : 21; previous type : 1..1; learning curve : 3; time offset : 80..80
  * A26_M2_TP3[1: 89 -- (8)8 --> 97]; nominal duration : 6; type : 3; previous : 27; previous type : 1..1; learning curve : 3; time offset : 80..80
  * A1_M2_TP3[1: 97 -- (4)4 --> 101]; nominal duration : 3; type : 3; previous : 26; previous type : 1..1; learning curve : 3; time offset : 80..80
  * A49_M2_TP3[1: 101 -- (5)5 --> 106]; nominal duration : 4; type : 3; previous : 1; previous type : 1..1; learning curve : 3; time offset : 80..80
  * A33_M2_TP3[1: 106 -- (2)2 --> 108]; nominal duration : 2; type : 3; previous : 49; previous type : 1..1; learning curve : 3; time offset : 80..80
  * A13_M2_TP3[1: 108 -- (1)1 --> 109]; nominal duration : 1; type : 3; previous : 33; previous type : 1..1; learning curve : 3; time offset : 80..80
  * A29_M2_TP1[1: 109 -- (4)4 --> 113]; nominal duration : 2; type : 1; previous : 13; previous type : 3..3; learning curve : 3; time offset : 109..109
  * A7_M2_TP0[1: 113 -- (3)3 --> 116]; nominal duration : 2; type : 0; previous : 29; previous type : 1..1; learning curve : 4; time offset : 113..113
  * A32_M2_TP4[1: 116 -- (22)22 --> 138]; nominal duration : 18; type : 4; previous : 7; previous type : 0..0; learning curve : 4; time offset : 116..116
  * A10_M2_TP4[1: 138 -- (10)10 --> 148]; nominal duration : 10; type : 4; previous : 32; previous type : 0..0; learning curve : 4; time offset : 116..116
  * A45_M2_TP4[1: 148 -- (6)6 --> 154]; nominal duration : 6; type : 4; previous : 10; previous type : 0..0; learning curve : 4; time offset : 116..116
  * A11_M2_TP4[1: 154 -- (14)14 --> 168]; nominal duration : 14; type : 4; previous : 45; previous type : 0..0; learning curve : 4; time offset : 116..116
  * A5_M2_TP1[1: 168 -- (15)15 --> 183]; nominal duration : 9; type : 1; previous : 11; previous type : 4..4; learning curve : 2; time offset : 168..168
  * A41_M2_TP0[1: 183 -- (5)5 --> 188]; nominal duration : 3; type : 0; previous : 5; previous type : 1..1; learning curve : 4; time offset : 183..183
  *
  */
