/*
 *  Source file provided under Apache License, Version 2.0, January 2004,
 *  http://www.apache.org/licenses/
 *  (c) Copyright DecisionBrain SAS 2016,2019
 */

package com.decisionbrain.cplex.mp

import com.decisionbrain.cplex.Modeler._
import com.decisionbrain.cplex.mp.MpModel._
import com.decisionbrain.cplex.NumExprNumeric._
import com.decisionbrain.cplex.NumExpr
import com.decisionbrain.cplex.NumVar

object Nurses {

  //
  //
  //

  object WeekDay extends Enumeration {
    type WeekDay = Value
    val Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday = Value
  }

  //
  // Data
  //

  val SKILLS = List("Anaesthesiology",
    "Cardiac Care",
    "Geriatrics",
    "Oncology",
    "Pediatrics"
  )

  val DEPTS = List("Consultation",
    "Emergency"
  )

  val NURSES = List(("Anne", 11, 1, 25),
    ("Bethanie", 4, 5, 28),
    ("Betsy", 2, 2, 17),
    ("Cathy", 2, 2, 17),
    ("Cecilia", 9, 5, 38),
    ("Chris", 11, 4, 38),
    ("Cindy", 5, 2, 21),
    ("David", 1, 2, 15),
    ("Debbie", 7, 2, 24),
    ("Dee", 3, 3, 21),
    ("Gloria", 8, 2, 25),
    ("Isabelle", 3, 1, 16),
    ("Jane", 3, 4, 23),
    ("Janelle", 4, 3, 22),
    ("Janice", 2, 2, 17),
    ("Jemma", 2, 4, 22),
    ("Joan", 5, 3, 24),
    ("Joyce", 8, 3, 29),
    ("Jude", 4, 3, 22),
    ("Julie", 6, 2, 22),
    ("Juliet", 7, 4, 31),
    ("Kate", 5, 3, 24),
    ("Nancy", 8, 4, 32),
    ("Nathalie", 9, 5, 38),
    ("Nicole", 0, 2, 14),
    ("Patricia", 1, 1, 13),
    ("Patrick", 6, 1, 19),
    ("Roberta", 3, 5, 26),
    ("Suzanne", 5, 1, 18),
    ("Vickie", 7, 1, 20),
    ("Wendie", 5, 2, 21),
    ("Zoe", 8, 3, 29)
  )

  val SHIFTS = List(("Emergency", WeekDay.Monday, 2, 8, 3, 5),
    ("Emergency", WeekDay.Monday, 8, 12, 4, 7),
    ("Emergency", WeekDay.Monday, 12, 18, 2, 5),
    ("Emergency", WeekDay.Monday, 18, 2, 3, 7),
    ("Consultation", WeekDay.Monday, 8, 12, 10, 13),
    ("Consultation", WeekDay.Monday, 12, 18, 8, 12),
    ("Cardiac Care", WeekDay.Monday, 8, 12, 10, 13),
    ("Cardiac Care", WeekDay.Monday, 12, 18, 8, 12),
    ("Emergency", WeekDay.Tuesday, 8, 12, 4, 7),
    ("Emergency", WeekDay.Tuesday, 12, 18, 2, 5),
    ("Emergency", WeekDay.Tuesday, 18, 2, 3, 7),
    ("Consultation", WeekDay.Tuesday, 8, 12, 10, 13),
    ("Consultation", WeekDay.Tuesday, 12, 18, 8, 12),
    ("Cardiac Care", WeekDay.Tuesday, 8, 12, 4, 7),
    ("Cardiac Care", WeekDay.Tuesday, 12, 18, 2, 5),
    ("Cardiac Care", WeekDay.Tuesday, 18, 2, 3, 7),
    ("Emergency", WeekDay.Wednesday, 2, 8, 3, 5),
    ("Emergency", WeekDay.Wednesday, 8, 12, 4, 7),
    ("Emergency", WeekDay.Wednesday, 12, 18, 2, 5),
    ("Emergency", WeekDay.Wednesday, 18, 2, 3, 7),
    ("Consultation", WeekDay.Wednesday, 8, 12, 10, 13),
    ("Consultation", WeekDay.Wednesday, 12, 18, 8, 12),
    ("Emergency", WeekDay.Thursday, 2, 8, 3, 5),
    ("Emergency", WeekDay.Thursday, 8, 12, 4, 7),
    ("Emergency", WeekDay.Thursday, 12, 18, 2, 5),
    ("Emergency", WeekDay.Thursday, 18, 2, 3, 7),
    ("Consultation", WeekDay.Thursday, 8, 12, 10, 13),
    ("Consultation", WeekDay.Thursday, 12, 18, 8, 12),
    ("Emergency", WeekDay.Friday, 2, 8, 3, 5),
    ("Emergency", WeekDay.Friday, 8, 12, 4, 7),
    ("Emergency", WeekDay.Friday, 12, 18, 2, 5),
    ("Emergency", WeekDay.Friday, 18, 2, 3, 7),
    ("Consultation", WeekDay.Friday, 8, 12, 10, 13),
    ("Consultation", WeekDay.Friday, 12, 18, 8, 12),
    ("Emergency", WeekDay.Saturday, 2, 12, 5, 7),
    ("Emergency", WeekDay.Saturday, 12, 20, 7, 9),
    ("Emergency", WeekDay.Saturday, 20, 2, 12, 12),
    ("Emergency", WeekDay.Sunday, 2, 12, 5, 7),
    ("Emergency", WeekDay.Sunday, 12, 20, 7, 9),
    ("Emergency", WeekDay.Sunday, 20, 2, 12, 12),
    ("Geriatrics", WeekDay.Sunday, 8, 10, 2, 5)
  )

  val NURSE_SKILLS = Map("Anne" -> List("Anaesthesiology", "Oncology", "Pediatrics"),
    "Betsy" -> List("Cardiac Care"),
    "Cathy" -> List("Anaesthesiology"),
    "Cecilia" -> List("Anaesthesiology", "Oncology", "Pediatrics"),
    "Chris" -> List("Cardiac Care", "Oncology", "Geriatrics"),
    "Gloria" -> List("Pediatrics"),
    "Jemma" -> List("Cardiac Care"),
    "Joyce" -> List("Anaesthesiology", "Pediatrics"),
    "Julie" -> List("Geriatrics"),
    "Juliet" -> List("Pediatrics"),
    "Kate" -> List("Pediatrics"),
    "Nancy" -> List("Cardiac Care"),
    "Nathalie" -> List("Anaesthesiology", "Geriatrics"),
    "Patrick" -> List("Oncology"),
    "Suzanne" -> List("Pediatrics"),
    "Wendie" -> List("Geriatrics"),
    "Zoe" -> List("Cardiac Care")
  )

  val VACATIONS = List(("Anne", WeekDay.Friday),
    ("Anne", WeekDay.Sunday),
    ("Cathy", WeekDay.Thursday),
    ("Cathy", WeekDay.Tuesday),
    ("Joan", WeekDay.Thursday),
    ("Joan", WeekDay.Saturday),
    ("Juliet", WeekDay.Monday),
    ("Juliet", WeekDay.Tuesday),
    ("Juliet", WeekDay.Thursday),
    ("Nathalie", WeekDay.Sunday),
    ("Nathalie", WeekDay.Thursday),
    ("Isabelle", WeekDay.Monday),
    ("Isabelle", WeekDay.Thursday),
    ("Patricia", WeekDay.Saturday),
    ("Patricia", WeekDay.Wednesday),
    ("Nicole", WeekDay.Friday),
    ("Nicole", WeekDay.Wednesday),
    ("Jude", WeekDay.Tuesday),
    ("Jude", WeekDay.Friday),
    ("Debbie", WeekDay.Saturday),
    ("Debbie", WeekDay.Wednesday),
    ("Joyce", WeekDay.Sunday),
    ("Joyce", WeekDay.Thursday),
    ("Chris", WeekDay.Thursday),
    ("Chris", WeekDay.Tuesday),
    ("Cecilia", WeekDay.Friday),
    ("Cecilia", WeekDay.Wednesday),
    ("Patrick", WeekDay.Saturday),
    ("Patrick", WeekDay.Sunday),
    ("Cindy", WeekDay.Sunday),
    ("Dee", WeekDay.Tuesday),
    ("Dee", WeekDay.Friday),
    ("Jemma", WeekDay.Friday),
    ("Jemma", WeekDay.Wednesday),
    ("Bethanie", WeekDay.Wednesday),
    ("Bethanie", WeekDay.Tuesday),
    ("Betsy", WeekDay.Monday),
    ("Betsy", WeekDay.Thursday),
    ("David", WeekDay.Monday),
    ("Gloria", WeekDay.Monday),
    ("Jane", WeekDay.Saturday),
    ("Jane", WeekDay.Sunday),
    ("Janelle", WeekDay.Wednesday),
    ("Janelle", WeekDay.Friday),
    ("Julie", WeekDay.Sunday),
    ("Kate", WeekDay.Tuesday),
    ("Kate", WeekDay.Monday),
    ("Nancy", WeekDay.Sunday),
    ("Roberta", WeekDay.Friday),
    ("Roberta", WeekDay.Saturday),
    ("Janice", WeekDay.Tuesday),
    ("Janice", WeekDay.Friday),
    ("Suzanne", WeekDay.Monday),
    ("Vickie", WeekDay.Wednesday),
    ("Vickie", WeekDay.Friday),
    ("Wendie", WeekDay.Thursday),
    ("Wendie", WeekDay.Saturday),
    ("Zoe", WeekDay.Saturday),
    ("Zoe", WeekDay.Sunday)
  )

  val NURSE_ASSOCIATIONS = List(("Isabelle", "Dee"),
    ("Anne", "Patrick")
  )

  val NURSE_INCOMPATIBILITIES = List(("Patricia", "Patrick"),
    ("Janice", "Wendie"),
    ("Suzanne", "Betsy"),
    ("Janelle", "Jane"),
    ("Gloria", "David"),
    ("Dee", "Jemma"),
    ("Bethanie", "Dee"),
    ("Roberta", "Zoe"),
    ("Nicole", "Patricia"),
    ("Vickie", "Dee"),
    ("Joan", "Anne")
  )

  val SKILL_REQUIREMENTS: List[TSkillRequirement] = List(("Emergency", "Cardiac Care", 1))

  val DEFAULT_WORK_RULES: TWorkRules = Tuple1(40)

  //
  // Tuples
  //

  type TNurse = (String, Int, Int, Int)
  type TWorkRules = Tuple1[Int]
  type TShift = (String, WeekDay.WeekDay, Int, Int, Int, Int)
  type TVacation = (String, Int)
  type TNursePair = (String, String)
  type TSkillRequirement = (String, String, Int)

  //
  // Classes
  //

  class Nurse(tnurse: TNurse) {
    val (name, seniority, qualification, payRate) = tnurse
    override def toString = {
      name
    }
  }

  class Shift(val tshift: TShift) {
    val (department, day, startTime, endTime, minRequirement, maxRequirement) = tshift
    override def toString() = {
      val dept2 = department slice (0, 4) toUpperCase()
      val dayname = day.toString() slice (0, 3)
      s"$dept2-$dayname-$startTime"
    }
  }

  class ShiftActivity (val day: WeekDay.WeekDay, val startTimeOfDay: Int, val endTimeOfDay: Int) {
    assert(startTimeOfDay >= 0)
    assert(startTimeOfDay <= 24)
    assert(endTimeOfDay >= 0)
    assert(endTimeOfDay <= 24)

    val startDayIndex = day.id
    val startTime = toAbsTime(startDayIndex, startTimeOfDay)
    val endDayIndex = if (endTimeOfDay > startTimeOfDay) startDayIndex  else startDayIndex + 1
    val endTime = toAbsTime(endDayIndex, endTimeOfDay)

    assert(startTime < endTime)

    def duration() = (endTime - startTime)

    def overlaps(otherShift: ShiftActivity): Boolean = {
      (otherShift.endTime > startTime) && (otherShift.startTime < endTime)
    }

    def toAbsTime(dayIndex: Int, timeOfDay: Int): Int = {
      val oneDayInHours = 24
      var time: Int = oneDayInHours * (dayIndex - 1)
      time += timeOfDay
      time
    }
  }


  var numberOfOverlaps: Int = _
  var workRules: TWorkRules = _
  var departments: List[String] = _
  var skills: List[String] = _
  var shifts: List[Shift] = _
  var nurses: List[Nurse] = _
  var nurseSkills: Map[String, List[String]] = _
  var skillRequirements: List[TSkillRequirement] = _
  var vacations: List[(String, WeekDay.Value)] = _
  var nurseAssociations: List[(String, String)] = _
  var nurseIncompatibilities: List[(String, String)] = _

  def loadData(model: MpModel,
               DEPTS: List[String],
               SKILLS: List[String],
               SHIFTS: List[TShift],
               NURSES: List[TNurse],
               NURSE_SKILLS: Map[String, List[String]],
               VACATIONS: List[(String, WeekDay.Value)],
               NURSE_ASSOCIATIONS: List[(String, String)],
               NURSE_INCOMPATIBILITIES: List[(String, String)]) = {
    numberOfOverlaps = 0
    workRules = DEFAULT_WORK_RULES
    departments = DEPTS
    skills = SKILLS
    shifts = for (s <- SHIFTS) yield new Shift(s)
    nurses = for (n <- NURSES) yield new Nurse(n)
    nurseSkills = NURSE_SKILLS
    skillRequirements = SKILL_REQUIREMENTS
    vacations = VACATIONS
    nurseAssociations = NURSE_ASSOCIATIONS
    nurseIncompatibilities = NURSE_INCOMPATIBILITIES
  }

  var vacationsByNurse: Map[Nurse, List[WeekDay.Value]] = _
  var shiftActivities: Map[Shift, ShiftActivity] = _
  var nursesById: Map[String, Nurse] = _

  def setupData(model: MpModel) = {
    vacationsByNurse = (for (n <- nurses) yield (n, for ((nurseId, day) <- vacations if nurseId == n.name) yield day))(collection.breakOut)
    // compute shift activities (start, end duration)
    shiftActivities = (for (s <- shifts) yield s -> new ShiftActivity(s.day, s.startTime, s.endTime))(collection.breakOut)
    nursesById = (for (n <- nurses) yield n.name -> n)(collection.breakOut)
  }

  // variables
  var nurseAssignmentVars: Map[(Nurse, Shift), NumVar] = _
  var nurseWorkTimeVars: Map[Nurse, NumExpr] = _
  var nurseOverAverageTimeVars: Map[Nurse, NumVar] = _
  var nurseUnderAverageTimeVars: Map[Nurse, NumVar] = _
  var averageNurseWorkTime: NumVar = _

  // KPIs
  var totalNumberOfAssignments: NumExpr = _
  var totalSalaryCost: NumExpr = _
  var totalFairness: NumExpr = _

  def setupVariables(model: MpModel) = {
    val (allNurses, allShifts) = (nurses, shifts)
    nurseAssignmentVars = model.boolVars(allNurses, allShifts, namer= (n: Nurse, s: Shift) => "nurse_%s_assigned_to_shift_%s".format(n, s))
    nurseWorkTimeVars = model.numVars(allNurses, namer= (n: Nurse) => "nurse_%s_work_time".format(n))
    nurseOverAverageTimeVars = model.numVars(allNurses, namer= (n: Nurse) => "nurse_%s_over_averageçwork_time".format(n))
    nurseUnderAverageTimeVars = model.numVars(allNurses, namer= (n: Nurse) => "nurse_%s_under_averageçwork_time".format(n))
    averageNurseWorkTime = model.numVar(name="AverageNurseWorkTime")
  }

  def setupConstraints(implicit model: MpModel) = {

    val maxWorkTime = workRules._1

    // define average
    // notice the syntax below where the integer "nurses.length" is converted to an numeric expression: this automatic
    // conversion is possible as the model is declared as implicit
    model.add(averageNurseWorkTime * nurses.length == sum(for (n <- nurses) yield nurseWorkTimeVars(n)), "")

    // compute nurse work time , average and under, over
    for (n <- nurses) {
      val workTimeVar = nurseWorkTimeVars(n)
      model.add(workTimeVar == sum(for (s <- shifts) yield nurseAssignmentVars(n, s)*shiftActivities(s).duration()), s"work_time_$n")
      model.add(workTimeVar == averageNurseWorkTime + nurseOverAverageTimeVars(n) - nurseUnderAverageTimeVars(n), s"averag_work_time_$n")
      model.add(workTimeVar <= maxWorkTime, s"max_time_$n")
    }

    // vacations
    for (n <- nurses; vac <- vacationsByNurse(n); s <- shifts if s.day == vac)
          model.add(nurseAssignmentVars(n, s) == 0, s"medium_vacations_$n-$vac-$s")

    // a nurse cannot be assigned overlapping shifts
    numberOfOverlaps = 0
    for (s1 <- shifts; s2 <- shifts if s1 != s2 && shiftActivities(s1).overlaps(shiftActivities(s2))) {
      numberOfOverlaps += 1
      for (n <- nurses) {
        model.add(nurseAssignmentVars(n, s1) + nurseAssignmentVars(n, s2) <= 1, s"medium_overlapping_$s1-$s2-$n")
      }
    }

    // requirement
    for (s <- shifts)
      model.addRange(s.minRequirement, sum(for (n <- nurses) yield nurseAssignmentVars(n, s)), s.maxRequirement,
        s"medium_shift_$s")

    for ((dept, skill, required) <- skillRequirements if required > 0) {
      for (s <- shifts if dept == s.department) {
        model.add(sum(for (n <- nurses if nurseSkills.contains(n.name) && nurseSkills(n.name).contains(skill))
          yield nurseAssignmentVars(n, s)) >= required)
      }
    }

    // nurse-nurse associations
    var c = 0
    for ((id1, id2) <- nurseAssociations if nursesById.contains(id1) && nursesById.contains(id2)) {
      val n1 = nursesById(id1)
      val n2 = nursesById(id2)
      for (s <- shifts) {
        c += 1
        val ctName = s"medium_ct_nurse_assoc_$id1-$id2-$c"
        model.add(nurseAssignmentVars(n1, s) == nurseAssignmentVars(n2, s), ctName)
      }
    }

    // nurse-nurse incompatibilities
    c = 0
    for ((id1, id2) <- nurseIncompatibilities if nursesById.contains(id1) && nursesById.contains(id2)) {
      val n1 = nursesById(id1)
      val n2 = nursesById(id2)
      for (s <- shifts) {
        c += 1
        val ctName = s"medium_ct_nurse_incompat_$id1-$id2-$c"
        model.add(nurseAssignmentVars(n1, s) + nurseAssignmentVars(n2, s) <= 1, ctName)
      }
    }

  }

  def setupObjective(implicit model: MpModel) = {

    val nurseCosts = for (n <- nurses; s <- shifts) yield nurseAssignmentVars(n, s) * n.payRate * shiftActivities(s).duration()
    totalNumberOfAssignments = sum(for (n <- nurses; s <- shifts) yield nurseAssignmentVars(n, s))
    totalSalaryCost = nurseCosts.sum
    totalFairness = sum(for (n <- nurses) yield nurseOverAverageTimeVars(n)) +
      sum(for (n <- nurses) yield nurseUnderAverageTimeVars(n))

    model.add(minimize(totalSalaryCost + totalNumberOfAssignments + totalFairness))
  }

  def buildModel() : MpModel = {

    // declare the model implicit: it will allow to call methods on the companion object and also to convert
    // automatically an integer to a numeric expression when needed
    implicit val model = new MpModel("Nurses")

    loadData(model, DEPTS, SKILLS, SHIFTS, NURSES, NURSE_SKILLS, VACATIONS, NURSE_ASSOCIATIONS,
      NURSE_INCOMPATIBILITIES)
    setupData(model)
    setupVariables(model)
    setupConstraints(model)
    setupObjective(model)

    // export model to LP file
//    model.exportModel("nurses.lp")

    model
  }

  def printInformation(model: MpModel) = {
    println(s"#departments: = ${departments.length}")
    println(s"#skills: ${skills.length}")
    println(s"#shifts: ${shifts.length}")
    println(s"#nurses: ${nurses.length}")
    println(s"#vacations: ${vacations.length}")
    println(s"#nurse associations: ${nurseAssociations.length}")
    println(s"#incompatibilities: ${nurseIncompatibilities.length}")
    model.printInformation()
  }

  def printSolution(model: MpModel) = {


    println("*************************** Solution ***************************")

    println("KPI: Total salary cost=" + model.getValue(totalSalaryCost))
    println("KPI: Total number of assignments=" + model.getValue(totalNumberOfAssignments))
    println("KPI: AverageNurseWorkTime=" + model.getValue(averageNurseWorkTime))
    println("KPI: Total fairness=" + model.getValue(totalFairness))

    println("Allocation By Department:")
    for (d <- departments) {
      val values = for (n <- nurses; s <- shifts if s.department == d) yield model.getValue(nurseAssignmentVars(n, s))
      println("\t%s: %f".format(d, values.sum))
    }

    println("Cost By Department:")
    for (d <- departments) {
      val values = for (n <- nurses; s <- shifts if s.department == d)
        yield model.getValue(nurseAssignmentVars(n, s)) * n.payRate * shiftActivities(s).duration()
      println("\t%s: %f".format(d, values.sum))
    }

    println("Nurses Assignments")
    for (n <- nurses.sortWith((left, right) => left.name < right.name)) {
      val totalHours = for (s <- shifts) yield model.getValue(nurseAssignmentVars(n, s)) * shiftActivities(s).duration()
      println("\t%s: total hours:%f".format(n.name, totalHours.sum))
      for (s <- shifts) {
        if (model.getValue(nurseAssignmentVars(n, s)) == 1)
          println("\t\t%s: %s %d-%d".format(s.day, s.department, s.startTime, s.endTime))
      }
    }
  }

  def solve(model: MpModel): Option[Double] = {
    if (model.solve()) {
      println("solution for a cost of %f".format(model.getObjValue()))
      printInformation(model)
      printSolution(model)
      return Some(model.getObjValue())
    }
    else {
      println("* model is infeasible")
      return None
    }
  }

  def run(): Option[Double] = {
    val model = buildModel()
    val status = solve(model)
    model.end()
    status
  }

  def main(args: Array[String]): Unit = {
    run()
  }
}
