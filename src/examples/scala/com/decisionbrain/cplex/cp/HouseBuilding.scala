/*
 *  Source file provided under Apache License, Version 2.0, January 2004,
 *  http://www.apache.org/licenses/
 *  (c) Copyright DecisionBrain SAS 2016,2019
 */

package com.decisionbrain.cplex.cp

import com.decisionbrain.cplex.Modeler._
import com.decisionbrain.cplex.cp.CpModel._


/**
  * This problem schedule a series of tasks of varying durations where some tasks must finish before others start. And
  * assign workers to each of the tasks such that each worker is assigned to only one task to a given time. The
  * objective of the problem is to maximize the matching worker skill level to the tasks.
  */
object HouseBuilding {

  //
  // Data
  //

  // Planning parameters contains the number of houses and the max amount of periods for our schedule
  val PLANNING_PARAMETERS = (5, 318)

  val HOUSES = 1 until PLANNING_PARAMETERS._1 + 1

  val HORIZON = PLANNING_PARAMETERS._2

  val tasks = List(
    ("masonry",   35), // name, duration
    ("carpentry", 15),
    ("plumbing",  40),
    ("ceiling",   15),
    ("roofing",    5),
    ("painting",  10),
    ("windows",    5),
    ("facade",    10),
    ("garden",     5),
    ("moving",     5)
  )

  val precedences = List(
    ("masonry",   "carpentry"), // before task, after task
    ("masonry",   "plumbing"),
    ("masonry",   "ceiling"),
    ("carpentry", "roofing"),
    ("ceiling",   "painting"),
    ("roofing",   "windows"),
    ("roofing",   "facade"),
    ("plumbing",  "facade"),
    ("roofing",   "garden"),
    ("plumbing",  "garden"),
    ("windows",   "moving"),
    ("facade",    "moving"),
    ("garden",    "moving"),
    ("painting",  "moving")
  )

  val workers = List("Joe", "Jack", "Jim")

  val skills = List(
      ("Joe",  "masonry",   9), // worker, task, level
      ("Joe",  "carpentry", 7),
      ("Joe",  "ceiling",   5),
      ("Joe",  "roofing",   6),
      ("Joe",  "windows",   8),
      ("Joe",  "facade",    5),
      ("Joe",  "garden",    5),
      ("Joe",  "moving",    6),
      ("Jack", "masonry",   5),
      ("Jack", "plumbing",  7),
      ("Jack", "ceiling",   8),
      ("Jack", "roofing",   7),
      ("Jack", "painting",  9),
      ("Jack", "facade",    5),
      ("Jack", "garden",    5),
      ("Jim",  "carpentry", 5),
      ("Jim",  "painting",  6),
      ("Jim",  "windows",   5),
      ("Jim",  "garden",    9),
      ("Jim",  "moving",    8)
  )

  val continuities = List(
    ("Joe",  "masonry",   "carpentry"),
    ("Jack", "roofing",   "facade"),
    ("Joe",  "carpentry", "roofing"),
    ("Jim",  "garden",    "moving")
  )

  //
  // Types
  //

  class Task(val name: String, val duration: Int) {
    override def toString: String = name
  }

  class Precedence(val before: Task, val after: Task) {
    override def toString: String = "endBeforeStart(" + before + "," + after + ")"
  }

  class Worker(val name: String) {
    override def toString: String = name
  }

  class Skill(val worker: Worker, val task: Task, val level: Int) {
    override def toString: String = "(" + worker + "," + task + "," + level + ")"
  }

  class Continuity(val worker: Worker, val task1: Task, val task2: Task) {
    override def toString: String = "(" + worker + "," + task1 + "," + task2 + ")"
  }

  var theTasks: Map[String, Task] = (for (t <- tasks; (name, duration) = t) yield (name -> new Task(name, duration)))(collection.breakOut)
  var thePrecedences : List[Precedence] = for (p <- precedences) yield new Precedence(findTask(p._1), findTask(p._2))
  var theWorkers : Map[String, Worker] = (for (w <- workers) yield (w -> new Worker(w)))(collection.breakOut)
  var theSkills: Map[(String, String), Skill] = (for (s <- skills; (worker, task, level) = s)
      yield (worker, task) -> new Skill(findWorker(worker), findTask(task), level))(collection.breakOut)
  var theContinuities: List[Continuity] = for (c <- continuities; (worker, task1, task2) = c)
    yield new Continuity(findWorker(worker), findTask(task1), findTask(task2))

  def findTask(name: String): Task = {
    theTasks(name)
  }

  def findWorker(name: String): Worker = {
    theWorkers(name)
  }

  def findSkill(worker: String, task: String): Skill = {
    theSkills(worker, task)
  }

  //
  // Variables
  //


  implicit var model: CpModel = _

  var taskVars: Map[(Int, Task), IntervalVar] = _
  var wtaskVars: Map[(Int, Skill), IntervalVar] = _


  def build(): CpModel = {

    model = CpModel("HouseBuilding")

    // variables

    taskVars = (for (house <- HOUSES; entry <- theTasks; (name, task) = entry)
      yield (house, task) -> model.intervalVar(startMin=0, endMax=HORIZON, sizeMin=task.duration, sizeMax=task.duration, name=s"house $house task $task"))(collection.breakOut)

    wtaskVars = (for (house <- HOUSES; e <- theSkills; ((worker, task), skill) = e)
      yield (house, skill) -> model.intervalVar(optional=true, name=s"house $house skill $skill"))(collection.breakOut)

    // objective

    val objective = sum(for (e <- theSkills; h <- HOUSES; skill = e._2)
      yield skill.level * presenceOf(wtaskVars(h, skill)))

    model.add(maximize(objective))

    // constraints

    for (h <- HOUSES) {
      // temporal constraint
      for (p <- thePrecedences) {
        model.add(endBeforeStart(taskVars(h, p.before), taskVars(h, p.after)))
      }
      // alternative workers
      for (e <- theTasks; (name, task) = e) {
        val wtasks = for (s <- theSkills; skill = s._2 if skill.task == task)
          yield wtaskVars(h, skill)
        model.add(alternative(taskVars(h, task), wtasks))
      }
      // continuity constraints
      for (c <- theContinuities)
        model.add(presenceOf(wtaskVars(h, findSkill(c.worker.name, c.task1.name)))
          == presenceOf(wtaskVars(h, findSkill(c.worker.name, c.task1.name))))
    }

    model.printInformation()

    // return the model
    model
  }

  def solve(): Boolean = {

    println(s"Solving model $model....")

//    model.exportModel("HouseBuilding.cpo")

    val status = model.solve()

    if (status) {
      println(s"Solution status: $status")
      println("Objective value: " + model.getObjectiveValue())
      println("Tasks: ")
      for (t <- taskVars) {
        val ((house, task), taskVar) = t
        println("\t" + "From " + model.getStart(taskVar) + " to " + model.getEnd(taskVar) + ", " + task.name + " in house " + house)
      }
    }

    status
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
