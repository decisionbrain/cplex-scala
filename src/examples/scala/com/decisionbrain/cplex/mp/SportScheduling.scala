/*
 * Source file provided under Apache License, Version 2.0, January 2004,
 *  http://www.apache.org/licenses/
 *  (c) Copyright DecisionBrain SAS 2016,2020
 *
 */

package com.decisionbrain.cplex.mp

import com.decisionbrain.cplex.mp.MpModel._
import com.decisionbrain.cplex.Modeler._
import com.decisionbrain.cplex.NumExpr
import com.decisionbrain.cplex.NumVar

object SportScheduling {

  //
  // Data
  //

  val nbs = (8, 1, 1, 16)

  val team_div1 = List("Baltimore Ravens", "Cincinnati Bengals", "Cleveland Browns",
    "Pittsburgh Steelers", "Houston Texans", "Indianapolis Colts",
    "Jacksonville Jaguars", "Tennessee Titans", "Buffalo Bills",
    "Miami Dolphins", "New England Patriots", "New York Jets",
    "Denver Broncos", "Kansas City Chiefs", "Oakland Raiders",
    "San Diego Chargers")

  val team_div2 = List("Chicago Bears", "Detroit Lions", "Green Bay Packers",
    "Minnesota Vikings", "Atlanta Falcons", "Carolina Panthers",
    "New Orleans Saints", "Tampa Bay Buccaneers", "Dallas Cowboys",
    "New York Giants", "Philadelphia Eagles", "Washington Redskins",
    "Arizona Cardinals", "San Francisco 49ers", "Seattle Seahawks",
    "St. Louis Rams")

  //
  // Types & Data members
  //

  type Match = (Int, Int, Int)

  var teams: List[String] = _
  var weeks: Range = _
  var matches: List[Match] = _

  var playVars:  Map[(Match, Int), NumVar] = _

  /**
    *  Create the optimization model for the sport scheduling problem.
    *
    * @return
    */
  def buildSportSchedulingModel() = {

    println("* Building Sport Scheduling model instance")

    implicit val model = new MpModel("sportscheduling")

    val (nbTeamsInDivision, nbIntraDivisional, nbInterDivisional, nbTeams) = nbs

    teams = (team_div1 ++ team_div2)

    assert(team_div1.size == team_div2.size)

    // team index ranges from 1 to 2N
    val teamRange = Range(1, 2 * nbTeamsInDivision + 1)

    // Calculate the number of weeks necessary.
    val nbWeeks = (nbTeamsInDivision - 1) * nbIntraDivisional + nbTeamsInDivision * nbInterDivisional
    weeks = Range(1, nbWeeks + 1)

    println("%d games, %d intradivisional, %d interdivisional".
      format(nbWeeks, (nbTeamsInDivision - 1) * nbIntraDivisional,
        nbTeamsInDivision * nbInterDivisional))

    // Season is split into two halves.
    val firstHalfWeeks = Range(1, nbWeeks / 2 + 1)
    val nbFirstHalfGames = nbWeeks / 3

    println("%s first half weeks, %d first half games".
      format(firstHalfWeeks, nbFirstHalfGames))

    // All possible matches (pairings) and whether of not each is intradivisional.
    def sameDivision(a: Int, b: Int) = if (b <= nbTeamsInDivision || a > nbTeamsInDivision) 1 else 0
    matches = (for (t1 <- teamRange; t2 <- teamRange if t1 < t2) yield (t1, t2, sameDivision(t1, t2))).toList

    // Number of games to play between pairs depends on
    // whether the pairing is intradivisional or not.
    def nbGames(m: Match) = if (m._3 == 1) nbIntraDivisional else nbInterDivisional
    val nbPlay : Map[Match, Int] = (for (m <- matches) yield m -> nbGames(m)).toMap

    def playName(m: Match, w: Int) = {
      val (team1, team2, _) = m
      "play_%d_%d_w%d".format(team1, team2, w)
    }
    playVars = model.boolVars(matches, weeks.toArray, namer=playName(_,_))

    for (m <- matches) {
      val (team1, team2, _) = m
      model.add(sum(for (w <- weeks) yield playVars(m, w)) == nbPlay(m),
        name="correct_nb_games_%d_%d".format(team1, team2))
    }

    for (w <- weeks) {
      // Each team must play exactly once in a week
      for (t <- teamRange) {
        model.add(sum(for (m <- matches; (team1, team2, _) = m; if team1 == t || team2 == t) yield playVars(m, w)) == 1,
          name="plays_exactly_once_%d_%s".format(w, t))
      }
      // Games between the same teams cannot be on successive weeks.
      for (m <- matches) {
        if (w < nbWeeks) {
          val (team1, team2, _) = m
          model.add(playVars(m, w) + playVars(m, w+1) <= 1,
            "no_successive_week_%d_%d_%d".format(team1, team2, w))
        }
      }
    }

    // Some intradivisional games should be in the first half.
    for (t <- teamRange) {
      val maxTeamInDicision : IndexedSeq[NumExpr] =
        for (w <- firstHalfWeeks; m <- matches if m._3 == 1 && (m._1 == t ||m._2 == t)) yield playVars(m, w)
      model.add(sum(maxTeamInDicision) >= nbFirstHalfGames,
        name="in_division_first_half_%s".format(t))
    }

    // postpone divisional matches as much as possible
    // we weight each play variable with the square of w.
    val objective = maximize(sum(for (w <- weeks; m <- matches if m._3 == 1) yield playVars(m, w)*w*w))
    model.add(objective)

    model
  }

  def solveSportSchedulingModel(): AnyVal = {

    val model = buildSportSchedulingModel()

//    model.exportModel("sportscheduling.lp")

    val result = model.solve()
    if (!result) {
      println("**** No solution found!")
      return -1
    }

    val objValue = model.getObjValue()
    println(s"Found solution with cost $objValue")

    val solution = for (w <- weeks; m <- matches; (team1, team2, isDivisional) = m if model.getValue(playVars(m, w)) >= (1.0 - 1e-6)) yield (w, isDivisional, teams(team1), teams(team2))

    var currweek = 0
    println("Intradivisional games are marked with a *")
    for (s <- solution) {
      // assume records are sorted by increasing week indices
      val (week, isDivisional, team1, team2) = s
      if (week != currweek) {
        currweek = week
        println(" == == == == == == == == == == == == == == == == ")
        println("On week %d".format(currweek))
      }
      if (isDivisional == 1) {
        print("\t* ")
      }
      else {
        print("\t  ")
      }
      println("%s will meet the %s".format(team1, team2))
    }

    model.end()

    objValue
  }

  def main(args: Array[String]): Unit = {
    val obj = solveSportSchedulingModel()
  }
}
