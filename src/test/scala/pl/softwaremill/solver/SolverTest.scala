package pl.softwaremill.solver

import domain._
import org.specs.Specification
import org.specs.runner.JUnit4
import org.drools.solver.core.localsearch.DefaultLocalSearchSolver

/**
 * @author Adam Warski (adam at warski dot org)
 */
object SolverTest extends Specification {
  "The solver" should {
    val solver = new ConferenceSolver {
      override def randomSeed = 1234L
    }

    def solverExample(roomsCount: Int, timesCount: Int, preferencesDefs: List[(Int, Int)], maxViolations: Int, preferencesDesc: String) = {
      val papersCount = roomsCount * timesCount

      "correctly assign %d papers to %d rooms and %d timeslots with %s".format(papersCount, roomsCount, timesCount, preferencesDesc) in {
        val papers = (1 to papersCount).map(i => { val p = new Paper; p.id = i; p}).toList
        val rooms = (1 to roomsCount).map(i => { val r = new Room; r.id = i; r}).toList
        val times = (1 to timesCount).map(i => { val t = new Time; t.id = i; t}).toList
        val slots = generateSlotsRectangle(rooms, times)
        val preferences = preferencesDefs.map { case (pid1, pid2) => {
          val p = new Preference
          p.paper1 = papers(pid1-1)
          p.paper2 = papers(pid2-1)
          p
        } }

        val bestSchedule = solver.solve(papers, slots, preferences)
        
        checkScheduleCorrect(bestSchedule, papers, slots)
        bestSchedule.violatedPreferences must haveSize(maxViolations)
      }
    }

    solverExample(3, 3, List(), 0, "no preferences")
    solverExample(2, 5, List(), 0, "no preferences")
    solverExample(3, 3, List((1, 3), (9, 8), (6, 7)), 0, "3 non-overlapping preferences")
    solverExample(2, 2, List((1, 2), (1, 3), (2, 3)), 1, "3 preferences, 1 must be violated")
    solverExample(3, 3, List((1, 2), (1, 3), (1, 4), (2, 3), (2, 4), (3, 4)), 1, "6 preferences, 1 must be violated")
  }

  def checkScheduleCorrect(schedule: Schedule, papers: List[Paper], slots: List[Slot]) {
    val (acc1, acc2) = schedule.assigments.foldLeft((Set[Paper](), Set[Slot]()))(
      (acc, ass) => acc match { case (acc1, acc2) => (acc1 + ass.paper, acc2 + ass.slot) })

    acc1 must_== (Set() ++ papers)
    acc2 must_== (Set() ++ slots)
  }

  def generateSlotsRectangle(rooms: List[Room], times: List[Time]): List[Slot] = {
    (for (r <- rooms; t <- times) yield { val s = new Slot; s.id = 3*(r.id-1) + t.id; s.room = r; s.time = t; s }).toList
  }
}

class SlotServiceJUnitTest extends JUnit4(SolverTest)