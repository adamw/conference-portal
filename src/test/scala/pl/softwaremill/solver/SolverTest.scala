package pl.softwaremill.solver

import domain._
import org.specs.Specification
import org.specs.runner.JUnit4

/**
 * @author Adam Warski (adam at warski dot org)
 */
object SolverTest extends Specification {
  "The solver" should {
    def solverExample(roomsCount: Int, timesCount: Int, preferencesDefs: Either[List[(Int, Int)], List[Int]], maxViolations: Int, preferencesDesc: String) = {
      val papersCount = roomsCount * timesCount

      "correctly assign %d papers to %d rooms and %d timeslots with %s".format(papersCount, roomsCount, timesCount, preferencesDesc) in {
        val papers = (1 to papersCount).map(i => { val p = new Paper; p.id = i; p}).toList
        val rooms = (1 to roomsCount).map(i => { val r = new Room; r.id = i; r}).toList
        val times = (1 to timesCount).map(i => { val t = new Time; t.id = i; t}).toList
        val slots = generateSlotsRectangle(rooms, times)
        val preferences = preferencesDefs.fold((_.map { case (pid1, pid2) => Preference(papers(pid1-1), papers(pid2-1), 1) }),
          Preference.generate(papers, _))

        val solver = new ConferenceSolver {
          override def createConfigurer = {
            val configurer = super.createConfigurer
            configurer.getConfig.setRandomSeed(1234l)
            configurer.getConfig.getTerminationConfig.setScoreAttained("0hard/" + (-maxViolations) + "soft")

            configurer
          }
        }

        val bestSchedule = solver.solve(papers, slots, preferences)
        
        checkScheduleCorrect(bestSchedule, papers, slots)
        bestSchedule.violatedPreferences must haveSize(maxViolations)
      }
    }

    solverExample(3, 3, Left(List()), 0, "no preferences")
    solverExample(2, 5, Left(List()), 0, "no preferences")
    solverExample(3, 3, Left(List((1, 3), (9, 8), (6, 7))), 0, "3 non-overlapping preferences")
    solverExample(2, 2, Right(List(1, 2, 3)), 1, "3 preferences, 1 must be violated")
    solverExample(3, 3, Right(List(1, 2, 3, 4)), 1, "6 preferences, 1 must be violated")
    solverExample(3, 4, Right(List(2, 4, 5, 12)), 0, "6 preferences, 0 must be violated")
    solverExample(3, 4, Right(List(3, 7, 8, 10, 12)), 1, "10 preferences, 1 must be violated")
    solverExample(3, 4, Right(List(1, 3, 7, 9, 10, 12)), 2, "15 preferences, 2 must be violated")
  }

  def checkScheduleCorrect(schedule: Schedule, papers: List[Paper], slots: List[Slot]) {
    val (acc1, acc2) = schedule.assigments.foldLeft((Set[Paper](), Set[Slot]()))(
      (acc, ass) => acc match { case (acc1, acc2) => (acc1 + ass.paper, acc2 + ass.slot) })

    acc1 must_== (Set() ++ papers)
    acc2 must_== (Set() ++ slots)
  }

  def generateSlotsRectangle(rooms: List[Room], times: List[Time]): List[Slot] = {
    (for (r <- rooms; t <- times) yield { val s = new Slot; s.id = r.id + (t.id-1)*rooms.size; s.room = r; s.time = t; s }).toList
  }
}

class SlotServiceJUnitTest extends JUnit4(SolverTest)