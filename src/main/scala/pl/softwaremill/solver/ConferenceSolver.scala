package pl.softwaremill.solver

import org.drools.solver.config.XmlSolverConfigurer
import org.drools.solver.core.Solver

import pl.softwaremill.solver.domain._

/**
 * @author Adam Warski (adam at warski dot org)
 */
class ConferenceSolver {
  def createSolver(): Solver = {
    val configurer = new XmlSolverConfigurer();
    configurer.configure("/solver/config.xml");
    configurer.buildSolver();
  }

}

object Test {
  def main(args: Array[String]) {
    val solver = (new ConferenceSolver()).createSolver

    val papers = (1 to 9).map(i => { val p = new Paper; p.id = i; p}).toList
    val rooms = (1 to 3).map(i => { val r = new Room; r.id = i; r}).toList
    val times = (1 to 3).map(i => { val t = new Time; t.id = i; t}).toList
    val slots = (for (r <- rooms; t <- times) yield { val s = new Slot; s.id = 3*(r.id-1) + t.id; s.room = r; s.time = t; s }).toList
    val assigments = papers.map(p => { val a = new Assigment; a.id = p.id; a.slot = slots.first; a.paper = p; a}).toList

    val startingSchedule = new Schedule(papers, rooms, times, slots, assigments, List())

    solver.setStartingSolution(startingSchedule)

    println("Start")

    solver.solve

    val bestSolution = solver.getBestSolution.asInstanceOf[Schedule]
    println("Best score: " + bestSolution.getScore)
    println("Assigments: " + bestSolution.assigments)

    checkScheduleCorrect(bestSolution, papers, slots)

    println("Stop")
  }

  def checkScheduleCorrect(schedule: Schedule, papers: List[Paper], slots: List[Slot]) {
    val (acc1, acc2) = schedule.assigments.foldLeft((Set[Paper](), Set[Slot]()))(
      (acc, ass) => acc match { case (acc1, acc2) => (acc1 + ass.paper, acc2 + ass.slot) })

    assert(acc1 == (Set() ++ papers))
    assert(acc2 == (Set() ++ slots))
  }
}