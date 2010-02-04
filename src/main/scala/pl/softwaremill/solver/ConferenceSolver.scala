package pl.softwaremill.solver

import org.drools.solver.config.XmlSolverConfigurer
import org.drools.solver.core.Solver

import pl.softwaremill.solver.domain._
import org.apache.log4j.{Level, PatternLayout, ConsoleAppender, Logger}

/**
 * @author Adam Warski (adam at warski dot org)
 */
class ConferenceSolver {
  val configPath = "/solver/config.xml"

  def randomSeed = System.currentTimeMillis

  def createSolver(): Solver = {
    val configurer = new XmlSolverConfigurer()
    configurer.configure(configPath);
    configurer.getConfig.setRandomSeed(randomSeed)
    configurer.buildSolver();
  }

  def createInitialAssigments(papers: List[Paper], slots: List[Slot]): List[Assigment] = {
    papers.map(p => { val a = new Assigment; a.slot = slots.first; a.paper = p; a }).toList
  }

  def solve(papers: List[Paper], slots: List[Slot], preferences: List[Preference]): Schedule = {
    val startingSchedule = new Schedule(papers, slots, createInitialAssigments(papers, slots), preferences)

    val solver = createSolver
    solver.setStartingSolution(startingSchedule)
    solver.solve

    solver.getBestSolution.asInstanceOf[Schedule]
  }
}

object Test {
  def main(args: Array[String]) {
    val solver = new ConferenceSolver

//    val roomsCount = 2
//    val timesCount = 2
//    val preferencesDefs = List((1, 2), (1, 3), (2, 3))

    val roomsCount = 3
    val timesCount = 3
    val preferencesDefs = List((1, 2), (1, 3), (1, 4), (2, 3), (2, 4), (3, 4))

    val papers = (1 to (roomsCount * timesCount)).map(i => { val p = new Paper; p.id = i; p}).toList
    val rooms = (1 to roomsCount).map(i => { val r = new Room; r.id = i; r}).toList
    val times = (1 to timesCount).map(i => { val t = new Time; t.id = i; t}).toList
    val slots = (for (r <- rooms; t <- times) yield { val s = new Slot; s.id = r.id + (t.id-1)*roomsCount; s.room = r; s.time = t; s }).toList
    val preferences = preferencesDefs.map { case (pid1, pid2) => {
      val p = new Preference
      p.paper1 = papers(pid1-1)
      p.paper2 = papers(pid2-1)
      p
    } }

    println("Start")

    val root = Logger.getRootLogger();
    if (!root.getAllAppenders().hasMoreElements()) {
      root.setLevel(Level.INFO)
      root.addAppender(new ConsoleAppender(new PatternLayout(PatternLayout.TTCC_CONVERSION_PATTERN)))
    }

    val start = System.currentTimeMillis
    val bestSolution = solver.solve(papers, slots, preferences)
    val end = System.currentTimeMillis

    println("Time: " + ((end-start)/1000))
    println("Best score: " + bestSolution.getScore)
    println("Assigments: " + bestSolution.assigments)

    checkScheduleCorrect(bestSolution, papers, slots)

    println("Violated: " + bestSolution.violatedPreferences)

    println("Stop")
  }

  def checkScheduleCorrect(schedule: Schedule, papers: List[Paper], slots: List[Slot]) {
    val (acc1, acc2) = schedule.assigments.foldLeft((Set[Paper](), Set[Slot]()))(
      (acc, ass) => acc match { case (acc1, acc2) => (acc1 + ass.paper, acc2 + ass.slot) })

    assert(acc1 == (Set() ++ papers))
    assert(acc2 == (Set() ++ slots))
  }
}