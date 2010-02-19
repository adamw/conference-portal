package pl.softwaremill.services

import net.liftweb.mapper._
import net.liftweb.common.Box

import pl.softwaremill.model._
import pl.softwaremill.lib.D
import pl.softwaremill.solver.ConferenceSolver
import pl.softwaremill.solver.domain.Preference

/**
 * @author Adam Warski (adam at warski dot org)
 */
trait ConferenceService {
  def allConferences: List[Conference]
  def conferencesInState(state: ConferenceState.Value): List[Conference]
  def find(id: String): Box[Conference]
  def generateSchedule(conf: Conference): GenerateScheduleResult
}

class ConferenceServiceImpl extends ConferenceService {
  def allConferences: List[Conference] = {
    Conference.findAll(OrderBy(Conference.name, Ascending))
  }

  def conferencesInState(state: ConferenceState.Value): List[Conference] = {
    Conference.findAll(By(Conference.mappedState, state.id), OrderBy(Conference.name, Ascending))
  }

  def find(id: String): Box[Conference] = {
    Conference.find(id)
  }

  def modelToSolverModel[T, U](modelList: List[T], createSolverModel: T=>U): (List[U], Map[T, U], Map[U, T]) = {
    modelList.foldLeft(Nil: List[U], Map[T, U](), Map[U, T]())((acc, modelEl) => {
      val solverModelEl = createSolverModel(modelEl)
      (solverModelEl :: acc._1, acc._2 + (modelEl -> solverModelEl), acc._3 + (solverModelEl -> modelEl))
    })
  }

  def generateSchedule(conf: Conference): GenerateScheduleResult = {
    val paperService = D.inject_![PaperService]

    val papers = paperService.acceptedConferencePapers(conf)
    val rooms = conf.rooms
    val slots = conf.slots.toList
    val slotSpans = conf.slotsBySpans.keys.toList

    // Converting the model
    val (solverPapers, papersToSolverPapers, solverPapersToPapers) = modelToSolverModel(papers, (paper: Paper) => {
      val p = new pl.softwaremill.solver.domain.Paper; p.id = paper.id.toInt; p
    })

    val (solverRooms, roomsToSolverRooms, solverRoomsToRooms) = modelToSolverModel(rooms, (room: Room) => {
      val r = new pl.softwaremill.solver.domain.Room; r.id = room.id.toInt; r
    })

    var timeId = 0;
    val (solverTimes, slotSpansToSolverTimes, solverTimesToSlotSpans) = modelToSolverModel(slotSpans, (span: SlotSpan) => {
      val t = new pl.softwaremill.solver.domain.Time; t.id = timeId; timeId += 1; t
    })

    val (solverSlots, slotsToSolverSlots, solverSlotsToSlots) = modelToSolverModel(slots, (slot: Slot) => {
      val s = new pl.softwaremill.solver.domain.Slot; s.id = slot.id.toInt;
      s.room = roomsToSolverRooms(slot.room.obj openOr null);
      s.time = slotSpansToSolverTimes(slot.slotSpan);
      s
    })

    // Generating the preferences
    val interestingPapersByUser = paperService.interestingPapersByUser(conf)
    val solverPreferences = interestingPapersByUser.flatMap {
      case (user, papersList) => Preference.generate(solverPapers, papersList.map(papersToSolverPapers(_).id.toInt))
    }.toList

    // Running the solver
    val solver = new ConferenceSolver
    val bestSolution = solver.solve(solverPapers, solverSlots, solverPreferences)

    // Converting the solution
    val assigments = bestSolution.assigments.foldLeft(Map[Slot, Paper]())((acc, assigment) => {
      acc + (solverSlotsToSlots(assigment.slot) -> solverPapersToPapers(assigment.paper))
    })

    GenerateScheduleResult(assigments, bestSolution.violatedPreferencesCount)
  }
}

case class GenerateScheduleResult(assigments: Map[Slot, Paper], violated: Int)