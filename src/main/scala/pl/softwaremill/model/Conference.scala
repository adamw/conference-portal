package pl.softwaremill.model

import xml._

import net.liftweb.http._
import net.liftweb.common._
import net.liftweb.mapper._
import net.liftweb.util.FieldError
import S._

import pl.softwaremill.lib.CollectionTools

import java.util.Date

/**
 * @author Adam Warski (adam at warski dot org)
 */
class Conference extends LongKeyedMapper[Conference] with IdPK with OneToMany[Long, Conference] {
  def getSingleton = Conference

  /* Properties */

  object name extends MappedPoliteString(this, 32) {
    override def validations = valMinLen(3, "Name must be at least 3 characters") _ :: super.validations
  }

  object dateStart extends MappedDate(this) {
    override def defaultValue = new Date()
  }
  
  object dateEnd extends MappedDate(this) {
    override def defaultValue = new Date()
  }

  object desc extends MappedPoliteString(this, 128) {
    override def validations = valMinLen(3, "Description must be at least 3 characters") _ :: super.validations
  }

  object _rooms extends MappedOneToMany(Room, Room.conference, OrderBy(Room.position, Ascending))
    with Owned[Room] with Cascade[Room] with PositionManager[Conference, Room] {
    def createNew = new Room
  }

  def rooms = _rooms.sorted

  object slots extends MappedOneToMany(Slot, Slot.conference)
    with Owned[Slot] with Cascade[Slot]

  object mappedState extends MappedInt(this) {
    override def defaultValue = ConferenceState.Prepare.id
    override def dbColumnName = "state"
  }

  object mainMenuItem extends LongMappedMapper[Conference, MenuItem](this, MenuItem)

  /* Managing slots */

  def slotsBySpans: Map[SlotSpan, List[Slot]] = {
    CollectionTools.aggregate[SlotSpan, Slot](slots.toList, _.slotSpan)
  }

  /* Validation */

  def validateSlots[T](slotsOverlap: (SlotSpan, SlotSpan, RoomDesc) => T): List[T] = {
    // Checking that for each room, spans do not overlap
    def validateSlotsDontOverlapInRooms(toCheck: List[(RoomDesc, List[Slot])], acc: List[T]): List[T] = toCheck match {
      case Nil => acc
      case (roomDesc, slots) :: toCheckTail => {
        /**
         * Checks if in the given sorted list of spans, there are two spans overlapping.
         */
        def checkSpansOverlapping(spansList: List[SlotSpan]): Box[T] = spansList match {
          case Nil => Empty
          case _ :: Nil => Empty
          case e1 :: e2 :: tail =>
            if (e1.end.compareTo(e2.start) > 0) Full(slotsOverlap(e1, e2, roomDesc))
            else checkSpansOverlapping(e2 :: tail)
        }

        // Generating and sorting the list of all slot spans by the start date
        val spans = slots.map(_.slotSpan).sort((span1, span2) => span1.start.compareTo(span2.start) < 0)
        checkSpansOverlapping(spans) match {
          case Full(error) => validateSlotsDontOverlapInRooms(toCheckTail, error :: acc)
          case _ => validateSlotsDontOverlapInRooms(toCheckTail, acc)
        }
      }
    }

    val slotsByRooms = CollectionTools.aggregate[RoomDesc, Slot](slots.toList, _.room.obj.open_!.desc)
    validateSlotsDontOverlapInRooms(slotsByRooms.toList, Nil)
  }

  def validateRooms = {
    def checkPositions(rooms: List[Room], currentPos: Int): List[FieldError] = rooms match {
      case Nil => Nil
      case room :: roomsTail =>
        if (room.position != currentPos)
          List(FieldError(name, ?("conference.rooms.invalid_position", room.name.is, room.position, currentPos)))
        else checkPositions(roomsTail, currentPos + 1)
    }

    checkPositions(rooms, 0)
  }

  def state = ConferenceState(mappedState.is)

  def state(newState: ConferenceState.Value) = mappedState(newState.id)

  def conferenceAfterAcceptReject = {
    val _state = state
    (_state == ConferenceState.Schedule || _state == ConferenceState.Finalize)
  }
}

object Conference extends Conference with LongKeyedMetaMapper[Conference] {
  override def validation = {
    def slotsOverlap(e1: SlotSpan, e2: SlotSpan, roomDesc: RoomDesc) =
      FieldError(name, Text(?("conference.slots.overlap", e1.startTime, e2.endTime,
        e2.startTime, e2.endTime, roomDesc.name)))
    
    List(conf => (conf.slots.flatMap(slot => slot.validate) ++
            conf.validateSlots(slotsOverlap _) ++
            conf.validateRooms).toList)
  }
}

object ConferenceState extends Enumeration {
  val Prepare = Value("conference_state.prepare")
  val C4P = Value("conference_state.c4p")
  val Accept = Value("conference_state.accept")
  val Schedule = Value("conference_state.schedule")
  val Finalize = Value("conference_state.finalize")
}