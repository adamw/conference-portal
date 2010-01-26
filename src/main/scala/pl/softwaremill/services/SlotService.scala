package pl.softwaremill.services

import xml.NodeSeq

import pl.softwaremill.model._

/**
 * @author Adam Warski (adam at warski dot org)
 */
trait SlotService {
  /**
   * Generates a slot for each room of the conference and for each slot span. The slots are added
   * to the given conference. 
   */
  def generateRectangle(conf: Conference, slotSpans: List[SlotSpan])

  /**
   * Generates a XHTML table of slots for the given conference.
   */
  def generateTable(conf: Conference, roomHeader: Room => NodeSeq, slotHeader: SlotSpan => NodeSeq,
                    slotEmpty: () => NodeSeq, slotFull: Slot => NodeSeq): NodeSeq
}

class SlotServiceImpl extends SlotService {
  def generateRectangle(conf: Conference, slotSpans: List[SlotSpan]) {
    def generate = {
      for (slotSpan <- slotSpans; room <- conf.rooms) yield {
        val slot = new Slot
        slot.room(room)
        slot.start(slotSpan.start)
        slot.end(slotSpan.end)
        slot
      }
    }

    conf.slots ++= generate
  }

  def generateTable(conf: Conference, roomHeader: Room => NodeSeq, slotHeader: SlotSpan => NodeSeq,
                    slotEmpty: () => NodeSeq, slotFull: Slot => NodeSeq) = {
    val slotsBySpans = conf.slotsBySpans
    val sortedSlotSpans = slotsBySpans.keys.toList.sort((span1, span2) => span1.start.compareTo(span2.start) < 0)
    val rooms = conf.rooms

    def headerRow = {
      def headerCells = rooms.flatMap((room: Room) => <th>{ roomHeader(room) }</th>)

      <tr>
        <th>&nbsp;</th>
        { headerCells }
      </tr>
    }

    def contentRow(slotSpan: SlotSpan) = {
      val slots = slotsBySpans(slotSpan)

      // A map from rooms to slots. There can be at most on slot per room as we assume that the slots are
      // valid.
      val roomsToSlots = (Map[RoomDesc, Slot]() /: slots)((acc, slot: Slot) => acc + (slot.room.obj.open_!.desc -> slot))

      def contentCells = rooms.flatMap((room: Room) => {
        val roomDesc = room.desc
        <td>{ if (!roomsToSlots.contains(roomDesc)) slotEmpty() else slotFull(roomsToSlots(roomDesc)) }</td>
      })

      <tr>
        <td>{ slotHeader(slotSpan) }</td>
        { contentCells }
      </tr>
    }

    def contentRows = sortedSlotSpans.flatMap(contentRow _)

    <table>
      { headerRow }
      { contentRows }
    </table>
  }
}