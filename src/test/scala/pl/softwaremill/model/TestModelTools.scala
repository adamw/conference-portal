package pl.softwaremill.model

/**
 * @author Adam Warski (adam at warski dot org)
 */
object TestModelTools {
  def addSlot(conf: Conference, timeStart: String, timeEnd: String, roomName: String): Slot = {
    val slotSpan = new SlotSpan
    slotSpan.setStartTime(timeStart)
    slotSpan.setEndTime(timeEnd)

    val room = conf.rooms.find(_.name.is == roomName).get

    val slot: Slot = new Slot
    slot.start(slotSpan.start)
    slot.end(slotSpan.end)
    slot.room(room)

    conf.slots += slot

    slot
  }
}