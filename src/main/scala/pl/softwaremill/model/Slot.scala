package pl.softwaremill.model

import java.text.SimpleDateFormat
import java.util.Date

import net.liftweb.http._
import net.liftweb.mapper._
import net.liftweb.common._
import net.liftweb.util.Helpers._
import net.liftweb.util.FieldError
import S._

/**
 * @author Adam Warski (adam at warski dot org)
 */
class Slot extends LongKeyedMapper[Slot] with IdPK {
  def getSingleton = Slot

  object start extends MappedTime(this)

  object end extends MappedTime(this)

  object conference extends LongMappedMapper[Slot, Conference](this, Conference)

  object room extends LongMappedMapper[Slot, Room](this, Room)

  def slotSpan = SlotSpan(start, end)

  def validateStartBeforeEnd: List[FieldError] = {
    if (start.is.compareTo(end.is) >= 0)
      List(FieldError(start, ?("slot.end_not_after_begin", slotSpan.startTime, slotSpan.endTime)))
    else Nil
  }
}

object Slot extends Slot with LongKeyedMetaMapper[Slot] {
  override def validation = List(slot => slot.validateStartBeforeEnd)
}

sealed case class SlotSpan(var start: Date, var end: Date) {
  def this() = this(new Date(0), new Date(0))

  private def slotTimeFormat = new SimpleDateFormat("HH:mm")

  private def toSlotTimeFormat(d: Date): String = if (d == null) toSlotTimeFormat(new Date(0)) else slotTimeFormat.format(d)
  private def parseSlotTimeFormat(s: String) = tryo(slotTimeFormat.parse(s))
  private def setSlotTimeFormat(s: String, field: Function1[Date, Unit]): Box[Unit] = {
    parseSlotTimeFormat(s) map { d => field(d) }
  }

  def startTime: String = toSlotTimeFormat(start)
  def endTime: String = toSlotTimeFormat(end)

  def setStartTime(s: String) = setSlotTimeFormat(s, start = _)
  def setEndTime(s: String) = setSlotTimeFormat(s, end = _)
} 