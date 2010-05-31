package pl.softwaremill.snippet

import scala.collection.mutable

import xml._

import net.liftweb.util.Helpers._
import net.liftweb.http._
import net.liftweb.common._
import net.liftweb.http.js.JsCmds._
import SHtml._
import S._

import pl.softwaremill.lib.D
import pl.softwaremill.model._

import pl.softwaremill.services.SlotService

import SnippetTools._

/**
 * @author Adam Warski (adam at warski dot org)
 */
class SlotEditor {
  lazy val slotService = D.inject[SlotService].open_!

  val slotSpans = new mutable.ListBuffer[SlotSpan];

  def addSlotSpan = slotSpans += new SlotSpan
  def deleteSlotSpan(idx: Int) = slotSpans.remove(idx)

  def noticeIfFailure[T](b: Box[T]) = b match {
    case Failure(msg, _, _) => S.error(msg)
    case Empty => S.error("Unknown")
    case _ =>
  }

  def checkAndSave {
    val conf = CurrentConference.is
    conf.validate match {
      case Nil  => conf.save(); S.notice(S.?("common.saved", conf.name))
      case xs   => xs.map { i => S.error(i.msg); }
    }
  }

  def reDrawGrid = SetHtml("slot_grid", slotGrid(NodeSeq.Empty))

  def slotsTimes(slotTimesTemplate: NodeSeq): NodeSeq = {
    def reDrawForm = SetHtml("slot_times", slotsTimes(slotTimesTemplate))
    def reDraw = CmdPair(reDrawForm, reDrawGrid)

    def slotTime(slotSpanTimeTemplate: NodeSeq): NodeSeq = {
      slotSpans.toList.zipWithIndex.flatMap { case (slotSpan: SlotSpan, idx: Int) =>
        bind("time", slotSpanTimeTemplate,
          "from" -> text(slotSpan.startTime, (s: String) => noticeIfFailure(slotSpan.setStartTime(s))),
          "to" -> text(slotSpan.endTime, (s: String) => noticeIfFailure(slotSpan.setEndTime(s))),
          "delete" -> ajaxSubmit(?("common.delete"), () => { deleteSlotSpan(idx); reDrawForm })
          )
      }
    }

    ajaxForm(
      bind("times", slotTimesTemplate,
        "list" -> slotTime _,
        "addSpan" -> ajaxSubmit(?("slots.add_span"), () => { addSlotSpan; reDrawForm }),
        "generate" -> ajaxSubmit(?("slots.generate"), () => {
          slotService.generateRectangle(CurrentConference, slotSpans.toList);
          reDraw
        }),
        "delete" -> ajaxSubmit(?("slots.delete"), () => { CurrentConference.slots.clear; reDraw }),
        "save" -> ajaxSubmit(?("slots.save"), () => { checkAndSave; reDraw }),
        "cancel" -> redirectButton(?("common.cancel"), "index.html")
        ))
  }

  def slotGrid(ignore: NodeSeq): NodeSeq = {
    slotService.generateTable(CurrentConference.is,
      room => Text(room.name.is),
      slotSpan => Text(?("slot.span.from_to", slotSpan.startTime, slotSpan.endTime)),
      () => EntityRef("nbsp"),
      slot => Text("# ") ++ a(() => { CurrentConference.slots -= slot; checkAndSave; reDrawGrid }, Text("X")))
  }
}