package pl.softwaremill.snippet

import xml._

import net.liftweb.util.Helpers._
import net.liftweb.http._
import js.JE._
import js.JsCmd
import js.JsCmds.{CmdPair, SetHtml}
import net.liftweb.common._
import SHtml._
import S._

import pl.softwaremill.model._
import pl.softwaremill.lib.D
import pl.softwaremill.services.{SlotService, ConferenceService, PaperService}

/**
 * @author Adam Warski (adam at warski dot org)
 */
class CreateSchedule {
  lazy val paperService = D.inject_![PaperService]
  lazy val conferenceService = D.inject_![ConferenceService]
  lazy val slotService = D.inject_![SlotService]

  def render(template: NodeSeq) = {
    val conf = CurrentConference.is
    val allInterests = paperService.interestsForConference(conf)

    def createScheduleTable(assigments: Map[Slot, Paper]) = {
      // TODO: factor out common code from this and SlotEditor
      slotService.generateTable(CurrentConference.is,
        room => Text(room.name.is),
        slotSpan => Text(?("slot.span.from_to", slotSpan.startTime, slotSpan.endTime)),
        () => EntityRef("nbsp"),
        slot => assigments(slot).title.is)
    }

    // TODO: secure 

    bind("schedule", template,
      "numberOfPreferences" -> ?("createschedule.number_of_preferences", allInterests.size),
      "generate" -> ajaxButton(?("createschedule.generate"), () => {
        val result = conferenceService.generateSchedule(conf)
        println(result)
        SetHtml("generated", createScheduleTable(result.assigments)) &
        SetHtml("generatedScore", Text(?("createschedule.generated", result.violated)))
      }))
  }
}