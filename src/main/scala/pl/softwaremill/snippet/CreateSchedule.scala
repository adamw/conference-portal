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
    val allInterestsSize = paperService.interestsForConference(conf).size

    def createScheduleTable(assigments: Map[Slot, Paper]) = {
      // TODO: factor out common code from this and SlotEditor
      slotService.generateTable(CurrentConference.is,
        room => Text(room.name.is),
        slotSpan => Text(?("slot.span.from_to", slotSpan.startTime, slotSpan.endTime)),
        () => EntityRef("nbsp"),
        slot => Text(assigments(slot).title.is))
    }

    def generateButton = if (conf.slots.size == paperService.acceptedConferencePapers(conf).size) {
      ajaxButton(?("createschedule.generate"), () => {
        val result = conferenceService.generateSchedule(conf)
        SetHtml("generated", createScheduleTable(result.assigments)) &
                SetHtml("generatedScore", Text(?("createschedule.generated", result.violated)))
      })
    } else Text(?("createschedule.cannot_generate"))

    bind("schedule", template,
      "numberOfPreferences" -> ?("createschedule.number_of_preferences", allInterestsSize),
      "generate" -> generateButton)
  }

  def mostPopular(template: NodeSeq): NodeSeq = {
    (for ((title, votes) <- paperService.papersPopularity(CurrentConference.is)) yield
      bind("paper", template, "title" -> title, "votes" -> votes)).flatMap((x: NodeSeq) => x)
  }
}