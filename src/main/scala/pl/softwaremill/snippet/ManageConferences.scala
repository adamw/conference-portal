package pl.softwaremill.snippet

import net.liftweb.util.Helpers._
import net.liftweb.http._
import net.liftweb.common._
import SHtml._
import S._
import Box._

import net.liftweb.http.js.JsCmds._

import xml._

import pl.softwaremill.services.ConferenceService
import pl.softwaremill.lib.D
import SnippetTools._
import pl.softwaremill.model.{Configuration, ConferenceState, Room, Conference}
import pl.softwaremill.loc._

/**
 * @author Adam Warski (adam at warski dot org)
 */
class ManageConferences  {
  lazy val conferenceService = D.inject_![ConferenceService]

  var listTemplate: NodeSeq = _
  var conferenceTemplate: NodeSeq = _
  var activeTemplate: NodeSeq = _

  def reDrawForm = SetHtml("conf_edit", edit(conferenceTemplate))
  def reDrawList = SetHtml("conf_list", list(listTemplate))
  def reDrawActive = SetHtml("active_conf", active(activeTemplate))
  def reDraw = CmdPair(CmdPair(reDrawForm, reDrawList), reDrawActive)

  def edit(conferenceTemplate: NodeSeq): NodeSeq = {
    val conf = CurrentConference.is

    this.conferenceTemplate = conferenceTemplate;

    def checkAndSave {
      conf.validate match {
        case Nil  => conf.save(); S.notice(S.?("common.saved", conf.name)); CurrentConference(Conference.create);
        case xs   => S.error(xs)
      }
    }

    def editRoom(roomTemplate: NodeSeq): NodeSeq = {
      conf.rooms.flatMap { room: Room =>
          bind("room", roomTemplate,
            "name" -> room.name.toForm,
            "moveUp" -> ajaxSubmit(?("common.move_up"), () => { conf._rooms.moveUp(room); reDrawForm }),
            "moveDown" -> ajaxSubmit(?("common.move_down"), () => { conf._rooms.moveDown(room); reDrawForm }),
            "delete" -> ajaxSubmit(?("common.delete"), () => { conf._rooms.deleteObj(room); reDrawForm })
            )
      }
    }

    def stateForm = {
      val options = ConferenceState.map { state => (state, ?(state.toString)) }
      selectObj[ConferenceState.Value](options.toList, Full(conf.state), conf.state(_))
    }

    ajaxForm(
      bind("conf", conferenceTemplate,
        "name" -> conf.name.toForm,
        "dateStart" -> conf.dateStart.toForm,
        "dateEnd" -> conf.dateEnd.toForm,
        "desc" -> conf.desc.toForm,
        "state" -> stateForm,
        "rooms" -> editRoom _,
        "addRoom" -> ajaxSubmit(?("conference.rooms.add"), () => { conf._rooms.addObj; reDrawForm }),
        "save" -> ajaxSubmit(if (conf.saved_?) ?("common.save") else ?("common.add"), () => { checkAndSave; reDraw }),
        "cancel" -> refreshButton(?("common.cancel"))
        ))
  }

  def list(listTemplate: NodeSeq) = {
    def options(conf: Conference)(separator: NodeSeq): NodeSeq = {
      def showIf(show: Boolean, loc: ConferenceAwareLoc, textKey: String): NodeSeq = {
        if (show)
          separator ++ anchor(loc.link.createPath(conf), ?(textKey))
        else NodeSeq.Empty
      }

      showIf(true, SlotEditorLoc, "conference.edit_slots") ++
      showIf(true, StatisticsLoc, "conference.statistics") ++
      showIf(true, CmsAdminLoc, "conference.cms_admin") ++
      showIf(conf.state == ConferenceState.Schedule || conf.state == ConferenceState.Finalize, CreateScheduleLoc, "conference.create_schedule") ++
      showIf(true, AcceptRejectLoc, "conference.accept_reject")
    }

    def doList(itemTemplate: NodeSeq): NodeSeq = {
      conferenceService.allConferences.flatMap { conf: Conference =>
        bind("conf", itemTemplate,
          "name" -> conf.name,
          "dateStart" -> conf.dateStart,
          "dateEnd" -> conf.dateEnd,
          "edit" -> a(() => { CurrentConference(conf); reDrawForm }, Text(?("common.edit"))),
          "delete" -> confirmLink("", () => { /*TODO: enable conf.delete_!*/ }, ?("common.delete"), ?("conference.confirm_delete", conf.name)),
          "options" -> options(conf) _
          )
      }
    }

    this.listTemplate = listTemplate
    bind("conf", listTemplate,
      "item" -> doList _
      )
  }

  def active(activeTemplate: NodeSeq): NodeSeq = {
    val configuration = Configuration.is

    def confSelect = {
      val conferences = conferenceService.allConferences
      val options = (Empty, ?("conference.active.none")) :: conferences.map { conf => (Full(conf), conf.name.is) }
      Full(selectObj[Box[Conference]](options, Full(configuration.activeConference), configuration.activeConference(_)))
    }

    this.activeTemplate = activeTemplate
    ajaxForm(
      bind("active", activeTemplate,
        "confSelect" -> confSelect,
        "save" -> ajaxSubmit(?("common.save"), () => { configuration.save; reDraw })
        ))
  }
}