package pl.softwaremill.snippet

import net.liftweb.util.Helpers._
import net.liftweb.http._
import pl.softwaremill.model.{ConferenceState, Room, Conference}
import net.liftweb.common._
import SHtml._
import S._
import Box._

import net.liftweb.http.js.JsCmds._

import xml._

import pl.softwaremill.services.ConferenceService
import pl.softwaremill.lib.D
import pl.softwaremill.loc.SlotEditorLoc

import SnippetTools._

/**
 * @author Adam Warski (adam at warski dot org)
 */
class ManageConferences  {
  lazy val conferenceService = D.inject[ConferenceService].open_!

  var listTemplate: NodeSeq = _
  var conferenceTemplate: NodeSeq = _

  def reDrawForm = SetHtml("conf_edit", edit(conferenceTemplate))
  def reDrawList = SetHtml("conf_list", list(listTemplate))
  def reDraw = CmdPair(reDrawForm, reDrawList)

  def edit(conferenceTemplate: NodeSeq): NodeSeq = {
    val conf = CurrentConference.is

    this.conferenceTemplate = conferenceTemplate;

    def checkAndSave {
      conf.validate match {
        case Nil  => conf.save(); S.notice(S.?("conference.saved", conf.name)); CurrentConference(Conference.create);
        case xs   => xs.map { i => S.error(i.msg); }
      }
    }

    def editRoom(roomTemplate: NodeSeq): NodeSeq = {
      conf.rooms.flatMap { room: Room =>
          bind("room", roomTemplate,
            "name" -> room.name.toForm,
            "moveUp" -> ajaxSubmit(?("room.moveUp"), () => { conf.moveUp(room); reDrawForm }),
            "moveDown" -> ajaxSubmit(?("room.moveDown"), () => { conf.moveDown(room); reDrawForm }),
            "delete" -> ajaxSubmit(?("common.delete"), () => { conf.deleteRoom(room); reDrawForm })
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
        "addRoom" -> ajaxSubmit(?("conference.rooms.add"), () => { conf.addRoom; reDrawForm }),
        "save" -> ajaxSubmit(if (conf.saved_?) ?("common.save") else ?("common.add"), () => { checkAndSave; reDraw }),
        "cancel" -> refreshButton(?("common.cancel"))
        ))
  }

  def list(listTemplate: NodeSeq) = {
    def doList(itemTemplate: NodeSeq): NodeSeq = {
      conferenceService.allConferences.flatMap { conf: Conference =>
        bind("conf", itemTemplate,
          "name" -> conf.name,
          "dateStart" -> conf.dateStart,
          "dateEnd" -> conf.dateEnd,
          "edit" -> a(() => { CurrentConference(conf); reDrawForm }, Text(?("common.edit"))),
          "delete" -> confirmLink("", () => conf.delete_!, ?("common.delete"), ?("conference.confirm_delete", conf.name)),
          "editSlots" -> anchor(SlotEditorLoc.link.createPath(conf), ?("conference.edit_slots"))
          )
      }
    }

    this.listTemplate = listTemplate;
    bind("conf", listTemplate,
      "item" -> doList _
      )
  }
}