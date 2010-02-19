package pl.softwaremill.loc

import xml._

import net.liftweb.sitemap.Loc
import Loc._
import net.liftweb.http.S._
import net.liftweb.common._
import net.liftweb.http._

import pl.softwaremill.model._
import pl.softwaremill.services.ConferenceService
import pl.softwaremill.lib.D
import pl.softwaremill.snippet.Util._
import pl.softwaremill.snippet.CurrentConference

import LocTools._

/**
 * A location where the current conference is set using an url parameter.
 * @author Adam Warski (adam at warski dot org)
 */
trait ConferenceAwareLoc extends Loc[Conference] {
  private val ConfIdParam = "conf_id"
  
  protected val PathList: List[String]

  def link = new Link(PathList) with LinkWithParams[Conference] {
    def params(value: Conference) = { Map(ConfIdParam -> value.id.is.toString) }
  }

  def defaultValue = Empty

  override def rewrite = Full({
    case RewriteRequest(parsePath @ ParsePath(PathList, _, _, _), _, httpRequest) => {
      val conferenceService = D.inject[ConferenceService].open_!
      val conferenceIdOpt = httpRequest.param(ConfIdParam).firstOption
      val conferenceBox: Box[Conference] = conferenceIdOpt.flatMap { conferenceService.find(_) }

      conferenceBox match {
        case Full(conference) => {
          CurrentConference(conference);
          (finalResponse(parsePath), conference)
        }
        case _ => (RewriteResponse("error" :: Nil,
          Map(errorMessageParam -> ?("conference.unknown", conferenceIdOpt getOrElse ?("conference.unknown.noid")))), null)
      }
    }
  })
}

object AcceptRejectLoc extends ConferenceAwareLoc {
  protected val PathList = "conferences" :: "accept_reject" :: Nil

  def name = "AcceptReject"

  def text = new LinkText((conf: Conference) => Text(?("menu.accept_reject", conf.name)))

  def params = List(Hidden, User.testSuperUser)
}

object SlotEditorLoc extends ConferenceAwareLoc {
  protected val PathList = "conferences" :: "slot_editor" :: Nil

  def name = "SlotEditor"

  def text = new LinkText((conf: Conference) => Text(?("menu.slot_editor", conf.name)))

  def params = List(Hidden, User.testSuperUser)
}

object StatisticsLoc extends ConferenceAwareLoc {
  protected val PathList = "conferences" :: "statistics" :: Nil

  def name = "Statistics"

  def text = new LinkText((conf: Conference) => Text(?("menu.statistics", conf.name)))

  def params = List(Hidden, User.testSuperUser)
}

object CmsAdminLoc extends ConferenceAwareLoc {
  protected val PathList = "conferences" :: "cms" :: Nil

  def name = "CmsAdmin"

  def text = new LinkText((conf: Conference) => Text(?("menu.cms_admin", conf.name)))

  def params = List(Hidden, User.testSuperUser)
}

object CreateScheduleLoc extends ConferenceAwareLoc {
  protected val PathList = "conferences" :: "create_schedule" :: Nil

  def name = "CreateSchedule"

  def text = new LinkText((conf: Conference) => Text(?("menu.create_schedule", conf.name)))

  def params = List(Hidden, User.testSuperUser)
}