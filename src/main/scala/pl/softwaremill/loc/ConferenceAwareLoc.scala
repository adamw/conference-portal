package pl.softwaremill.loc

import net.liftweb.sitemap.Loc
import Loc._
import net.liftweb.http.S._
import net.liftweb.common._
import net.liftweb.http._

import pl.softwaremill.model.Conference
import pl.softwaremill.services.ConferenceService
import pl.softwaremill.lib.D
import pl.softwaremill.snippet.Util._
import pl.softwaremill.snippet.CurrentConference

import LocTools._

/**
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