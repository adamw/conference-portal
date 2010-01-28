package pl.softwaremill.loc

import xml._

import net.liftweb.sitemap.Loc
import Loc._
import net.liftweb.http.S._
import net.liftweb.common._
import net.liftweb.http._

import pl.softwaremill.snippet.CurrentConference
import pl.softwaremill.model.{Configuration, ConferenceState}

import LocTools._

/**
 * @author Adam Warski (adam at warski dot org)
 */
object SchedulePreferencesLoc extends Loc[Unit] {
  private val PathList = "schedule_preferences" :: Nil

  def name = "SchedulePreferences"

  def text = new LinkText(ignore => Text(?("menu.schedule_preferences")))

  def params = List(showIfActiveConferenceInState(ConferenceState.Schedule), showRequireLogin)

  def link = new Link(PathList)

  def defaultValue = Full(())

  override def rewrite = Full({
    case RewriteRequest(parsePath @ ParsePath(PathList, _, _, _), _, _) => {
      Configuration.is.activeConference match {
        case Full(conf) => {
          CurrentConference(conf)
          (finalResponse(parsePath), ())
        }
        case _ => (RewriteResponse("error" :: Nil), ())
      }
    }
  })
}