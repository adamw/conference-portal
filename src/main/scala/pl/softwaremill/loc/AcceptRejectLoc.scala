package pl.softwaremill.loc

import net.liftweb.sitemap.Loc
import Loc._
import net.liftweb.http.S._

import xml.Text

import pl.softwaremill.model.Conference

/**
 * @author Adam Warski (adam at warski dot org)
 */
object AcceptRejectLoc extends ConferenceAwareLoc {
  protected val PathList = "conferences" :: "accept_reject" :: Nil

  def name = "AcceptReject"

  def text = new LinkText((conf: Conference) => Text(?("menu.accept_reject", conf.name)))

  def params = List(Hidden)
}