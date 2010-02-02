package pl.softwaremill.loc

import net.liftweb.sitemap.Loc
import Loc._
import net.liftweb.http.S._

import xml.Text

import pl.softwaremill.model.{User, Conference}

/**
 * @author Adam Warski (adam at warski dot org)
 */
object SlotEditorLoc extends ConferenceAwareLoc {
  protected val PathList = "conferences" :: "slot_editor" :: Nil

  def name = "SlotEditor"

  def text = new LinkText((conf: Conference) => Text(?("menu.slot_editor", conf.name)))

  def params = List(Hidden, User.testSuperUser)
}
