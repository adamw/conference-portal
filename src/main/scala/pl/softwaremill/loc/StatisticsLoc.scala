package pl.softwaremill.loc

import net.liftweb.sitemap.Loc
import Loc._
import net.liftweb.http.S._

import xml.Text

import pl.softwaremill.model.{User, Conference}

/**
 * @author Adam Warski (adam at warski dot org)
 */
object StatisticsLoc extends ConferenceAwareLoc {
  protected val PathList = "conferences" :: "statistics" :: Nil

  def name = "Statistics"

  def text = new LinkText((conf: Conference) => Text(?("menu.statistics", conf.name)))

  def params = List(Hidden, User.testSuperUser)
}