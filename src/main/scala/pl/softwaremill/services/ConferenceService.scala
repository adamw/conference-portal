package pl.softwaremill.services

import pl.softwaremill.model.Conference
import net.liftweb.mapper._
import net.liftweb.common.Box

/**
 * @author Adam Warski (adam at warski dot org)
 */
trait ConferenceService {
  def allConferences: List[Conference]
  def find(id: String): Box[Conference]
}

class ConferenceServiceImpl extends ConferenceService {
  def allConferences: List[Conference] = {
    Conference.findAll(OrderBy(Conference.name, Ascending))
  }

  def find(id: String): Box[Conference] = {
    Conference.find(id)
  }
}