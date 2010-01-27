package pl.softwaremill.services

import net.liftweb.mapper._
import net.liftweb.common.Box

import pl.softwaremill.model.{ConferenceState, Conference}

/**
 * @author Adam Warski (adam at warski dot org)
 */
trait ConferenceService {
  def allConferences: List[Conference]
  def conferencesInState(state: ConferenceState.Value): List[Conference]
  def find(id: String): Box[Conference]
}

class ConferenceServiceImpl extends ConferenceService {
  def allConferences: List[Conference] = {
    Conference.findAll(OrderBy(Conference.name, Ascending))
  }

  def conferencesInState(state: ConferenceState.Value): List[Conference] = {
    Conference.findAll(By(Conference.mappedState, state.id), OrderBy(Conference.name, Ascending))
  }

  def find(id: String): Box[Conference] = {
    Conference.find(id)
  }
}