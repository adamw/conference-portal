package pl.softwaremill.model

import net.liftweb.mapper._
import net.liftweb.http.S._
import net.liftweb.http.SHtml._
import net.liftweb.common._
import net.liftweb.common.Box._

import pl.softwaremill.services.ConferenceService
import pl.softwaremill.lib.D

/**
 * @author Adam Warski (adam at warski dot org)
 */
class Paper extends LongKeyedMapper[Paper] with IdPK {
  def getSingleton = Paper

  object title extends MappedPoliteString(this, 256) {
    override def validations = valMinLen(3, ?("paper.title.invalid_length")) _ :: super.validations
  }

  object shortDescription extends MappedTextarea(this, 3000) {
    override def validations = valMinLen(100, ?("paper.short_description.invalid_length")) _ :: super.validations
  }

  object user extends LongMappedMapper[Paper, User](this, User)

  object conference extends LongMappedMapper[Paper, Conference](this, Conference) {
    override def _toForm = {
      val conferences = D.inject_![ConferenceService].conferencesInState(ConferenceState.C4P)
      val options = conferences.map { conf => (conf, conf.name.is) }
      val defaultSelection: Box[Conference] = conference.obj match {
        case f @ Full(_) => f
        case _ => conferences.firstOption
      }
      Full(selectObj[Conference](options, defaultSelection, conference(_)))
    }

    override def validations = ModelTools.valNotNull("paper.conference_required", this) _ :: super.validations
  }
}

object Paper extends Paper with LongKeyedMetaMapper[Paper]