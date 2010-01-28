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

  object mappedStatus extends MappedInt(this) {
    override def defaultValue = PaperStatus.Pending.id
    override def dbColumnName = "status"
  }

  def status = PaperStatus(mappedStatus.is)

  def status(newStatus: PaperStatus.Value) = mappedStatus(newStatus.id)

  def author = user.obj.map { usr: User => usr.shortName } openOr ?("paper.no_author")
}

object Paper extends Paper with LongKeyedMetaMapper[Paper]

object PaperStatus extends Enumeration {
  val Pending = Value("paper_status.pending")
  val Accepted = Value("paper_status.accepted")
  val Rejected = Value("paper_status.rejected")
}

/**
 * A join-entity between papers and users. Specifies which users are interested in which papers.
 */
class UserInterested extends LongKeyedMapper[UserInterested] with IdPK {
  def getSingleton = UserInterested

  object user extends LongMappedMapper[UserInterested, User](this, User)

  object paper extends LongMappedMapper[UserInterested, Paper](this, Paper)
}

object UserInterested extends UserInterested with LongKeyedMetaMapper[UserInterested]
