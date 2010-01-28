package pl.softwaremill.model

import net.liftweb.mapper._
import net.liftweb.common._

/**
 * An entity holding system configuration
 * @author Adam Warski (adam at warski dot org)
 */
class Configuration extends LongKeyedMapper[Configuration] with IdPK {
  def getSingleton = Configuration

  protected object _activeConference extends LongMappedMapper[Configuration, Conference](this, Conference) {
    override def dbColumnName = "active_conference"
  }

  def activeConference: Box[Conference] = _activeConference.obj

  def activeConference(conference: Box[Conference]) {
    _activeConference(conference)
  }
}

object Configuration extends Configuration with LongKeyedMetaMapper[Configuration] {
  def is: Configuration = {
    val confBox = Configuration.find()
    confBox match {
      case Full(conf) => conf
      case _ => {
        val conf = new Configuration
        conf.save
        conf
      }
    }
  }
}