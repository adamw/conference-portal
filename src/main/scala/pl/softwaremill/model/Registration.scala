package pl.softwaremill.model

import net.liftweb.mapper._

/**
 * @author Adam Warski (adam at warski dot org)
 */
class Registration extends LongKeyedMapper[Registration] with IdPK {
  def getSingleton = Registration

  object user extends LongMappedMapper[Registration, User](this, User)

  object conference extends LongMappedMapper[Registration, Conference](this, Conference)

  object confirmed extends MappedBoolean(this)
}

object Registration extends Registration with LongKeyedMetaMapper[Registration]
