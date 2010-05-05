package pl.softwaremill.model

import net.liftweb.mapper._
import net.liftweb.common._
import net.liftweb.http._

import SHtml._
import S._

/**
 * @author Adam Warski (adam at warski dot org)
 */
class Registration extends LongKeyedMapper[Registration] with IdPK {
  def getSingleton = Registration

  object user extends LongMappedMapper[Registration, User](this, User)

  object conference extends LongMappedMapper[Registration, Conference](this, Conference)

  object confirmed extends MappedBoolean(this)

  object confirmationEmailSent extends MappedBoolean(this)

  object validated extends MappedBoolean(this)

  object source extends MappedString(this, 128) {
    val options = SourceOptions.findAll(OrderBy(SourceOptions.id, Ascending)).map(_.value.is)

    override def _toForm = {
      val opts = options.map { opt => (opt, opt) }.toList
      Full(selectObj[String](opts, Empty, source(_)))
    }

    override def displayName = ?("registration.source")
  }

  object confirmationCode extends MappedUniqueId(this, 10)
}

object Registration extends Registration with LongKeyedMetaMapper[Registration]

class SourceOptions extends LongKeyedMapper[SourceOptions] with IdPK {
  def getSingleton = SourceOptions

  object value extends MappedString(this, 128)
}

object SourceOptions extends SourceOptions with LongKeyedMetaMapper[SourceOptions]
