package pl.softwaremill.model

import net.liftweb.util.FieldError
import net.liftweb.mapper.{Mapper, MappedInt}

/**
 * @author Adam Warski (adam at warski dot org)
 */
trait Positionable[T <: Mapper[T]] { this: Mapper[T] =>
  val defaultPosition: Int
  val validatePosition: Int => List[FieldError]

  object position extends MappedInt[T](this) {
    override def defaultValue = defaultPosition
    override def validations = validatePosition :: super.validations
  }
}