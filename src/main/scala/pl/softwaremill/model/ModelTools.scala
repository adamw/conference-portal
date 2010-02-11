package pl.softwaremill.model

import net.liftweb.http.S
import net.liftweb.util.{FieldIdentifier, FieldError}
import net.liftweb.mapper.LongMappedForeignMapper
import xml.{XML, Text}

/**
 * @author Adam Warski (adam at warski dot org)
 */
object ModelTools {
  def valMin(min: Int, errorKey: String, field: FieldIdentifier)(value: Int) = {
    valOperator(_ < _, min, errorKey, field, value)
  }

  def valMax(max: Int, errorKey: String, field: FieldIdentifier)(value: Int) = {
    valOperator(_ > _, max, errorKey, field, value)
  }

  def valMaxMin(min: Int, max: Int, errorKey: String, field: FieldIdentifier)(value: Int) = {
    valMin(min, errorKey, field)(value) match {
      case Nil => valMax(max, errorKey, field)(value)
      case errors => errors
    }
  }

  def valXml(errorKey: String, field: FieldIdentifier)(xml: String) = {
    try {
      if (xml != "") XML.loadString(xml)
      Nil
    } catch {
      case e => List(FieldError(field, S.?(errorKey, e.getMessage)))
    }
  }

  private def valOperator(operator: (Int, Int) => Boolean, bound: Int, errorKey: String, field: FieldIdentifier, value: Int) = {
    if (operator(value, bound)) List(FieldError(field, Text(S.?(errorKey))))
    else Nil
  }

  def valNotNull(errorKey: String, field: LongMappedForeignMapper[_,_])(value: Long) = {
    field.valHasObj(value) map { ignore => FieldError(field, Text(S.?(errorKey))) }
  }
}