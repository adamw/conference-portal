package pl.softwaremill.model

import net.liftweb.http.S
import xml.Text
import net.liftweb.util.{FieldIdentifier, FieldError}

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

  private def valOperator(operator: (Int, Int) => Boolean, bound: Int, errorKey: String, field: FieldIdentifier, value: Int) = {
    if (operator(value, bound)) List(FieldError(field, Text(S.?(errorKey))))
    else Nil
  }
}