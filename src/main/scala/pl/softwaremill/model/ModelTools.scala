package pl.softwaremill.model

import xml.{XML, Text}

import net.liftweb.http.S
import net.liftweb.util.{FieldIdentifier, FieldError}
import net.liftweb.mapper.{MappedField, Mapper, LongMappedForeignMapper}

import S._

import dispatch._

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

  def valRequired(field: MappedField[_, _])(value: String) = {
    if (value == null || value == "") List(FieldError(field, S.?("common.field.required", field.displayName.toLowerCase))) else Nil
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

  def checkCaptcha(challenge: String, response: String): Boolean = {
    val http = new Http
    val req = :/("api-verify.recaptcha.net") / "verify"
    val remoteAddress = S.request.map(_.request.remoteAddress) openOr ""
    val postParams = Map("privatekey" -> "6LfLawsAAAAAAN0cK1scWz9osc6wHP6E_O2HroAH",
      "remoteip" -> remoteAddress,
      "challenge" -> challenge,
      "response" -> response)

    val rform = req << postParams
    http(rform >~ { s => { s.getLine(1) == "true" } })
  }

  def validateCaptchaAndEntity(wrongCaptchaField: FieldIdentifier, entity: Mapper[_]): List[FieldError] = {
    def wrongCaptcha = FieldError(wrongCaptchaField, Text(?("captcha.wrong")))
    val entityValidateResult = entity.validate

    (for (captchaChallenge <- S.param("recaptcha_challenge_field");
          captchaResponse <- S.param("recaptcha_response_field")) yield {
      if (!checkCaptcha(captchaChallenge, captchaResponse))
        wrongCaptcha :: entityValidateResult
      else
        entityValidateResult
    }) openOr List(wrongCaptcha)
  }
}