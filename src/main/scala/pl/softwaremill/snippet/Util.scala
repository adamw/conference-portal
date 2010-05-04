package pl.softwaremill.snippet

import xml._

import net.liftweb.http._
import net.liftweb.util.Helpers._

import S._

import pl.softwaremill.model.User

/**
 * @author Adam Warski (adam at warski dot org)
 */
class Util {
 def in(html: NodeSeq) =
   if (User.loggedIn_?) html else NodeSeq.Empty

 def out(html: NodeSeq) =
   if (!User.loggedIn_?) html else NodeSeq.Empty

  def error(errorTemplate: NodeSeq) = {
    bind("error", errorTemplate,
      "message" -> (S.param(Util.errorMessageParam) openOr ?("error.unknown"))
    )
  }

  def info(infoTemplate: NodeSeq) = {
    bind("info", infoTemplate,
      "message" -> (S.param(Util.infoMessageParam) openOr "")
    )
  }

  def unavailable(unavailableTemplate: NodeSeq) = {
    bind("unavailable", unavailableTemplate,
      "message" -> (?(Util.UnavailableMessageKey.is))
    )
  }
}

object Util {
  val errorMessageParam = "errorMessage";
  val infoMessageParam = "infoMessage";
  object UnavailableMessageKey extends RequestVar("")
}