package pl.softwaremill.snippet

import xml.NodeSeq
import pl.softwaremill.model.User
import net.liftweb.http._
import S._
import net.liftweb.util.Helpers._

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
}

object Util {
  val errorMessageParam = "message";
}