package pl.softwaremill.snippet

import xml._

import net.liftweb.util.Helpers._
import net.liftweb.http._

import S._
import SHtml._

import pl.softwaremill.lib.D
import pl.softwaremill.services.RegistrationService
import pl.softwaremill.model.User

/**
 * @author Adam Warski (adam at warski dot org)
 */
class Register {
  lazy val registrationService = D.inject_![RegistrationService]

  def render(template: NodeSeq) = {
    val user = User.currentUser.open_!
    val conf = CurrentConference.is
    val isRegistered = registrationService.isRegistered(user, conf)
    bind("register", template,
      "info" -> ?("register.info", conf.name.is),
      "do" -> (if (isRegistered)
        NodeSeq.Empty
      else
        link("", () => { registrationService.registerUser(user, conf); notice(?("register.do.successfull")) }, Text(?("register.do.link", conf.name.is)))),
      "undo" -> (if (isRegistered)
        link("", () => { registrationService.unregisterUser(user, conf); notice(?("register.undo.successfull")) }, Text(?("register.undo.link", conf.name.is)))
      else
        NodeSeq.Empty)
      )
  }
}