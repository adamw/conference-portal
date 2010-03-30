package pl.softwaremill.snippet

import xml._

import net.liftweb.util.Helpers._
import net.liftweb.http._
import net.liftweb.common._

import S._
import SHtml._

import pl.softwaremill.lib.D
import pl.softwaremill.model.ModelTools._
import pl.softwaremill.model.{Registration, User}
import pl.softwaremill.services.{RegisterData, RegistrationService}

/**
 * @author Adam Warski (adam at warski dot org)
 */
class Register {
  lazy val registrationService = D.inject_![RegistrationService]

  object OngoingRegistrationData extends RequestVar(registrationService.newRegisterData(CurrentConference.is))

  def render(template: NodeSeq) = {
    val conf = CurrentConference.is

    def doRegisterNewUser(template: NodeSeq) = {
      val ongoingRegistration = OngoingRegistrationData.is
      val user = ongoingRegistration.user

      bind("do", template,
        "registerFields" -> User.registerFields.flatMap(f =>
          <tr><td>{f.displayName}</td><td>{User.getActualBaseField(user, f).toForm openOr NodeSeq.Empty}</td></tr>),
        "source" -> <tr><td>{Registration.source.displayName}</td><td>{ongoingRegistration.registration.source.toForm openOr NodeSeq.Empty}</td></tr>,
        "submit" -> submit(?("register.do.text"), () => {
          validateCaptchaAndEntity(user.id, user) match {
            case Nil => registrationService.register(ongoingRegistration); notice(?("register.do.successfull")); User.logUserIn(user)
            case xs => S.error(xs); OngoingRegistrationData(ongoingRegistration)
          }
        }))
    }

    val isRegistered = registrationService.isRegistered(OngoingRegistrationData.is.user, conf)
    val isNewUser = OngoingRegistrationData.is.user.saved_?

    if (isRegistered) Text(?("register.registered", conf.name.is))
    else if (!isNewUser)
      bind("register", template,
        "info" -> ?("register.nouser.info", conf.name.is),
        "do" -> doRegisterNewUser _
        )
    else {
      val ongoingRegistration = OngoingRegistrationData.is
      bind("register", template,
        "info" -> ?("register.existinguser.info", conf.name.is),
        "do" -> submit(?("register.do.text"), () => {
          registrationService.register(ongoingRegistration); notice(?("register.do.successfull"))
        })
        )
    }
  }
}
