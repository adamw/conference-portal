package pl.softwaremill.services

import net.liftweb.mapper._

import pl.softwaremill.model._

import net.liftweb.util.Mailer
import Mailer._

import net.liftweb.http.S._

/**
 * @author Adam Warski (adam at warski dot org)
 */
trait RegistrationService {
  def newRegisterData(conf: Conference): RegisterData
  def register(data: RegisterData)
  def isRegistered(user: User, conf: Conference): Boolean
  def confirmRegistration(code: String): Boolean
}

class RegistrationServiceImpl extends RegistrationService {
  private def currentRegistration(user: User, conf: Conference) = {
    Registration.find(By(Registration.user, user), By(Registration.conference, conf))
  }

  def newRegisterData(conf: Conference) = {
    val registration = new Registration
    // By default the password is the confirmation code
    val user = User.currentUser openOr (new User).password(registration.confirmationCode.is).validated(true)

    registration.conference(conf).user(user)

    RegisterData(user, registration)
  }

  def isRegistered(user: User, conf: Conference) = user.saved_? && currentRegistration(user, conf).isDefined

  def register(data: RegisterData) {
    if (!data.user.saved_?) data.user.save

    val conf = data.registration.conference.obj.open_!

    val current = currentRegistration(data.user, conf)
    if (!current.isDefined) {
      data.registration.save
    }

    // Send information e-mail
    val confName = conf.name.is
    val bodyText = ?("register.mail.body", confName, confName, data.user.email,
        data.registration.confirmationCode, confName)
    Mailer.sendMail(
      From("do-not-reply@javarsovia.pl"),
      Subject(?("register.mail.subject", confName)),
      To(data.user.email),
      PlainPlusBodyType(bodyText, "UTF-8"))
  }

  def confirmRegistration(code: String) = {
    false
  }
}

sealed case class RegisterData(user: User, registration: Registration)