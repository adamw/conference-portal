package pl.softwaremill.services

import net.liftweb.mapper._

import pl.softwaremill.model._

import net.liftweb.util.Mailer
import Mailer._

import net.liftweb.http.S._
import net.liftweb.http.S
import net.liftweb.common._
import pl.softwaremill.loc.Locs

/**
 * @author Adam Warski (adam at warski dot org)
 */
trait RegistrationService {
  def newRegisterData(conf: Conference): RegisterData
  def register(data: RegisterData)
  def isRegistered(user: User, conf: Conference): Boolean
  def confirmRegistration(code: String): Boolean
  def validateRegister(code: String): Box[Registration]
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
    val saved = data.user.saved_?
    if (!saved) data.user.save

    val conf = data.registration.conference.obj.open_!

    val current = currentRegistration(data.user, conf)
    if (!current.isDefined) {
      data.registration.save
    }

    // If the user was saved before, it means that he created an account eariler and verified it. Then there's no need
    // for sending validation e-mails, only sending the confirmation.
    if (saved) {
      validateRegister(data.registration.confirmationCode, false)
    } else {
      // Send validation e-mail
      val confName = conf.name.is
      val validationLink = S.hostAndPath + Locs.RegisterValidateLoc.link.createPath(data.registration.confirmationCode)
      val bodyText = ?("register.validate.mail.body", confName, validationLink, confName)

      sendEmail(data.user.email, ?("register.validate.mail.subject", confName), bodyText)
    }
  }

  def validateRegister(code: String) = {
    validateRegister(code, true)
  }

  private def validateRegister(code: String, sendGeneratedPassword: Boolean) = {
    Registration.find(By(Registration.confirmationCode, code)).map({ reg =>
      // Validating the registration
      reg.validated(true).save

      // Sending a "registered" e-mail
      val email = reg.user.obj.open_!.email
      val confName = reg.conference.obj.open_!.name.is
      val bodyText = if (sendGeneratedPassword) {
        ?("register.registered.mail.body", confName, confName, email, reg.confirmationCode, confName)
      } else {
        ?("register.registered.mail.body.nopassword", confName, confName)
      }

      sendEmail(email, ?("register.registered.mail.subject", confName), bodyText)

      reg
    })
  }

  private def sendEmail(to: String, subject: String, body: String) {
    Mailer.sendMail(
      From("do-not-reply@javarsovia.pl"),
      Subject(subject),
      To(to),
      PlainPlusBodyType(body, "UTF-8"))
  }

  def confirmRegistration(code: String) = {
    false
  }
}

sealed case class RegisterData(user: User, registration: Registration)