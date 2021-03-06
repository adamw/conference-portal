package pl.softwaremill.services

import net.liftweb.mapper._

import pl.softwaremill.model._

import net.liftweb.util.Mailer
import CustomMailer._

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
  def confirmRegistration(code: String): Box[Registration]
  def validateRegister(code: String): Box[Registration]
  def sendConfirmations(conf: Conference): Int
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

      sendEmail(data.user.email, ?("register.validate.mail.subject", confName), bodyText, () => ())
    }
  }

  def validateRegister(code: String) = {
    validateRegister(code, true)
  }

  private def validateRegister(code: String, sendGeneratedPassword: Boolean) = {
    Registration.find(By(Registration.confirmationCode, code)).map({ reg =>
      if (!reg.validated) {
        // Validating the registration
        reg.validated(true).save

        // Sending a "registered" e-mail
        val user = reg.user.obj.open_!
        val email = user.email
        val confName = reg.conference.obj.open_!.name.is
        val schedulePreferencesLoginLink = S.hostAndPath + Locs.AutoLoginSchedulePreferencesLoc.link.createPath(user)
        val bodyText = if (sendGeneratedPassword) {
          ?("register.registered.mail.body", confName, schedulePreferencesLoginLink, email, reg.confirmationCode, confName)
        } else {
          ?("register.registered.mail.body.nopassword", confName, schedulePreferencesLoginLink)
        }

        sendEmail(email, ?("register.registered.mail.subject", confName), bodyText, () => ())
      }

      reg
    })
  }

  private def sendEmail(to: String, subject: String, body: String, afterSend: () => Unit) {
    CustomMailer.sendMail(
      From("do-not-reply@javarsovia.pl"),
      Subject(subject),
      afterSend,
      To(to),
      PlainPlusBodyType(body, "UTF-8"))
  }

  def sendConfirmations(conf: Conference) = {
    var sent = 0
    for (reg <- Registration.findAll(By(Registration.conference, conf),
      By(Registration.validated, true), By(Registration.confirmationEmailSent, false))) {
      val email = reg.user.obj.open_!.email
      val confName = conf.name.is
      val confirmationLink = S.hostAndPath + Locs.RegisterConfirmLoc.link.createPath(reg.confirmationCode)
      sendEmail(email, ?("register.confirm.mail.subject", confName),
        ?("register.confirm.mail.body", confName, confirmationLink,
          confName), () => { reg.confirmationEmailSent(true).save })

      sent = sent + 1
    }

    sent
  }

  def confirmRegistration(code: String) = {
    Registration.find(By(Registration.confirmationCode, code)).map({ reg =>
      // Confirming the registration
      reg.confirmed(true).save

      reg
    })
  }
}

sealed case class RegisterData(user: User, registration: Registration)