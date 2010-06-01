package pl.softwaremill.snippet

import xml._

import net.liftweb.util.Helpers._
import net.liftweb.http._

import S._
import SHtml._

import net.liftweb.http.js.JsCmds._

import pl.softwaremill.lib.D
import pl.softwaremill.model.{User, Sex}
import pl.softwaremill.services.{RegistrationService, StatisticsService}

import SnippetTools._

import xml._
import pl.softwaremill.loc.StatisticsLoc

/**
 * @author Adam Warski (adam at warski dot org)
 */
class Statistics {
  lazy val statsService = D.inject_![StatisticsService]
  lazy val registrationService = D.inject_![RegistrationService]

  def render(template: NodeSeq) = {
    val conf = CurrentConference.is

    def bindAllParticipantsRow(rowTemplate: NodeSeq): NodeSeq = {
      statsService.all(conf).flatMap({ case (user, reg) => {
        bind("participant", rowTemplate,
          "firstName" -> user.firstName,
          "lastName" -> user.lastName,
          "email" -> user.email,
          "sex" -> ?(user.sex.toString),
          "homeTown" -> user.homeTown,
          "source" -> reg.source,
          "confirmed" -> reg.confirmed.toString,
          "confirmationSent" -> reg.confirmationEmailSent.toString,
          "tshirtSize" -> user.tshirtSize.toString
          )
      } })
    }

    bind("stats", template,
      "registered" -> statsService.registered(conf),
      "confirmed" -> statsService.confirmed(conf),
      "femaleRegistered" -> statsService.registered(conf, Sex.Female),
      "maleRegistered" -> statsService.registered(conf, Sex.Male),
      "madeSchedulePreferences" -> statsService.madeSchedulePreferences(conf),
      "allParticipants" -> bindAllParticipantsRow _,
      "sendConfirmations" -> link(StatisticsLoc.link.createPath(CurrentConference.is), () => {
        val sent = registrationService.sendConfirmations(conf)
        notice(?("register.sendconfirm.sent", sent))
        _Noop
      }, Text(?("register.sendconfirm")), confirmAttr(?("register.sendconfirm.confirm")))
      )
  }
}