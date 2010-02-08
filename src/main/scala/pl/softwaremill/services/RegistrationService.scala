package pl.softwaremill.services

import net.liftweb.mapper._

import pl.softwaremill.model._

/**
 * @author Adam Warski (adam at warski dot org)
 */
trait RegistrationService {
  def registerUser(user: User, conf: Conference)
  def unregisterUser(user: User, conf: Conference)
  def isRegistered(user: User, conf: Conference) : Boolean
}

class RegistrationServiceImpl extends RegistrationService {
  private def currentRegistration(user: User, conf: Conference) = {
    Registration.find(By(Registration.user, user), By(Registration.conference, conf))
  }

  def isRegistered(user: User, conf: Conference) = currentRegistration(user, conf).isDefined

  def registerUser(user: User, conf: Conference) {
    val current = currentRegistration(user, conf)
    if (!current.isDefined) {
      (new Registration).conference(conf).user(user).save
    }
  }

  def unregisterUser(user: User, conf: Conference) {
    val current = currentRegistration(user, conf)
    current.map { reg => reg.delete_! }
  }
}