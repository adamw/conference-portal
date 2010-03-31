package pl.softwaremill.services

import pl.softwaremill.model.{User, Registration, Conference, Sex}
import net.liftweb.mapper.{PreCache, In, By}

/**
 * @author Adam Warski (adam at warski dot org)
 */
trait StatisticsService {
  def registered(conf: Conference): Long
  def registered(conf: Conference, sex: Sex.Value): Long
  def confirmed(conf: Conference): Long
  def madeSchedulePreferences(conf: Conference): Long
  def all(conf: Conference): List[(User, Registration)]
}

class StatisticsServiceImpl extends StatisticsService {
  def registered(conf: Conference): Long = {
    Registration.count(By(Registration.conference, conf))
  }

  def registered(conf: Conference, sex: Sex.Value): Long = {
    Registration.count(By(Registration.conference, conf), In(Registration.user, User.id, By(User.mappedSex, sex.id)))
  }

  def confirmed(conf: Conference): Long = {
    Registration.count(By(Registration.conference, conf), By(Registration.confirmed, true))
  }

  def madeSchedulePreferences(conf: Conference): Long = {
    0
  }

  def all(conf: Conference): List[(User, Registration)] = {
    Registration.findAll(By(Registration.conference, conf), PreCache(Registration.user, true))
            .map(r => (r.user.obj.open_!, r))
  }
}