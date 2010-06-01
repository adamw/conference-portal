package pl.softwaremill.services

import pl.softwaremill.model.{User, Registration, Conference, Sex}
import net.liftweb.mapper.{DB, PreCache, In, By}

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
    Registration.count(By(Registration.conference, conf), By(Registration.validated, true))
  }

  def registered(conf: Conference, sex: Sex.Value): Long = {
    Registration.count(By(Registration.conference, conf), By(Registration.validated, true),
      In(Registration.user, User.id, By(User.mappedSex, sex.id)))
  }

  def confirmed(conf: Conference): Long = {
    Registration.count(By(Registration.conference, conf), By(Registration.validated, true),
      By(Registration.confirmed, true))
  }

  def madeSchedulePreferences(conf: Conference): Long = {
    val sql = """select count(distinct ui.user_c) from userinterested ui
	join paper p on ui.paper = p.id
	join conference c on p.conference = c.id
	where c.id = ?"""

    val result = DB.runQuery(sql, List(conf.id.toString))
    (result._2)(0)(0).toLong
  }

  def all(conf: Conference): List[(User, Registration)] = {
    Registration.findAll(By(Registration.conference, conf), By(Registration.validated, true),
      PreCache(Registration.user, true)).map(r => (r.user.obj.open_!, r))
  }
}