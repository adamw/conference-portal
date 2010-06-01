package pl.softwaremill.services

import net.liftweb.mapper._
import net.liftweb.common._

import pl.softwaremill.lib.D
import pl.softwaremill.model.{Conference, PaperStatus, Paper, User}

/**
 * @author Adam Warski (adam at warski dot org)
 */
trait UserService {
  def find(id: String): Box[User]
  def findAcceptedAuthor(conf: Conference, id: String): Box[User]
  def acceptedAuthors(conf: Conference): List[User]
  def autologin(code: String): Box[User]
}

class UserServiceImpl extends UserService {
  lazy val paperService = D.inject_![PaperService]

  def find(id: String): Box[User] = {
    User.find(id)
  }

  def findAcceptedAuthor(conf: Conference, id: String): Box[User] = {
    val users: List[User] = Paper.findMap(
      By(Paper.conference, conf),
      By(Paper.mappedStatus, PaperStatus.Accepted.id),
      BySql("paper.user_c = ?", IHaveValidatedThisSQL("adamw", "01/02/2010"), id))(_.user.obj)

    // If there are multiple users, all are the same (in case when the user authors more than one paper)
    users.firstOption
  }

  def acceptedAuthors(conf: Conference): List[User] = {
    paperService.acceptedConferencePapers(conf).flatMap(_.user.obj)
  }

  def autologin(code: String): Box[User] =  {
    User.find(By(User.uniqueId, code))
  }
}