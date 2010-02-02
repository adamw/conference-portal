package pl.softwaremill.loc

import net.liftweb.sitemap.Loc
import Loc._
import net.liftweb.http.S._
import net.liftweb.common._
import net.liftweb.http._

import xml.Text

import pl.softwaremill.model.User
import pl.softwaremill.lib.D
import pl.softwaremill.snippet.Util._
import pl.softwaremill.services.UserService
import pl.softwaremill.snippet.{CurrentConference, CurrentAuthor}
import LocTools._

/**
 * @author Adam Warski (adam at warski dot org)
 */
object AuthorLoc extends Loc[User] {
  private val AuthorPath = "author"

  def name = "ViewAuthor"

  def link = new Link[User](AuthorPath :: Nil) {
    override def pathList(user: User): List[String] = super.pathList(user) ++ List(user.id.is.toString)
  }

  def text = new LinkText((user: User) => Text(?("menu.view_author", user.shortName)))

  def params = Hidden :: showIfActiveConferenceSatisfies(conferenceAfterAcceptReject _) :: Nil

  def defaultValue = Empty

  override def rewrite = Full({
    case RewriteRequest(parsePath @ ParsePath(AuthorPath :: userId :: Nil, _, _, _), _, httpRequest) => {
      withActiveConference[User](null, conferenceAfterAcceptReject _) {
        val userService = D.inject_![UserService]
        val userBox: Box[User] = userService.findAcceptedAuthor(CurrentConference.is, userId)

        userBox match {
          case Full(user) => {
            CurrentAuthor(user);
            (finalResponse(AuthorPath :: Nil), user)
          }
          case _ => (RewriteResponse("error" :: Nil,
            Map(errorMessageParam -> ?("author.unknown"))), null)
        }
      }
    }
    case RewriteRequest(parsePath @ ParsePath(AuthorPath :: Nil, _, _, _), _, httpRequest) => {
      (RewriteResponse("error" :: Nil, Map(errorMessageParam -> ?("author.unknown.noid"))), null)
    }
  })
}