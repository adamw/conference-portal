package pl.softwaremill.loc

import xml._

import net.liftweb.sitemap.Loc
import net.liftweb.common._
import net.liftweb.http._

import Loc._
import S._

import pl.softwaremill.model._
import pl.softwaremill.lib.D
import pl.softwaremill.services._
import pl.softwaremill.snippet.{CurrentAuthor, CurrentConference, Util}

import LocTools._
import Util._

/**
 * @author Adam Warski (adam at warski dot org)
 */
object Locs {
  trait PrefixLoc[T] extends Loc[T] {
    protected val PathList: List[String]
    protected def doRewrite(request: RewriteRequest): (RewriteResponse, T)

    protected def default: T
    def defaultValue = Full(default)

    def rewrite: LocRewrite
  }

  /**
   * Serves all paths with the path equals to {@code PathList} (but not sub-paths).
   */
  trait SinglePathLoc[T] extends PrefixLoc[T] {
    override def rewrite = Full({
      case request @ RewriteRequest(ParsePath(PathList, _, _, _), _, _) => doRewrite(request)
    })

    def link = new Link(PathList)

    def params: List[Loc.LocParam[T]] = Nil
  }

  /**
   * Makes the loc accessible and shown if the current conference meets a condition.
   */
  trait AcceptableConferenceLoc[T] extends PrefixLoc[T] {
    protected def conferenceAcceptable(conf: Conference): Boolean

    override abstract def params: List[Loc.LocParam[T]] = ifActiveConferenceSatisfies(conferenceAcceptable _) :: super.params

    override abstract def doRewrite(request: RewriteRequest): (RewriteResponse, T) = {
      if (!conferenceAcceptable(CurrentConference.is)) {
        (RewriteResponse("error" :: Nil), default)
      } else {
        super.doRewrite(request)
      }
    }
  }

  /**
   * Sets the current conference basing on the active conference.
   */
  trait ActiveConferenceLoc[T] extends PrefixLoc[T] {
    override abstract def doRewrite(request: RewriteRequest): (RewriteResponse, T) = {
      Configuration.is.activeConference match {
        case Full(conf) => {
          CurrentConference(conf)
          super.doRewrite(request)
        }
        case _ => (RewriteResponse("error" :: Nil), default)
      }
    }
  }

  /**
   * Sends a final response with the current parse path and the default value.
   */
  trait FinalResponseSinglePathLoc[T] extends PrefixLoc[T] {
    override def doRewrite(request: RewriteRequest): (RewriteResponse, T) = {
      request match { case RewriteRequest(parsePath, _, _) => (finalResponse(parsePath), default) }
    }
  }

  /**
   * If a condition isn't met, the loc shows an unavailable page with the specified message
   */
  trait UnavailableConferenceLoc[T] extends Loc[T] {
    protected def conferenceAvailable(conf: Conference): Boolean

    protected def unavailableKey: String

    override def calcTemplate: Box[NodeSeq] = {
      if (!conferenceAvailable(CurrentConference.is)) {
        UnavailableMessageKey(unavailableKey)
        TemplateFinder.findAnyTemplate("unavailable" :: Nil)
      } else super.calcTemplate
    }
  }

  class AuthorLocBase extends PrefixLoc[User] {
    private val AuthorPath = "author"

    protected val PathList = AuthorPath :: Nil
    protected def default: User = null

    override def params: List[Loc.LocParam[User]] = Hidden :: Nil

    def name = "ViewAuthor"

    override def link = new Link[User](AuthorPath :: Nil) {
      override def pathList(user: User): List[String] = super.pathList(user) ++ List(user.id.is.toString)
    }

    def text = new LinkText((user: User) => Text(?("menu.view_author", user.shortName)))

    protected def doRewrite(request: RewriteRequest): (RewriteResponse, User) = {
      request match {
        case RewriteRequest(parsePath @ ParsePath(AuthorPath :: userId :: Nil, _, _, _), _, httpRequest) => {
          val userService = D.inject_![UserService]
          val userBox: Box[User] = userService.findAcceptedAuthor(CurrentConference.is, userId)

          userBox match {
            case Full(user) => {
              CurrentAuthor(user);
              (finalResponse(AuthorPath :: Nil), user)
            }
            case _ => (RewriteResponse("error" :: Nil, Map(errorMessageParam -> ?("author.unknown"))), null)
          }
        }
        case _ => (RewriteResponse("error" :: Nil, Map(errorMessageParam -> ?("author.unknown.noid"))), null)
      }
    }

    override def rewrite = Full({
      // Accepting any paths which start with "author"
      case request @ RewriteRequest(ParsePath(AuthorPath :: _, _, _, _), _, _) => {
        doRewrite(request)
      }
    })
  }

  val AuthorLoc = new AuthorLocBase with AcceptableConferenceLoc[User] with ActiveConferenceLoc[User] {
    protected def conferenceAcceptable(conf: Conference) = conf.conferenceAfterAcceptReject
  }

  val SchedulePreferencesLoc = new SinglePathLoc[Unit] with FinalResponseSinglePathLoc[Unit] with AcceptableConferenceLoc[Unit] with ActiveConferenceLoc[Unit] {
    protected val PathList = "schedule_preferences" :: Nil
    protected def default = ()
    protected def conferenceAcceptable(conf: Conference) = conf.state == ConferenceState.Schedule

    def name = "SchedulePreferences"
    def text = new LinkText(ignore => Text(?("menu.schedule_preferences")))
  }

  val ViewConferenceLoc = new SinglePathLoc[Unit] with FinalResponseSinglePathLoc[Unit] with UnavailableConferenceLoc[Unit] with ActiveConferenceLoc[Unit] {
    protected val PathList = "conference" :: Nil
    protected def default = ()
    protected def conferenceAvailable(conf: Conference) = conf.conferenceAfterAcceptReject
    protected def unavailableKey = "papers.available_after_c4p"

    def name = "ViewConference"
    def text = new LinkText(ignore => Text(?("menu.view_conference")))
  }

  val AuthorsLoc = new SinglePathLoc[Unit] with FinalResponseSinglePathLoc[Unit] with UnavailableConferenceLoc[Unit] with ActiveConferenceLoc[Unit] {
    protected val PathList = "authors" :: Nil
    protected def default = ()
    protected def conferenceAvailable(conf: Conference) = conf.conferenceAfterAcceptReject
    protected def unavailableKey = "authors.available_after_c4p"

    def name = "ViewAuthors"
    def text = new LinkText(ignore => Text(?("menu.view_authors")))
  }

  val ViewScheduleLoc = new SinglePathLoc[Unit] with FinalResponseSinglePathLoc[Unit] with UnavailableConferenceLoc[Unit] with ActiveConferenceLoc[Unit] {
    protected val PathList = "schedule" :: Nil
    protected def default = ()
    protected def conferenceAvailable(conf: Conference) = conf.state == ConferenceState.Finalize
    protected def unavailableKey = "schedule.available_after_c4p"

    def name = "ViewSchedule"
    def text = new LinkText(ignore => Text(?("menu.schedule")))
  }
}