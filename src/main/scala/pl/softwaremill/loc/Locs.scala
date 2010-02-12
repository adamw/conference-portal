package pl.softwaremill.loc

import xml._

import net.liftweb.sitemap.{Menu, Loc}
import net.liftweb.common._
import net.liftweb.http._
import net.liftweb.util.Helpers._

import Box._
import Loc._
import S._

import pl.softwaremill.model._
import pl.softwaremill.lib.D
import pl.softwaremill.services._
import pl.softwaremill.snippet.{CurrentAuthor, CurrentConference, CurrentMenuItemPage, Util}

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

  trait RequiresLoginLoc[T] extends Loc[T] {
    override abstract def params = LocTools.showRequireLogin :: super.params
  }

  /**
   * Serves all paths with the path equals to {@code PathList} (but not sub-paths).
   */
  trait SinglePathLoc[T] extends PrefixLoc[T] {
    def link = new Link(PathList)

    def params: List[Loc.LocParam[T]] = Nil

    override def rewrite = Full({
      case request @ RewriteRequest(ParsePath(PathList, _, _, _), _, _) => doRewrite(request)
    })
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

  val SchedulePreferencesLoc = new SinglePathLoc[Unit] with FinalResponseSinglePathLoc[Unit] with AcceptableConferenceLoc[Unit] with ActiveConferenceLoc[Unit]
          with RequiresLoginLoc[Unit] {
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

  val RegisterLoc = new SinglePathLoc[Unit] with FinalResponseSinglePathLoc[Unit] with AcceptableConferenceLoc[Unit] with ActiveConferenceLoc[Unit]
          with RequiresLoginLoc[Unit] {
    protected val PathList = "register" :: Nil
    protected def default = ()
    protected def conferenceAcceptable(conf: Conference) = conf.state != ConferenceState.Prepare

    def name = "Register"
    def text = new LinkText(ignore => Text(?("menu.register")))
  }

  val TweetsLoc = new SinglePathLoc[Unit] with FinalResponseSinglePathLoc[Unit] with ActiveConferenceLoc[Unit] {
    protected val PathList = "tweets" :: Nil
    protected def default = ()

    def name = "Tweets"
    def text = new LinkText(ignore => Text(?("menu.tweets")))
  }

  class CmsLocBase extends PrefixLoc[MenuItem] {
    private val ContentPath = "content"

    protected val PathList = ContentPath :: Nil
    protected def default: MenuItem = null

    def name = "Content"
    def text = new LinkText(menuItem => Text(menuItem.title.is))

    override def params: List[Loc.LocParam[MenuItem]] = Hidden :: Nil

    override def link = new Link[MenuItem](PathList) {
      override def pathList(menuItem: MenuItem): List[String] = super.pathList(menuItem) ++ MenuItemPath(menuItem)
    }

    protected def doRewrite(request: RewriteRequest): (RewriteResponse, MenuItem) = {
      request match {
        case RewriteRequest(parsePath @ ParsePath(ContentPath :: rest, _, _, _), _, httpRequest) => {
          val rootMenuItem = CurrentConference.is.mainMenuItem.obj.open_!

          (rootMenuItem, rest) match {
            case MenuItemPath(menuItem) => { CurrentMenuItemPage(menuItem); (finalResponse(ContentPath :: Nil), menuItem) }
            case _ => (RewriteResponse("error" :: Nil, Map(errorMessageParam -> ?("menuitem.unknown"))), null)
          }
        }
        case _ => (RewriteResponse("error" :: Nil, Map(errorMessageParam -> ?("menuitem.unknown"))), null)
      }
    }

    override def rewrite = Full({
      case request @ RewriteRequest(ParsePath(ContentPath :: _, _, _, _), _, _) => {
        doRewrite(request)
      }
    })
  }

  val CmsLoc = new CmsLocBase with ActiveConferenceLoc[MenuItem]

  val ManageLoc = Loc("Conferences", new Link("conferences" :: "index" :: Nil), ?("menu.conferences"), User.testSuperUser)
}

object Menus {
  import Locs._

  val ConferenceMenus =
          // View Authors
          Menu(AuthorsLoc) ::
          // View conference
          Menu(ViewConferenceLoc) ::
          // Schedule
          Menu(ViewScheduleLoc) ::
          // Register
          Menu(RegisterLoc) ::
          // C4P
          c4pMenu ::
          // Schedule preferences
          Menu(SchedulePreferencesLoc) ::
          // View papers
          Menu(ViewPaperLoc) ::
          // View author
          Menu(AuthorLoc) :: Nil

  private def c4pMenu: Menu = {
    val editPaper = Menu(Loc("C4PEdit", "c4p" :: "edit" :: Nil, ?("menu.c4p.edit"), Hidden, User.loginFirst))
    val main = Menu(Loc("C4PList", "c4p" :: "index" :: Nil, ?("menu.c4p.my"), LocTools.showRequireLogin), editPaper)
    main
  }

  val ManageMenu = manageMenu

  private def manageMenu: Menu = {
    val slotEditor = Menu(SlotEditorLoc)
    val acceptReject = Menu(AcceptRejectLoc)
    val stats = Menu(StatisticsLoc)
    val cmsAdmin = Menu(CmsAdminLoc)
    val main = Menu(ManageLoc, slotEditor, acceptReject, stats, cmsAdmin)
    main
  }
}