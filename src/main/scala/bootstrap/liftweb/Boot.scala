package bootstrap.liftweb

import xml.Text

import _root_.net.liftweb.util._
import _root_.net.liftweb.common._
import _root_.net.liftweb.http._
import S._
import _root_.net.liftweb.sitemap._
import _root_.net.liftweb.sitemap.Loc._
import _root_.net.liftweb.mapper.{DB, Schemifier, DefaultConnectionIdentifier, StandardDBVendor}
import _root_.pl.softwaremill.model._
import java.util.Locale
import pl.softwaremill.loc._
import LocTools._

/**
 * A class that's instantiated early and run.  It allows the application
 * to modify lift's environment
 */
class Boot {
  def boot {
    if (!DB.jndiJdbcConnAvailable_?)
      DB.defineConnectionManager(DefaultConnectionIdentifier,
        new StandardDBVendor(Props.get("db.driver") openOr "com.mysql.jdbc.Driver",
          Props.get("db.url") openOr "jdbc:mysql://localhost/conference?user=root",
          Props.get("db.user"), Props.get("db.password")))

    Schemifier.schemify(true, Log.infoF _, User, Conference, Room, Slot, Paper, UserInterested, Configuration)

    // where to search snippet
    LiftRules.addToPackages("pl.softwaremill")

    // Build SiteMap
    val entries =
            // Misc hidden
            Menu(Loc("Error", List("error"), "Error", Hidden)) ::
            // Home
            Menu(Loc("Home", List("index"), "Home")) ::
            Menu(Loc("Static", Link(List("static"), true, "/static/index"), "Static Content")) ::
            // Conferences
            conferencesMenu ::
            // View conference
            Menu(ViewConferenceLoc) ::
            // C4P
            c4pMenu ::
            // Schedule preferences
            Menu(SchedulePreferencesLoc) ::
            // View papers
            Menu(ViewPaperLoc) ::
            // User controls
            User.sitemap

    LiftRules.setSiteMap(SiteMap(entries:_*))

    /*
     * Show the spinny image when an Ajax call starts and make the spinny image go away when it ends.
     */
    LiftRules.ajaxStart = Full(() => LiftRules.jsArtifacts.show("ajax-loader").cmd)
    LiftRules.ajaxEnd = Full(() => LiftRules.jsArtifacts.hide("ajax-loader").cmd)

    LiftRules.early.append(_.setCharacterEncoding("UTF-8"))

    LiftRules.loggedInTest = Full(() => User.loggedIn_?)

    LiftRules.resourceNames = "conference" :: Nil

    LiftRules.localeCalculator = { req => currentUserLocale openOr LiftRules.defaultLocaleCalculator(req) }

    S.addAround(DB.buildLoanWrapper)
  }

  private def conferencesMenu: Menu = {
    val slotEditor = Menu(SlotEditorLoc)
    val acceptReject = Menu(AcceptRejectLoc)
    val main = Menu(Loc("Conferences", new Link("conferences" :: "index" :: Nil), ?("menu.conferences")), slotEditor, acceptReject)
    main
  }

  private def c4pMenu: Menu = {
    val editPaper = Menu(Loc("C4PEdit", "c4p" :: "edit" :: Nil, ?("menu.c4p.edit"), Hidden, User.loginFirst))
    val main = Menu(Loc("C4PList", "c4p" :: "index" :: Nil, ?("menu.c4p.my"), LocTools.showRequireLogin), editPaper)
    main
  }

  object ViewConferenceLoc extends ActiveConferenceLoc {
    protected val PathList = "conference" :: Nil
    protected def acceptConference(conf: Conference) = (conf.state == ConferenceState.Schedule || conf.state == ConferenceState.Finalize)

    def name = "ViewConference"
    def text = new LinkText(ignore => Text(?("menu.view_conference")))
  }

  object SchedulePreferencesLoc extends ActiveConferenceLoc {
    protected val PathList = "schedule_preferences" :: Nil
    protected def acceptConference(conf: Conference) = conf.state == ConferenceState.Schedule

    override def params = showRequireLogin :: super.params

    def name = "SchedulePreferences"
    def text = new LinkText(ignore => Text(?("menu.schedule_preferences")))
  }

  private def currentUserLocale: Box[Locale] = {
    User.currentUser.map { user: User =>
      user.locale.isAsLocale
    }
  }
}



