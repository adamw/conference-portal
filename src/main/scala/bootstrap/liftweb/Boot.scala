package bootstrap.liftweb

import _root_.net.liftweb.util._
import _root_.net.liftweb.common._
import _root_.net.liftweb.http._
import S._
import _root_.net.liftweb.sitemap._
import _root_.net.liftweb.sitemap.Loc._
import _root_.net.liftweb.mapper.{DB, Schemifier, DefaultConnectionIdentifier, StandardDBVendor}
import _root_.pl.softwaremill.model._
import pl.softwaremill.loc._
import Helpers._

import java.text.SimpleDateFormat
import java.util.{Date, Locale}
import pl.softwaremill.comet.{TweetsUpdater, Shutdown, Update}

/**
 * A class that's instantiated early and run.  It allows the application
 * to modify lift's environment
 */
class Boot {
  def boot {
    if (!DB.jndiJdbcConnAvailable_?)
      DB.defineConnectionManager(DefaultConnectionIdentifier,
        new StandardDBVendor(Props.get("db.driver") openOr "com.mysql.jdbc.Driver",
          Props.get("db.url") openOr "jdbc:mysql://localhost/conference?user=root&useUnicode=true&characterEncoding=UTF-8",
          Props.get("db.user"), Props.get("db.password")))

    Schemifier.schemify(true, Log.infoF _, User, Conference, Room, Slot, Paper, UserInterested, Configuration,
      Registration, pl.softwaremill.model.MenuItem)

    // where to search snippet
    LiftRules.addToPackages("pl.softwaremill")

    // Build SiteMap
    val entries =
            // Misc hidden
            Menu(Loc("Error", List("error"), "Error", Hidden)) ::
            // View papers
            Menu(ViewPaperLoc) ::
            // View author
            Menu(Locs.AuthorLoc) ::
            // View CMS pages
            Menu(Locs.CmsLoc) ::
            // Home
            Menu(Loc("Home", List("index"), ?("menu.home"), Hidden)) ::
            // View Authors
            Menu(Locs.AuthorsLoc) ::
            // View conference
            Menu(Locs.ViewConferenceLoc) ::
            // Schedule
            Menu(Locs.ViewScheduleLoc) ::
            // Register
            Menu(Locs.RegisterLoc) ::
            // C4P
            c4pMenu ::
            // Schedule preferences
            Menu(Locs.SchedulePreferencesLoc) ::
            // Tweets
            //Menu(Locs.TweetsLoc) ::
            // Conferences management
            conferencesMenu ::
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

    val defaultLocale = new Locale("pl")
    Locale.setDefault(defaultLocale)
    LiftRules.localeCalculator = { req => currentUserLocale openOr defaultLocale }

    def dateFormat = new SimpleDateFormat("dd/MM/yyyy")
    LiftRules.formatDate = d => if (d == null) LiftRules.formatDate(new Date(0)) else dateFormat.format(d)
    LiftRules.parseDate = str => tryo(dateFormat.parse(str)) or Helpers.toDate(str)

    S.addAround(DB.buildLoanWrapper)

    // Starting tweet updates
    //TweetsUpdater ! Update()

    //LiftRules.unloadHooks.append(() => TweetsUpdater ! Shutdown())
  }

  private def conferencesMenu: Menu = {
    val slotEditor = Menu(SlotEditorLoc)
    val acceptReject = Menu(AcceptRejectLoc)
    val stats = Menu(StatisticsLoc)
    val cmsAdmin = Menu(CmsAdminLoc)
    val main = Menu(Loc("Conferences", new Link("conferences" :: "index" :: Nil), ?("menu.conferences")/*, User.testSuperUser*/),
      slotEditor, acceptReject, stats, cmsAdmin)
    main
  }

  private def c4pMenu: Menu = {
    val editPaper = Menu(Loc("C4PEdit", "c4p" :: "edit" :: Nil, ?("menu.c4p.edit"), Hidden, User.loginFirst))
    val main = Menu(Loc("C4PList", "c4p" :: "index" :: Nil, ?("menu.c4p.my"), LocTools.showRequireLogin), editPaper)
    main
  }

  private def currentUserLocale: Box[Locale] = {
    User.currentUser.map { user: User =>
      user.locale.isAsLocale
    }
  }
}



