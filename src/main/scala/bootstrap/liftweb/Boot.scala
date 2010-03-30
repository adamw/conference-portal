package bootstrap.liftweb

import _root_.net.liftweb.util._
import _root_.net.liftweb.common._
import _root_.net.liftweb.http._
import js.jquery.JQuery14Artifacts
import S._
import _root_.net.liftweb.sitemap._
import _root_.net.liftweb.sitemap.Loc._
import _root_.net.liftweb.mapper.{DB, Schemifier, DefaultConnectionIdentifier, StandardDBVendor}
import _root_.pl.softwaremill.model._
import pl.softwaremill.loc._
import Helpers._

import java.text.SimpleDateFormat
import java.util.{Date, Locale}
import pl.softwaremill.lib._
import pl.softwaremill.comet.{Update, TweetsUpdater}
import pl.softwaremill.services.{UpdateSpec, ExternalMarkupUpdater, FileService}

import javax.mail._

/**
 * A class that's instantiated early and run.  It allows the application
 * to modify lift's environment
 */
class Boot {
  def boot {
    if (!DB.jndiJdbcConnAvailable_?)
      DB.defineConnectionManager(DefaultConnectionIdentifier,
        new StandardDBVendor(Props.get("db.driver") openOr "",
          Props.get("db.url") openOr "",
          Props.get("db.user"), Props.get("db.password")))

    Schemifier.schemify(true, Log.infoF _, User, Conference, Room, Slot, Paper, UserInterested, Configuration,
      Registration, pl.softwaremill.model.MenuItem, File)

    // where to search snippet
    LiftRules.addToPackages("pl.softwaremill")

    LiftRules.dispatch.append(FileService.matcher)

    val entries =
            // Misc hidden
            Menu(Loc("Error", List("error"), "Error", Hidden)) ::
            // View CMS pages
            Menu(Locs.CmsLoc) ::
            // Home
            Menu(Loc("Home", List("index"), ?("menu.home"), Hidden)) ::
            // Tweets
            Menu(Locs.TweetsLoc) ::
            // Conferences management
            Menus.ManageMenu :: Nil :::
            // Conference view
            Menus.ConferenceMenus :::
            // User controls
            User.sitemap

    LiftRules.setSiteMap(SiteMap(entries:_*))

    /*
     * Show the spinny image when an Ajax call starts and make the spinny image go away when it ends.
     */
    LiftRules.ajaxStart = Full(() => LiftRules.jsArtifacts.show("ajax-loader").cmd)
    LiftRules.ajaxEnd = Full(() => LiftRules.jsArtifacts.hide("ajax-loader").cmd)

    LiftRules.early.append(_.setCharacterEncoding("UTF-8"))

    LiftRules.jsArtifacts = JQuery14Artifacts

    LiftRules.loggedInTest = Full(() => User.loggedIn_?)

    LiftRules.resourceNames = "conference" :: Nil

    val defaultLocale = new Locale("pl")
    Locale.setDefault(defaultLocale)
    LiftRules.localeCalculator = { req => currentUserLocale openOr defaultLocale }

    def dateFormat = new SimpleDateFormat("dd/MM/yyyy")
    LiftRules.formatDate = d => if (d == null) LiftRules.formatDate(new Date(0)) else dateFormat.format(d)
    LiftRules.parseDate = str => tryo(dateFormat.parse(str)) or Helpers.toDate(str)

    S.addAround(DB.buildLoanWrapper)

    // Starting tweet and markup updates
    TweetsUpdater ! Update()

    ExternalMarkupUpdater ! UpdateSpec("http://javarsovia.pl", ("id", "srodkowa_kolumna") :: ("id", "prawa_kolumna") :: Nil)

    LiftRules.unloadHooks.append(() => TweetsUpdater ! Shutdown())
    LiftRules.unloadHooks.append(() => ExternalMarkupUpdater ! Shutdown())

    configMailer(Props.get("mail.smtp.host") openOr "localhost",
      Props.get("mail.smtp.username") openOr "",
      Props.get("mail.smtp.password") openOr "")
  }

  private def currentUserLocale: Box[Locale] = {
    User.currentUser.map { user: User =>
      user.locale.isAsLocale
    }
  }

  private def configMailer(host: String, user: String, password: String) {
    System.setProperty("mail.smtp.starttls.enable","true");
    System.setProperty("mail.smtp.host", host)
    System.setProperty("mail.smtp.auth", "true")
    Mailer.authenticator = Full(new Authenticator {
      override def getPasswordAuthentication =
        new PasswordAuthentication(user, password)
    })
  }
}



