package pl.softwaremill.loc

import net.liftweb.http._
import net.liftweb.common._
import net.liftweb.sitemap.Loc._

import pl.softwaremill.model.{Conference, ConferenceState, Configuration, User}
import pl.softwaremill.snippet.CurrentConference

/**
 * @author Adam Warski (adam at warski dot org)
 */
object LocTools {
  def finalResponse(parsePath: ParsePath) = new RewriteResponse(parsePath, Map.empty, true)
  def finalResponse(path: List[String]): RewriteResponse = finalResponse(ParsePath(path, "", true, false))

  /**
   * A LocParam that shows the menu item but requires the user to login before accessing the loc.
   */
  def showRequireLogin = EarlyResponse(() => Full(User.loginFirst.failMsg()).filter(ignore => !User.loggedIn_?))

  /**
   * An If LocParams that shows the menu item only if the active conference satisfies the given predicate.
   */
  def showIfActiveConferenceSatisfies(predicate: Conference => Boolean) = If(() => {
    Configuration.is.activeConference match {
      case Full(conf) => predicate(conf)
      case _ => false
    }
  }, () => RedirectResponse("/"))

  def withActiveConference[T](default: => T, acceptConference: Conference => Boolean)(block: => (RewriteResponse, T)): (RewriteResponse, T)   = {
    Configuration.is.activeConference match {
      case Full(conf) if acceptConference(conf) => {
        CurrentConference(conf)
        block
      }
      case _ => (RewriteResponse("error" :: Nil), default)
    }
  }

  def conferenceAfterAcceptReject(conf: Conference) = (conf.state == ConferenceState.Schedule || conf.state == ConferenceState.Finalize)
}