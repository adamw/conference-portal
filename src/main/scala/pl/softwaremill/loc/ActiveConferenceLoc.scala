package pl.softwaremill.loc

import net.liftweb.sitemap.Loc
import Loc._
import net.liftweb.common._
import net.liftweb.http._

import pl.softwaremill.snippet.CurrentConference
import LocTools._
import pl.softwaremill.model.{Conference, Configuration}

/**
 * A location where the current conference is set basing on the active conference. Requires login.
 * @author Adam Warski (adam at warski dot org)
 */
trait ActiveConferenceLoc extends Loc[Unit] {
  protected val PathList: List[String]

  /**
   * @return Should the location be available for the given conference
   */
  protected def acceptConference(conf: Conference): Boolean

  def params: List[Loc.LocParam[Unit]] = List(showIfActiveConferenceSatisfies(acceptConference _))

  def link = new Link(PathList)

  def defaultValue = Full(())

  override def rewrite = Full({
    case RewriteRequest(parsePath @ ParsePath(PathList, _, _, _), _, _) => {
      Configuration.is.activeConference match {
        case Full(conf) if acceptConference(conf) => {
          CurrentConference(conf)
          (finalResponse(parsePath), ())
        }
        case _ => (RewriteResponse("error" :: Nil), ())
      }
    }
  })
}