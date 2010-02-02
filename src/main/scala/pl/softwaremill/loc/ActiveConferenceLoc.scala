package pl.softwaremill.loc

import net.liftweb.sitemap.Loc
import Loc._
import net.liftweb.common._
import net.liftweb.http._

import LocTools._
import pl.softwaremill.model.Conference

/**
 * A location where the current conference is set basing on the active conference.
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
      withActiveConference((), acceptConference _) {
        (finalResponse(parsePath), ())
      }
    }
  })
}