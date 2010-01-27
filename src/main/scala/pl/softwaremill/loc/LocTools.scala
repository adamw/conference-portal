package pl.softwaremill.loc

import net.liftweb.http._
import net.liftweb.sitemap.SiteMap

/**
 * @author Adam Warski (adam at warski dot org)
 */
object LocTools {
  def finalResponse(parsePath: ParsePath) = new RewriteResponse(parsePath, Map.empty, true)
  def finalResponse(path: List[String]): RewriteResponse = finalResponse(ParsePath(path, "", true, false))
  def linkForLoc(locName: String) = {
    (for {
      loc <- SiteMap.findLoc(locName)
      link <- loc.createDefaultLink } yield link.text) openOr "/"
  }
}