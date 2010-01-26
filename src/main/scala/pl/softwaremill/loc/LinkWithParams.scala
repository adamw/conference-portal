package pl.softwaremill.loc

import net.liftweb.sitemap.Loc.Link
import net.liftweb.util.Helpers._

/**
 * @author Adam Warski (adam at warski dot org)
 */
trait LinkWithParams[T] extends Link[T] {
  override def createPath(value: T) = {
    val path = super.createPath(value)
    val paramsMap = params(value)
    val paramsString = paramsMap.map({ case (k, v) => urlEncode(k) + "=" + urlEncode(v) }).mkString("?", "&", "")
    path + paramsString
  }

  def params(value: T): Map[String, String]
}