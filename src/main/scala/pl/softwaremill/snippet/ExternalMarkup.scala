package pl.softwaremill.snippet

import xml.NodeSeq

import net.liftweb.http.S
import net.liftweb.common.Box

import pl.softwaremill.services.ExternalMarkupService

/**
 * @author Adam Warski (adam at warski dot org)
 */
class ExternalMarkup {
  def render(ignore: NodeSeq): NodeSeq = {
    (for (url <- S.attr("url");
         attr <- S.attr("attr");
         value <- S.attr("value")) yield ExternalMarkupService(url, attr, value)) openOr NodeSeq.Empty
  }
}