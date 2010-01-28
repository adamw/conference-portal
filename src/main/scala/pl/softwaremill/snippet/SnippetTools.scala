package pl.softwaremill.snippet

import xml._

import net.liftweb.util.Helpers._
import net.liftweb.http._
import net.liftweb.common._
import js.JE.JsRaw
import SHtml._

import net.liftweb.http.js.JsCmds._

/**
 * @author Adam Warski (adam at warski dot org)
 */
object SnippetTools {
  def anchor(link: String, text: String) = <a href={link}>{text}</a>

  def refreshButton(text: String) = <input type="button" value={text} /> % ("onclick" -> "location.reload(true)")

  def redirectButton(text: String, to: String) = <input type="button" value={text} /> % ("onclick" -> "window.location = '%s'".format(to))

  def confirmLink(to: String, func: () => Any, text: String, confirmText: String) = link(to, func, Text(text),
    "onclick" -> JsRaw("if (!confirm(" + confirmText.encJs + ")) return false;").cmd)
}