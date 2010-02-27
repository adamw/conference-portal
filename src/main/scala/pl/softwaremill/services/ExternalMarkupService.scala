package pl.softwaremill.services

import xml._
import xml.parsing._

import io.Source

import net.liftweb.util.Helpers._

import net.liftweb.actor.LiftActor
import net.liftweb.util.ActorPing

import pl.softwaremill.lib._
import java.util.concurrent.ConcurrentHashMap

/**
 * @author Adam Warski (adam at warski dot org)
 */
object ExternalMarkupService {
  val markup = new ConcurrentHashMap[(String, String, String), NodeSeq]

  def apply(url: String, attr: String, value: String) = markup.get((url, attr, value))

  def apply(url: String, attr: String, value: String, node: NodeSeq) = markup.put((url, attr, value), node)
}

object ExternalMarkupUpdater extends LiftActor {
  def readURL(url: String) = XhtmlParser(Source.fromURL(url, "UTF-8"))

  def findNodeWithAttrValue(node: NodeSeq, attr: String, value: String): NodeSeq =
    (node \\ "_").filter(_.attribute(attr).filter(_ == value).isDefined)

  var scheduleUpdates = true

  protected def messageHandler = {
    case u @ UpdateSpec(url, attrNameValues) => {
      try {
        val content = readURL(url)
        attrNameValues.foreach({ case (name, value) =>
          ExternalMarkupService(url, name, value, findNodeWithAttrValue(content, name, value)) })
      } catch {
        case e => e.printStackTrace() 
      }

      if (scheduleUpdates) { ActorPing.schedule(this, u, 10 minutes) }
    }

    case Shutdown() => scheduleUpdates = false
  }
}

case class UpdateSpec(url: String, attrNameValues: List[(String, String)])