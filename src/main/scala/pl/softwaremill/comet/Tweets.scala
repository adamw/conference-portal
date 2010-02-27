package pl.softwaremill.comet

import io.Source

import net.liftweb.util.Helpers._
import net.liftweb.http._
import net.liftweb.common._
import net.liftweb.util.ActorPing
import net.liftweb.json._
import net.liftweb.actor._

import util.parsing.json.JSON

import js.JsCmds._
import js.JE._

import pl.softwaremill.lib._

/**
 * @author Adam Warski (adam at warski dot org)
 */
object Tweets

class TweetsClient extends CometActor {
  implicit private val formats = DefaultFormats

  def render =
    <span>
      <div id="tweets"></div>
    </span>

  override def lowPriority : PartialFunction[Any, Unit] = {
    case UpdateTweets(newTweets) => {
      partialUpdate(Call("newMessages", JsRaw(Serialization.write(newTweets))))
    }
  }

  override def localSetup() {
    TweetsMaster ! Subscribe(this)
    super.localSetup()
  }

  override def localShutdown() {
    TweetsMaster ! Unsubscribe(this)
    super.localShutdown()
  }

  override def lifespan: Box[TimeSpan] = Full(30 seconds)
}

object TweetsMaster extends LiftActor {
  var clients: List[TweetsClient] = Nil
  var currentTweets: List[Tweet] = Nil

  protected def messageHandler = {
    case Subscribe(t) => {
      println("sub")
      clients ::= t
      t ! UpdateTweets(currentTweets)
    }

    case Unsubscribe(t) => clients -= t

    case UpdateTweets(newTweets) => {
      // Sending the tweets delta to clients, if there are any
      if (clients != Nil) {
        val delta = newTweets -- currentTweets

        if (delta != Nil) clients map { _ ! UpdateTweets(delta) }
      }

      currentTweets = newTweets
    }
  }
}

object TweetsUpdater extends LiftActor {
  var scheduleUpdates = true

  protected def messageHandler = {
    case Update() => {
      TweetsMaster ! UpdateTweets(readTweets)
      if (scheduleUpdates) { ActorPing.schedule(this, Update(), 20 seconds) }
    }
    
    case Shutdown() => scheduleUpdates = false
  }

  def readTweets = {
    def toString(o: Any) = if (o == null) null else o.toString

    val source = Source.fromURL("http://search.twitter.com/search.json?q=obama&rpp=10", "UTF-8")
    val json = source.getLines.mkString
    val parsed = JSON.parseFull(json).asInstanceOf[Option[Map[Any, Any]]] getOrElse Map[Any, Any]()

    val results = parsed.getOrElse("results", List[Any]()).asInstanceOf[List[List[Any]]]

    val tweets = results.map { result =>
      val resultMap = result.foldLeft(Map[String, String]())((acc, el) => el match {
        case (key, value) => acc + (toString(key) -> toString(value))
        case _ => acc
      })

      Tweet(resultMap.getOrElse("source", ""),
        resultMap.getOrElse("text", ""),
        resultMap.getOrElse("from_user", ""),
        resultMap.getOrElse("profile_image_url", ""))
    }

    tweets
  }
}

// Used by the updater
case class Update()

// Used by the clients and the master
case class UpdateTweets(newTweets: List[Tweet])
case class Subscribe(t: TweetsClient)
case class Unsubscribe(t: TweetsClient)

// Data class
case class Tweet(link: String, message: String, author: String, profileImageUrl: String)

