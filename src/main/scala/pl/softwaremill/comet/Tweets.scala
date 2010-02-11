package pl.softwaremill.comet

import xml._

import net.liftweb.util.Helpers._
import net.liftweb.http._
import net.liftweb.common._
import net.liftweb.util.ActorPing
import net.liftweb.http.js.JsCmds.SetHtml
import net.liftweb.actor._

/**
 * @author Adam Warski (adam at warski dot org)
 */
object Tweets

class TweetsClient extends CometActor {
  def render = <span id="tweets">Waiting for tweets to arrive ...</span>

  override def lowPriority : PartialFunction[Any, Unit] = {
    case UpdateTweets(newTweets) => {
      println("New tweets " + newTweets)
      partialUpdate(SetHtml("tweets", Text(timeNow.toString)))
    }
  }

  override def localSetup() {
    TweetsMaster ! Subscribe(this)
    super.localSetup()
  }

  override def localShutdown() {
    println("!!!")
    println("!!!")
    println("UNSUB")
    println("!!!")
    println("!!!")
    TweetsMaster ! Unsubscribe(this)
    super.localShutdown()
  }

  override def lifespan: Box[TimeSpan] = Full(2 minutes)
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

        println("update " + delta)

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
      if (scheduleUpdates) { ActorPing.schedule(this, Update(), 10000L) }
    }
    
    case Shutdown() => scheduleUpdates = false
  }

  var testCount = 3;
  def readTweets = { println("read tweets"); testCount += 1; (1 to testCount).map(i => Tweet("i"+i, "m"+i, "a"+i)).toList.takeRight(10) }
}

// Used by the updater
case class Update()
case class Shutdown()

// Used by the clients and the master
case class UpdateTweets(newTweets: List[Tweet])
case class Subscribe(t: TweetsClient)
case class Unsubscribe(t: TweetsClient)

// Data class
case class Tweet(link: String, message: String, author: String)

