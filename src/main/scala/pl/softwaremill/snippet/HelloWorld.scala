package pl.softwaremill.snippet

import _root_.scala.xml.{NodeSeq, Text}
import _root_.net.liftweb.util._
import _root_.net.liftweb.common._
import _root_.java.util.Date
import pl.softwaremill.lib._
import Helpers._

class HelloWorld {
  lazy val date: Box[Date] = D.inject[Date] // inject the date

  def howdy(in: NodeSeq): NodeSeq =
    Helpers.bind("b", in, "time" -> date.map(d => Text(d.toString)))

  /*
   lazy val date: Date = D.time.vend // create the date via factory

   def howdy(in: NodeSeq): NodeSeq = Helpers.bind("b", in, "time" -> date.toString)
   */
}


