package pl.softwaremill.snippet

import xml.{Text, NodeSeq}

import net.liftweb.util.Helpers._
import net.liftweb.http.RequestVar

import pl.softwaremill.model.{Paper, Conference}

/**
 * @author Adam Warski (adam at warski dot org)
 */
object CurrentConference extends RequestVar(Conference.create)

class CurrentConference {
  def name(ignore: NodeSeq): NodeSeq = Text(CurrentConference.is.name)
}

object CurrentPaper extends RequestVar(Paper.create)

class CurrentPaper {
  def title(default: NodeSeq): NodeSeq = {
    val paper = CurrentPaper.is
    if (paper.saved_?) Text(CurrentPaper.is.title) else default
  }

  def view(viewTemplate: NodeSeq): NodeSeq = {
    val paper = CurrentPaper.is
    bind("paper", viewTemplate,
      "title" -> paper.title,
      "shortDescription" -> paper.shortDescription,
      "author" -> "???"
      )
  }
}