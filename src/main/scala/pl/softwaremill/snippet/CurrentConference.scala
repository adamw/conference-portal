package pl.softwaremill.snippet

import xml.{Text, NodeSeq}

import net.liftweb.util.Helpers._
import net.liftweb.http._
import S._

import pl.softwaremill.model.{Paper, Conference}
import pl.softwaremill.lib.D
import pl.softwaremill.services.PaperService
import pl.softwaremill.loc.ViewPaperLoc

import SnippetTools._

/**
 * @author Adam Warski (adam at warski dot org)
 */
object CurrentConference extends RequestVar(Conference.create)

class CurrentConference {
  def name(ignore: NodeSeq): NodeSeq = Text(CurrentConference.is.name)

  def acceptedPapers(paperTemplate: NodeSeq): NodeSeq = {
    val papers = D.inject_![PaperService].acceptedConferencePapers(CurrentConference.is)

    papers.flatMap(paper => bind("paper", paperTemplate,
      "title" -> paper.title,
      "author" -> paper.author,
      "view" -> anchor(ViewPaperLoc.link.createPath(paper), ?("common.view"))
      ))
  }
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
      "shortDescription" -> paper.shortDescription.toHtml,
      "author" -> paper.author
      )
  }
}