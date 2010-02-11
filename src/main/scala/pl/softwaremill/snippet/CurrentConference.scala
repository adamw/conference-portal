package pl.softwaremill.snippet

import xml.{Unparsed, Text, NodeSeq}

import net.liftweb.util.Helpers._
import net.liftweb.http._
import S._

import pl.softwaremill.model.{MenuItem, User, Paper, Conference}
import pl.softwaremill.lib.D
import pl.softwaremill.services.{UserService, PaperService}
import pl.softwaremill.loc.{Locs, ViewPaperLoc}

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

  def acceptedAuthors(authorTemplate: NodeSeq): NodeSeq = {
    val authors = D.inject_![UserService].acceptedAuthors(CurrentConference.is)

    authors.flatMap(author => bind("author", authorTemplate,
      "name" -> author.shortName,
      "view" -> anchor(Locs.AuthorLoc.link.createPath(author), ?("common.view"))
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

object CurrentAuthor extends RequestVar(User.create)

class CurrentAuthor {
  def name(ignore: NodeSeq): NodeSeq = Text(CurrentAuthor.is.shortName)

  def bio(ignore: NodeSeq): NodeSeq = CurrentAuthor.is.bio.toHtml

  def acceptedPapers(paperTemplate: NodeSeq): NodeSeq = {
    val papers = D.inject_![PaperService].acceptedConferencePapers(CurrentConference.is, CurrentAuthor.is)

    papers.flatMap(paper => bind("paper", paperTemplate,
      "title" -> paper.title,
      "view" -> anchor(ViewPaperLoc.link.createPath(paper), ?("common.view"))
      ))
  }
}

object CurrentMenuItemPage extends RequestVar(MenuItem.create)

class CurrentMenuItemPage {
  def title(ignore: NodeSeq): NodeSeq = Text(CurrentMenuItemPage.is.title)

  def content(ignore: NodeSeq): NodeSeq = Unparsed(CurrentMenuItemPage.is.pageContent)
}