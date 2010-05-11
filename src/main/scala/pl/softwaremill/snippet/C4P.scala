package pl.softwaremill.snippet

import xml._

import net.liftweb.util.Helpers._
import net.liftweb.http._
import SHtml._
import S._

import pl.softwaremill.lib.D
import pl.softwaremill.model._
import pl.softwaremill.loc.ViewPaperLoc

import SnippetTools._
import pl.softwaremill.services.{ConferenceService, PaperService}

/**
 * @author Adam Warski (adam at warski dot org)
 */
class C4P {
  lazy val paperService = D.inject_![PaperService]

  def newLink(newLinkTemplate: NodeSeq): NodeSeq = {
    if (D.inject_![ConferenceService].hasConferencesInC4P) {
      link("edit.html", () => (), newLinkTemplate)
    } else {
      NodeSeq.Empty
    }
  }

  def edit(editTemplate: NodeSeq): NodeSeq = {
    val paper = CurrentPaper.is

    def checkAndSave {
      paper.user(User.currentUser.open_!)
      paper.validate match {
        case Nil  => {
          val savedBefore = paper.saved_?
          paper.save();
          S.notice(S.?(if (savedBefore) "paper.saved_existing" else "paper.saved_new", paper.title));
          redirectTo("index.html")
        }
        case xs   => xs.map { i => S.error(i.msg); }; CurrentPaper(paper)
      }
    }

    bind("paper", editTemplate,
      "title" -> paper.title.toForm,
      "conference" -> paper.conference.toForm,
      "shortDescription" -> paper.shortDescription.toForm,
      "save" -> submit(?("common.save"), () => checkAndSave),
      "cancel" -> redirectButton(?("common.cancel"), "index.html")
      )
  }

  def list(rowTemplate: NodeSeq): NodeSeq = {
    val papers = paperService.userPapers(User.currentUser.open_!)

    if (papers.size == 0) {
      Text(?("c4p.no_presentations"))
    } else {
      papers.flatMap { paper: Paper =>
        val conference = paper.conference.obj openOr (new Conference)
        val c4p = conference.state == ConferenceState.C4P
        bind("paper", rowTemplate,
          "title" -> paper.title,
          "conference" -> conference.name.is,
          "view" -> anchor(ViewPaperLoc.link.createPath(paper), ?("common.view")),
          "edit" -> (if (c4p) link("edit.html", () => CurrentPaper(paper), Text(?("common.edit"))) else NodeSeq.Empty),
          "delete" -> (if (c4p) confirmLink("", () => paper.delete_!, ?("common.delete"), ?("paper.confirm_delete", paper.title)) else NodeSeq.Empty)
          )
      }
    }
  }
}