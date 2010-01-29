package pl.softwaremill.snippet

import xml._

import net.liftweb.util.Helpers._
import net.liftweb.http._
import net.liftweb.common._
import SHtml._
import S._

import pl.softwaremill.lib.D
import pl.softwaremill.model._
import pl.softwaremill.services.PaperService
import pl.softwaremill.loc.ViewPaperLoc

import SnippetTools._
import PaperSnippetTools._

/**
 * @author Adam Warski (adam at warski dot org)
 */
class AcceptRejectPapers {
  lazy val paperService = D.inject_![PaperService]

  def selectionsLeft(ignore: NodeSeq) = SelectionConfig.selectionsLeftNode

  private var acceptedPapers: Set[Paper] = _
  initAcceptedPapers()

  private def initAcceptedPapers() = { acceptedPapers = Set() ++ paperService.acceptedConferencePapers(CurrentConference.is) }

  object SelectionConfig extends PaperListWithMaxSelectionConfig {
    val maxSelections = CurrentConference.is.slots.size
    val papers = paperService.conferencePapers(CurrentConference.is)
    val deselectLinkText = ?("paper.reject")
    val selectLinkText = ?("paper.accept")

    def isSelected(paper: Paper) = acceptedPapers.contains(paper)

    def select(paper: Paper, select: Boolean) = {
      paper.status(if (select) PaperStatus.Accepted else PaperStatus.Rejected)
      paper.save
      initAcceptedPapers;
    }

    def selectedCount = acceptedPapers.size

    def selectionsLeftText(left: Int): String = ?("acceptreject.selections_left", left, maxSelections)

    def bindCells(paper: Paper, cellsTemplate: NodeSeq): NodeSeq = {
      bind("cell", cellsTemplate,
        "title" -> paper.title,
        "author" -> paper.author,
        "status" -> ?(paper.status.toString),
        "desc" -> paper.shortDescription.toHtml,
        "view" -> anchor(ViewPaperLoc.link.createPath(paper), ?("common.view"))
        )
    }
  }

  def list(listTemplate: NodeSeq): NodeSeq = {
    paperListWithMaxSelection(listTemplate, SelectionConfig)
  }
}