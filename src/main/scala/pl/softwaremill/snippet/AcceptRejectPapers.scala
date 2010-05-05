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
import pl.softwaremill.loc.Locs.AuthorLoc

import SnippetTools._
import PaperSnippetTools._

/**
 * @author Adam Warski (adam at warski dot org)
 */
// TODO: currently quite ugly, as this is also a method of viewing all presentations, not only accepting/ rejecting them
class AcceptRejectPapers {
  lazy val paperService = D.inject_![PaperService]

  val selectionConfig = if (CurrentConference.is.state == ConferenceState.Accept) AcceptRejectSelectionConfig else ViewSelectionConfig

  def selectionsLeft(ignore: NodeSeq) = selectionConfig.selectionsLeftNode

  private var acceptedPapers: Set[Paper] = _
  initAcceptedPapers()

  private def initAcceptedPapers() = { acceptedPapers = Set() ++ paperService.acceptedConferencePapers(CurrentConference.is) }

  abstract class BaseAcceptRejectSelectionConfig extends PaperListWithMaxSelectionConfig {
    val papers = paperService.conferencePapers(CurrentConference.is)
    val deselectLinkText = ?("paper.reject")
    val selectLinkText = ?("paper.accept")

    def select(paper: Paper, select: Boolean) = {
      paper.status(if (select) PaperStatus.Accepted else PaperStatus.Rejected)
      paper.save
      initAcceptedPapers;
    }

    def selectionsLeftText(left: Int): String = ?("acceptreject.selections_left", left, maxSelections)

    def bindCells(paper: Paper, cellsTemplate: NodeSeq): NodeSeq = {
      bind("cell", cellsTemplate,
        "title" -> paper.title,
        "author" -> anchor(AuthorLoc.link.createPath(paper.user.obj.open_!), paper.author),
        "status" -> ?(paper.status.toString),
        "desc" -> paper.shortDescription.toHtml,
        "view" -> anchor(ViewPaperLoc.link.createPath(paper), ?("common.view"))
        )
    }
  }

  object AcceptRejectSelectionConfig extends BaseAcceptRejectSelectionConfig {
    val maxSelections = CurrentConference.is.slots.size
    def isSelected(paper: Paper) = acceptedPapers.contains(paper)
    def selectedCount = acceptedPapers.size
  }

  object ViewSelectionConfig extends BaseAcceptRejectSelectionConfig {
    val maxSelections = 0
    def isSelected(paper: Paper) = false
    def selectedCount = 0
  }

  def list(listTemplate: NodeSeq): NodeSeq = {
    paperListWithMaxSelection(listTemplate, selectionConfig)
  }
}