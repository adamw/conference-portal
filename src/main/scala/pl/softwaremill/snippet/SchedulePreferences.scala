package pl.softwaremill.snippet

import xml._

import net.liftweb.util.Helpers._
import net.liftweb.http._
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
class SchedulePreferences {
  private lazy val paperService = D.inject_![PaperService]
  private val user = User.currentUser.open_!

  // Keeping this in a set to quickly check if a paper is interesting
  private var interestedInPapers: Set[Paper] = _
  initInterestedInPapers()

  private def initInterestedInPapers() = { interestedInPapers = Set() ++ paperService.interestingPapersForUser(user) }

  def selectionsLeft(ignore: NodeSeq) = SelectionConfig.selectionsLeftNode

  object SelectionConfig extends PaperListWithMaxSelectionConfig {
    val maxSelections = CurrentConference.is.slotsBySpans.size
    val papers = paperService.acceptedConferencePapers(CurrentConference.is)
    val deselectLinkText = ?("schedule.deselect")
    val selectLinkText = ?("schedule.select")

    def isSelected(paper: Paper) = interestedInPapers.contains(paper)

    def select(paper: Paper, select: Boolean) = {
      paperService.updateUserInterestedInPaper(user, paper, select);
      initInterestedInPapers;
    }

    def selectedCount = interestedInPapers.size

    def selectionsLeftText(left: Int): String = ?("schedule.selections_left", left, maxSelections)

    def bindCells(paper: Paper, cellsTemplate: NodeSeq): NodeSeq = {
      bind("cell", cellsTemplate,
        "title" -> paper.title,
        "author" -> paper.author,
        "desc" -> Text(paper.shortDescription),
        "view" -> anchor(ViewPaperLoc.link.createPath(paper), ?("common.view"))
        )
    }
  }

  def list(listTemplate: NodeSeq): NodeSeq = {
    paperListWithMaxSelection(listTemplate, SelectionConfig)
  }
}