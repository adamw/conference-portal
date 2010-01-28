package pl.softwaremill.snippet

import xml._

import net.liftweb.util.Helpers._
import net.liftweb.http._
import js.JsCmds.{CmdPair, SetHtml}
import net.liftweb.common._
import SHtml._
import S._

import pl.softwaremill.lib.D
import pl.softwaremill.model._
import pl.softwaremill.services.PaperService

import pl.softwaremill.loc.ViewPaperLoc

import SnippetTools._

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

  // The maximum number of selections is the number of slot spans for a conference
  private val maxSelections = CurrentConference.is.slotsBySpans.size

  def selectionsLeft(ignore: NodeSeq) = {
    val left = maxSelections - interestedInPapers.size
    Text(?("schedule.selections_left", left, maxSelections))
  }

  def list(listTemplate: NodeSeq): NodeSeq = {
    def reDrawSelectionsLeft = SetHtml("selections_left", selectionsLeft(NodeSeq.Empty))
    val papers = paperService.acceptedConferencePapers(CurrentConference.is)

    def rows(rowTemplate: NodeSeq): NodeSeq = paperListWithSingleRowRerender(rowTemplate, papers, Full(reDrawSelectionsLeft _),
      (paper, cellsTemplate, reDraw) => {
        val interested = interestedInPapers.contains(paper)
        def selectionBlocked = maxSelections == interestedInPapers.size

        def selectDeselectLink = a(() => {
          val selectionBlockedBefore = selectionBlocked
          paperService.updateUserInterestedInPaper(user, paper, !interested);
          initInterestedInPapers;
          val selectionBlockedAfter = selectionBlocked

          // Doing a full-redraw if the selection has been blocked or unblocked to hide/show all select links
          if (selectionBlockedBefore != selectionBlockedAfter) {
            CmdPair(reDrawSelectionsLeft, SetHtml("paper_list", list(listTemplate)))
          } else reDraw() },
          Text(?(if (interested) "schedule.deselect" else "schedule.select")))

        bind("cell", cellsTemplate,
          "title" -> paper.title,
          "author" -> paper.author,
          "desc" -> paper.shortDescription,
          "view" -> anchor(ViewPaperLoc.link.createPath(paper), ?("common.view")),
          "select" -> (if (selectionBlocked && !interested) NodeSeq.Empty else selectDeselectLink)
          )
      })

    bind("list", listTemplate, "row" -> rows _)
  }
}