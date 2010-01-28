package pl.softwaremill.snippet

import xml._

import net.liftweb.util.Helpers._
import net.liftweb.http._
import js.JsCmds.SetHtml
import net.liftweb.common._
import SHtml._
import S._

import pl.softwaremill.lib.D
import pl.softwaremill.model._
import pl.softwaremill.services.PaperService

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

  def list(rowTemplate: NodeSeq): NodeSeq = {
    def reDrawSelectionsLeft = SetHtml("selections_left", selectionsLeft(NodeSeq.Empty))
    val papers = paperService.acceptedConferencePapers(CurrentConference.is)

    paperListWithSingleRowRerender(rowTemplate, papers, Full(reDrawSelectionsLeft _), (paper, cellsTemplate, reDraw) => {
      val interested = interestedInPapers.contains(paper)

      bind("cell", cellsTemplate,
          "title" -> paper.title,
          "author" -> paper.author,
          "desc" -> paper.shortDescription,
          "select" -> a(() => {
            paperService.updateUserInterestedInPaper(user, paper, !interested);
            initInterestedInPapers;
            reDraw() },
            Text(?(if (interested) "schedule.deselect" else "schedule.select")))
          )
    })
  }
}