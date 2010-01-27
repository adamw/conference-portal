package pl.softwaremill.snippet

import xml._

import net.liftweb.util.Helpers._
import net.liftweb.http._
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
class AcceptRejectPapers {
  lazy val paperService = D.inject_![PaperService]
  
  def list(rowTemplate: NodeSeq): NodeSeq = {
    val papers = paperService.conferencePapers(CurrentConference.is)
    papers.flatMap { paper: Paper =>
      bind("paper", rowTemplate,
        "title" -> paper.title,
        "desc" -> paper.shortDescription,
        "accept" -> NodeSeq.Empty,
        "reject" -> NodeSeq.Empty
        )
    }
  }
}