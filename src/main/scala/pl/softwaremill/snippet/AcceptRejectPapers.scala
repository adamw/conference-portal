package pl.softwaremill.snippet

import xml._

import net.liftweb.util.Helpers._
import net.liftweb.http._
import js.JsCmds.SetHtml
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
    def updatePaperStatus(paper: Paper, status: PaperStatus.Value) {
      paper.status(status)
      paper.save
    }

    def row(paper: Paper, idx: Int) = {
      val rowId = "row" + idx

      def cells(cellsTemplate: NodeSeq): NodeSeq = {                
        def reDraw = SetHtml(rowId, cells(cellsTemplate))

        bind("cell", cellsTemplate,
          "title" -> paper.title,
          "author" -> paper.author,
          "status" -> ?(paper.status.toString),
          "desc" -> paper.shortDescription,
          "view" -> anchor(ViewPaperLoc.link.createPath(paper), ?("common.view")),
          "accept" -> a(() => { updatePaperStatus(paper, PaperStatus.Accepted); reDraw }, Text(?("paper.accept"))),
          "reject" -> a(() => { updatePaperStatus(paper, PaperStatus.Rejected); reDraw }, Text(?("paper.reject")))
          )
      }

      bind("paper", rowTemplate,
        AttrBindParam("rowId", Text(rowId), "id"),
        "cells" -> cells _
        )
    }

    val papers = paperService.conferencePapers(CurrentConference.is)
    papers.zipWithIndex.flatMap { case (paper: Paper, idx: Int) => row(paper, idx) }
  }
}