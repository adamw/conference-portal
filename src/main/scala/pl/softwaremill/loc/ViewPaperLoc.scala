package pl.softwaremill.loc

import net.liftweb.sitemap.Loc
import Loc._
import net.liftweb.http.S._
import net.liftweb.common._
import net.liftweb.http._

import xml.Text

import pl.softwaremill.model.Paper
import pl.softwaremill.lib.D
import pl.softwaremill.snippet.Util._
import pl.softwaremill.services.PaperService
import pl.softwaremill.snippet.CurrentPaper

import LocTools._

/**
 * @author Adam Warski (adam at warski dot org)
 */
object ViewPaperLoc extends Loc[Paper] {
  private val PaperPath = "paper"

  def name = "ViewPaper"

  def link = new Link[Paper](PaperPath :: Nil) {
    override def pathList(paper: Paper): List[String] = super.pathList(paper) ++ List(paper.id.is.toString)
  }

  def text = new LinkText((paper: Paper) => Text(?("menu.view_paper", paper.title)))

  def params = List(Hidden)

  def defaultValue = Empty

  override def rewrite = Full({
    case RewriteRequest(parsePath @ ParsePath(PaperPath :: paperId :: Nil, _, _, _), _, httpRequest) => {
      val paperService = D.inject_![PaperService]
      val paperBox: Box[Paper] = paperService.find(paperId)

      paperBox match {
        case Full(paper) => {
          CurrentPaper(paper);
          (finalResponse(PaperPath :: Nil), paper)
        }
        case _ => (RewriteResponse("error" :: Nil,
          Map(errorMessageParam -> ?("paper.unknown", paperId))), null)
      }
    }
    case RewriteRequest(parsePath @ ParsePath(PaperPath :: Nil, _, _, _), _, httpRequest) => {
      (RewriteResponse("error" :: Nil, Map(errorMessageParam -> ?("paper.unknown.noid"))), null)
    }
  })
}