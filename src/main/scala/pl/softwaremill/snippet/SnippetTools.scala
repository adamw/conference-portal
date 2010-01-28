package pl.softwaremill.snippet

import xml._

import net.liftweb.util.Helpers._
import net.liftweb.http._
import js.JsCmd
import net.liftweb.common._
import js.JE.JsRaw
import SHtml._

import net.liftweb.http.js.JsCmds._

import pl.softwaremill.model.Paper

/**
 * @author Adam Warski (adam at warski dot org)
 */
object SnippetTools {
  def anchor(link: String, text: String) = <a href={link}>{text}</a>

  def refreshButton(text: String) = <input type="button" value={text} /> % ("onclick" -> "location.reload(true)")

  def redirectButton(text: String, to: String) = <input type="button" value={text} /> % ("onclick" -> "window.location = '%s'".format(to))

  def confirmLink(to: String, func: () => Any, text: String, confirmText: String) = link(to, func, Text(text),
    "onclick" -> JsRaw("if (!confirm(" + confirmText.encJs + ")) return false;").cmd)

  /**
   * Binds a list of papers with the possibility to re-render individual rows.
   * @param rowTemplate The template of one row containing paper information. The row should have a {@code paper:rowId}
   * attribute, and inside the element with the attribute a {@code paper:cells} element, which should hold the cells
   * content.
   * @param papers The list of papers.
   * @param reDrawOther An optional command re-rendering other parts of the page together with the row.
   * @param bindCells The function that will be called to generate markup for one paper, basing on the paper instance,
   * the cells template markup and a function that re-renders the row (and possibly other fragments of the page, as
   * specified in the {@code reDrawOther} parameter.
   */
  def paperListWithSingleRowRerender(rowTemplate: NodeSeq, papers: List[Paper], reDrawOther: Box[() => JsCmd],
                                     bindCells: (Paper, NodeSeq, () => JsCmd) => NodeSeq) = {
    def row(paper: Paper, idx: Int) = {
      val rowId = "row" + idx

      def cells(cellsTemplate: NodeSeq): NodeSeq = {
        def reDrawRow = SetHtml(rowId, cells(cellsTemplate))
        def reDraw = reDrawOther match {
          case Full(cmd) => CmdPair(reDrawRow, cmd())
          case _ => reDrawRow
        }

        bindCells(paper, cellsTemplate, reDraw _)
      }

      bind("paper", rowTemplate,
        AttrBindParam("rowId", Text(rowId), "id"),
        "cells" -> cells _
        )
    }

    papers.zipWithIndex.flatMap { case (paper: Paper, idx: Int) => row(paper, idx) }
  }
}