package pl.softwaremill.snippet

import xml._

import net.liftweb.util.Helpers._
import net.liftweb.http._
import js.JsCmd
import net.liftweb.common._
import SHtml._

import net.liftweb.http.js.JsCmds._

import pl.softwaremill.model.Paper

/**
 * @author Adam Warski (adam at warski dot org)
 */
object PaperSnippetTools {
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

  /**
   * Binds a list of papers with the possibility to re-render individual rows, select them, and bound the selection
   * with a maximum number.
   * @param listTemplate The template of the whole list. The template should have a {@code list:row} element. The
   * list should be in an element with id {@code paper_list}. See also the {@link paperListWithSingleRowRerender}.
   * @param config Configuration
   */
  def paperListWithMaxSelection(listTemplate: NodeSeq, config: PaperListWithMaxSelectionConfig): NodeSeq = {
    def doList: NodeSeq = {
      def reDrawSelectionsLeft = SetHtml("selections_left", config.selectionsLeftNode)

      def rows(rowTemplate: NodeSeq): NodeSeq = paperListWithSingleRowRerender(rowTemplate, config.papers, Full(reDrawSelectionsLeft _),
        (paper, cellsTemplate, reDraw) => {
          val selected = config.isSelected(paper)
          def selectionBlocked = config.maxSelections <= config.selectedCount

          def selectDeselectLink = a(() => {
            val selectionBlockedBefore = selectionBlocked
            config.select(paper, !selected)
            val selectionBlockedAfter = selectionBlocked

            // Doing a full-redraw if the selection has been blocked or unblocked to hide/show all select links
            if (selectionBlockedBefore != selectionBlockedAfter) {
              CmdPair(reDrawSelectionsLeft, SetHtml("paper_list", doList))
            } else reDraw() },
            Text(if (selected) config.deselectLinkText else config.selectLinkText))

          bind("cell", config.bindCells(paper, cellsTemplate),
            "select" -> (if (selectionBlocked && !selected) NodeSeq.Empty else selectDeselectLink)
            )
        })

      bind("list", listTemplate, "row" -> rows _)
    }

    doList
  }

  trait PaperListWithMaxSelectionConfig {
    val selectLinkText: String
    val deselectLinkText: String
    val papers: List[Paper]
    val maxSelections: Int

    def isSelected(paper: Paper): Boolean
    def select(paper: Paper, select: Boolean)
    def selectedCount: Int
    def selectionsLeftText(left: Int): String

    def selectionsLeftNode = Text(selectionsLeftText(maxSelections - selectedCount))

    def bindCells(paper: Paper, cellsTemplate: NodeSeq): NodeSeq
  }
}