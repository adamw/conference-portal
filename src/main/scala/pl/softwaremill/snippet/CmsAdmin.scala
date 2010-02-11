package pl.softwaremill.snippet

import xml._

import net.liftweb.util.Helpers._
import net.liftweb.http._
import js.JE._
import js.JsCmd
import js.JsCmds.{CmdPair, SetHtml}
import net.liftweb.common._
import SHtml._
import S._

import pl.softwaremill.model._

import SnippetTools._

/**
 * @author Adam Warski (adam at warski dot org)
 */
class CmsAdmin {
  object CurrentMenuItem extends RequestVar[Box[MenuItem]](Empty)

  var editTreeTemplates: NodeSeq = _
  var editTemplate: NodeSeq = _

  def reRenderEditTree: JsCmd = SetHtml("editTree", editTree(editTreeTemplates))
  def reRenderEditMenuItem: JsCmd = SetHtml("editMenuItem", editMenuItem(editTemplate))
  def reRender: JsCmd = CmdPair(reRenderEditTree, reRenderEditMenuItem)

  def editTree(templates: NodeSeq): NodeSeq = {
    this.editTreeTemplates = templates

    def rootMenuItem = {
      val conf = CurrentConference.is
      conf.mainMenuItem.obj match {
        case Full(menuItem) => menuItem
        case _ => {
          val root = new MenuItem

          root.menuItemType(MenuItemType.Parent)
          root.save

          conf.mainMenuItem(root)
          conf.save

          root
        }
      }
    }

    def addMenuItem(child: MenuItem, parent: MenuItem) = {
      child.title(?("menuitem.title_template")).parent(parent).save

      parent.children += child
      parent.save

      child
    }

    def deleteMenuItem(menuItem: MenuItem) {
      if (menuItem.hasParent) {
        val parent = menuItem.parent.obj.open_!
        parent.children -= menuItem
        parent.save
      }
    }

    def addTypeForm(parent: MenuItem) = {
      val child = new MenuItem

      def typeForm = {
        val options = MenuItemType.map { menuItemType => (menuItemType, ?(menuItemType.toString)) }
        selectObj[MenuItemType.Value](options.toList, Full(child.menuItemType), child.menuItemType(_))
      }

      ajaxForm(
        bind("add", chooseTemplate("tree", "add", templates),
          "type" -> typeForm,
          "submit" -> ajaxSubmit(?("common.add"), () => { addMenuItem(child, parent); reRender }))
        )
    }

    def tree(menuItem: MenuItem): NodeSeq = {
      <li>
        <span>
          { if (Full(menuItem) == CurrentMenuItem.is) <strong>{menuItem.title.is}</strong> else menuItem.title.is }
          ({ ?(menuItem.menuItemType.toString) })
          {
          if (menuItem.hasParent) {
            a(() => { CurrentMenuItem(Full(menuItem)); reRender }, Text(?("common.edit"))) :: Text(" ") ::
            a(Call("confirm_menuitem_delete", Str(?("menuitem.confirm_delete", menuItem.title.is))),
              () => { deleteMenuItem(menuItem); reRender },
              Text(?("common.delete"))) :: Nil
          } else NodeSeq.Empty
          }
        </span>
        <ul>
          { menuItem.children.flatMap(tree(_)) }
          {
          if (menuItem.menuItemType == MenuItemType.Parent) <li>{ addTypeForm(menuItem) }</li>
          else NodeSeq.Empty
          }
        </ul>
      </li>
    }

    tree(rootMenuItem)
  }

  def editMenuItem(editTemplate: NodeSeq): NodeSeq = {
    this.editTemplate = editTemplate

    def bindFooter(menuItem: MenuItem)(footerTemplate: NodeSeq) =
      bind("footer", footerTemplate,
        "save" -> ajaxSubmit(?("common.save"), () => { menuItem.save; reRender }),
        "cancel" -> a(() => { CurrentMenuItem(Empty); reRender }, Text(?("common.cancel")))
        )

    def bindRow(labelKey: String, content: Box[NodeSeq], rowTemplate: NodeSeq) = {
      bind("row", rowTemplate,
        "label" -> ?(labelKey),
        "content" -> content)
    }

    def bindTypeSpecific(menuItem: MenuItem, template: NodeSeq): NodeSeq = {
      menuItem.menuItemType match {
        case MenuItemType.Link => bindRow("menuitem.link", menuItem.linkContent.toForm, template)
        case MenuItemType.Page => bindRow("menuitem.page", menuItem.pageContent.toForm, template)
        case _ => NodeSeq.Empty
      }
    }
    
    def bindRows(menuItem: MenuItem)(rowTemplate: NodeSeq): NodeSeq = {
      bindRow("menuitem.title", menuItem.title.toForm, rowTemplate) ++
      bindTypeSpecific(menuItem, rowTemplate) 
    }

    CurrentMenuItem.is match {
      case Full(menuItem) =>
        ajaxForm(
          bind("template", chooseTemplate("edit", "template", editTemplate),
            "row" -> bindRows(menuItem) _,
            "footer" -> bindFooter(menuItem) _
            )
          )
      case _ => NodeSeq.Empty
    }
  }
}