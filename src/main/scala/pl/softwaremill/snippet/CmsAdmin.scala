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
      parent._children.addObj(child)
      parent.save

      child
    }

    def deleteMenuItem(menuItem: MenuItem) {
      if (menuItem.hasParent) {
        val parent = menuItem.parent.obj.open_!
        parent._children.deleteObj(menuItem)
        parent.save
      }
    }

    def addTypeForm(parent: MenuItem) = {
      val child = parent._children.createNew

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

    def menuItemBody(menuItem: MenuItem) = {
      <span>
        { if (Full(menuItem) == CurrentMenuItem.is) <strong>{menuItem.title.is}</strong> else menuItem.title.is }
        ({ ?(menuItem.menuItemType.toString) })
        {
        if (menuItem.hasParent) {
          a(() => { CurrentMenuItem(Full(menuItem)); reRender }, Text(?("common.edit"))) :: Text(" ") ::
                  a(() => { menuItem.parent.obj.open_!._children.moveUp(menuItem).save; reRender }, Text(?("common.move_up"))) :: Text(" ") ::
                  a(() => { menuItem.parent.obj.open_!._children.moveDown(menuItem).save; reRender }, Text(?("common.move_down"))) :: Text(" ") ::
                  a(Call("confirm_menuitem_delete", Str(?("menuitem.confirm_delete", menuItem.title.is))),
                    () => { deleteMenuItem(menuItem); reRender },
                    Text(?("common.delete"))) :: Nil
        } else NodeSeq.Empty
        }
      </span>
    }

    def menuItemAdditionalChildren(menuItem: MenuItem) =
      if (menuItem.menuItemType == MenuItemType.Parent)
        Full(<li>{ addTypeForm(menuItem) }</li>)
      else Empty

    rootMenuItem.htmlTree(menuItemBody _, menuItemAdditionalChildren _, mi => true, Nil)
  }

  def editMenuItem(editTemplate: NodeSeq): NodeSeq = {
    this.editTemplate = editTemplate

    def saveMenuItem(menuItem: MenuItem) {
      menuItem.validate match {
        case Nil  => menuItem.save(); S.notice(S.?("common.saved", menuItem.title))
        case xs   => S.error(xs)
      }
    }

    def bindFooter(menuItem: MenuItem)(footerTemplate: NodeSeq) =
      bind("footer", footerTemplate,
        "save" -> ajaxSubmit(?("common.save"), () => { saveMenuItem(menuItem); reRender }),
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
        case MenuItemType.Page => bindRow("menuitem.page_path", menuItem.pagePath.toForm, template) ++
          bindRow("menuitem.page", menuItem.pageContent.toForm, template)
        case MenuItemType.Parent => bindRow("menuitem.page_path", menuItem.pagePath.toForm, template)
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