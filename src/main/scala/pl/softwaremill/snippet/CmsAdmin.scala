package pl.softwaremill.snippet

import xml._

import net.liftweb.util.Helpers._
import net.liftweb.http._
import js.JE._
import js.JsCmds.SetHtml
import net.liftweb.common._
import SHtml._
import S._

import pl.softwaremill.lib.D
import pl.softwaremill.model._

import SnippetTools._

/**
 * @author Adam Warski (adam at warski dot org)
 */
class CmsAdmin {
  def editTree(templates: NodeSeq): NodeSeq = {
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

    def reRender = SetHtml("editTree", editTree(templates))

    def addMenuItem(child: MenuItem, parent: MenuItem) = {
      println("X")

      child.title(?("menuitem.title_template")).parent(parent).save

      println("C " + child)

      parent.children += child
      parent.save

      println("P " + parent)

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
          "submit" -> ajaxSubmit(?("common.add"), () => { println("Add"); addMenuItem(child, parent); reRender }))
        )
    }

    def tree(menuItem: MenuItem): NodeSeq = {
      <li>
        <span>
          { menuItem.title.is }
          {
          if (menuItem.hasParent) {
            a(Call("confirm_menuitem_delete", Str(?("menuitem.confirm_delete", menuItem.title.is))),
              () => { deleteMenuItem(menuItem); reRender },
              Text(?("common.delete")))
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
}