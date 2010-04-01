package pl.softwaremill.model

import xml._

import net.liftweb.mapper._
import net.liftweb.http.S._
import net.liftweb.common._

import ModelTools._

/**
 * @author Adam Warski (adam at warski dot org)
 */
class MenuItem extends LongKeyedMapper[MenuItem] with IdPK with OneToMany[Long, MenuItem] with Positionable[MenuItem] {
  def getSingleton = MenuItem

  object mappedMenuItemType extends MappedInt(this) {
    override def defaultValue = MenuItemType.Page.id
    override def dbColumnName = "menu_item_type"
  }

  object title extends MappedString(this, 1000)

  def menuItemType = MenuItemType(mappedMenuItemType.is)
  def menuItemType(newType: MenuItemType.Value) = mappedMenuItemType(newType.id)

  object pagePath extends MappedString(this, 1000)

  object pageContent extends MappedTextarea(this, 10000) {
    override def textareaRows = 30
    override def textareaCols = 80

    override def validations = valXml(?("menuitem.page.invalid_xml"), pageContent) _ :: super.validations
  }

  object linkContent extends MappedString(this, 1000)

  object _children extends MappedOneToMany(MenuItem, MenuItem.parent) with Owned[MenuItem] with Cascade[MenuItem]
    with PositionManager[MenuItem, MenuItem] {
    def createNew = new MenuItem
  }

  def children: List[MenuItem] = _children.sorted

  object parent extends LongMappedMapper[MenuItem, MenuItem](this, MenuItem)

  def hasParent = parent.defined_?

  def htmlTree(body: MenuItem => NodeSeq, additionalChildren: MenuItem => Box[NodeSeq], visible: MenuItem => Boolean,
               nestedUlClasses: List[String]): NodeSeq = {
    if (visible(this)) {
      <li>
        { body(this) }
        {
        val chld = children
        val addChld = additionalChildren(this)
        if (chld.size > 0 || addChld.isDefined) {
          <ul class={nestedUlClasses.firstOption.map(Text(_))}>
            { chld.flatMap(_.htmlTree(body, additionalChildren, visible, nestedUlClasses match { case Nil => Nil; case l => l })) }
            { addChld openOr NodeSeq.Empty }
          </ul>
        } else NodeSeq.Empty
        }
      </li>
    } else NodeSeq.Empty
  }
}

object MenuItem extends MenuItem with LongKeyedMetaMapper[MenuItem]

object MenuItemType extends Enumeration {
  val Parent = Value("menuitemtype.parent")
  val Link = Value("menuitemtype.link")
  val Page = Value("menuitemtype.page")
  val Conference = Value("menuitemtype.conference")
  val User = Value("menuitemtype.user")
  val Manage = Value("menuitemtype.manage")
  val Special = Value("menuitemtype.special")
}

object MenuItemPath {
  def apply(menuItem: MenuItem) = {
    def accumulatePaths(current: MenuItem, acc: List[String]): List[String] = {
      current.parent.obj match {
        case Full(parent) => accumulatePaths(parent, current.pagePath.is :: acc)
        case _ => acc
      }
    }

    accumulatePaths(menuItem, Nil)
  }

  def unapply(rootAndPaths: (MenuItem, List[String])): Option[MenuItem] = {
    def find(parent: MenuItem, path: List[String]): Box[MenuItem] = {
      path match {
        case Nil => if (parent.menuItemType == MenuItemType.Page || parent.menuItemType == MenuItemType.Special) Full(parent) else Empty
        case head :: tail => parent.children.find(_.pagePath.is == head).flatMap(find(_, tail))
      }
    }

    find(rootAndPaths._1, rootAndPaths._2)
  }
}