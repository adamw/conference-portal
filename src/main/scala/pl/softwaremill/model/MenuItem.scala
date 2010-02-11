package pl.softwaremill.model

import xml._

import net.liftweb.http._
import net.liftweb.common._
import net.liftweb.mapper._
import net.liftweb.util.FieldError
import S._

import pl.softwaremill.lib.CollectionTools

import java.util.Date

/**
 * @author Adam Warski (adam at warski dot org)
 */

class MenuItem extends LongKeyedMapper[MenuItem] with IdPK with OneToMany[Long, MenuItem] {
  def getSingleton = MenuItem

  object mappedMenuItemType extends MappedInt(this) {
    override def defaultValue = MenuItemType.Page.id
    override def dbColumnName = "menu_item_type"
  }

  object title extends MappedString(this, 1000)

  def menuItemType = MenuItemType(mappedMenuItemType.is)
  def menuItemType(newType: MenuItemType.Value) = mappedMenuItemType(newType.id)

  object pageContent extends MappedTextarea(this, 10000)

  object linkContent extends MappedString(this, 1000)

  object children extends MappedOneToMany(MenuItem, MenuItem.parent) with Owned[MenuItem] with Cascade[MenuItem]

  object parent extends LongMappedMapper[MenuItem, MenuItem](this, MenuItem)

  def hasParent = parent.defined_?
}

object MenuItem extends MenuItem with LongKeyedMetaMapper[MenuItem] {

}

object MenuItemType extends Enumeration {
  val Parent = Value("menuitemtype.parent")
  val Link = Value("menuitemtype.link")
  val Page = Value("menuitemtype.page")
  val Special = Value("menuitemtype.special")
}