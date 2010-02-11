package pl.softwaremill.model

import net.liftweb.util.FieldError
import net.liftweb.mapper.{OneToMany, Mapper, MappedInt}

/**
 * @author Adam Warski (adam at warski dot org)
 */
trait Positionable[T <: Mapper[T]] { this: Mapper[T] =>
  val defaultPosition: Int
  val validatePosition: Int => List[FieldError]

  object position extends MappedInt[T](this) {
    override def defaultValue = defaultPosition
    override def validations = validatePosition :: super.validations
  }
}

trait PositionManager[C <: OneToMany[Long, C], T <: Positionable[T] with Mapper[T]] { this: C#MappedOneToMany[T] =>
  def createNew: T

  def addObj = {
    val obj = createNew
    obj.position(this.size)
    this += obj
    obj
  }

  def deleteObj(obj: T) = {
    this -= obj

    // Fixing the indexes using the sorted list
    val sortedObjs = sorted
    for (i <- obj.position.is until sortedObjs.size) sortedObjs(i).position(i)
    this
  }

  def moveUp(obj: T) = {
    move(obj, obj.position.is-1, -1)
  }

  def moveDown(obj: T) = {
    move(obj, obj.position.is+1, this.size)
  }

  def sorted = this.toList.sort((_: T).position.is < (_: T).position.is)

  private def move(obj: T, newIdx: Int, bound: Int) = {
    if (newIdx != bound) {
      // There must be an obj with a position equal to newIdx
      val secondObj = this.find(_.position == newIdx).get

      secondObj.position(obj.position.is)
      obj.position(newIdx)
    }

    this
  }

}