package pl.softwaremill.model

import net.liftweb.mapper._

/**
 * @author Adam Warski (adam at warski dot org)
 */
trait FullCascade[T<:Mapper[T], O<:KeyedMapper[Long,O]] extends LifecycleCallbacks { this: LongMappedMapper[T, O] =>
  override abstract def afterDelete = {
    this.obj.map { _.delete_! }
    super.afterDelete
  }

  override abstract def afterSave = {
    this.obj.map { _.save }
    super.afterSave
  }
}