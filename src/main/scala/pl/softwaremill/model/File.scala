package pl.softwaremill.model

import scala.xml._
import net.liftweb.mapper._
import net.liftweb.util.Helpers

/**
 * @author Adam Warski (adam at warski dot org)
 */
class File extends LongKeyedMapper[File] with IdPK {
  def getSingleton = File

  object content extends MappedBinary(this)

  object uniqueId extends MappedUniqueId(this, 16) {
    override def dbIndexed_? = true
  }

  object saveTime extends MappedLong(this) {
    override def defaultValue = Helpers.millis
  }

  object mimeType extends MappedString(this, 256)

  def imageHtml : NodeSeq = <img src={"/" + File.FILE_URL_CONTEXT + "/" + uniqueId} />  
}

object File extends File with LongKeyedMetaMapper[File] {
  val FILE_URL_CONTEXT = "file"

  override def beforeUpdate = ((file: File) => { file.saveTime(Helpers.millis); () }) :: super.beforeUpdate 
}
