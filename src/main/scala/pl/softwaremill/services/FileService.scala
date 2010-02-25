package pl.softwaremill.services

import net.liftweb.http._
import net.liftweb.mapper._
import net.liftweb.common._
import net.liftweb.util.Helpers._

import pl.softwaremill.model.File

/**
 * @author Adam Warski (adam at warski dot org)
 * @author Dridus (http://github.com/Dridus)
 */
object FileService {
  object FileMatch {
    def unapply(in: String): Option[File] = File.find(By(File.uniqueId, in.trim))
  }

  def matcher: LiftRules.DispatchPF = {
    case req @ Req(File.FILE_URL_CONTEXT :: FileMatch(file) :: Nil, _, GetRequest) =>
      () => serveFile(file, req)
  }

  def serveFile(file: File, req: Req): Box[LiftResponse] = {
    if (req.testIfModifiedSince(file.saveTime.is+1)) {
      Full(InMemoryResponse(
        new Array[Byte](0),
        List("Last-Modified" -> toInternetDate(file.saveTime.is)),
        Nil,
        304))
    } else {
      val content = file.content.is
      Full(InMemoryResponse(
        content,
        List("Last-Modified" -> toInternetDate(file.saveTime.is),
          "Content-Type" -> file.mimeType.is,
          "Content-Length" -> content.length.toString),
        Nil,
        200))
    }
  }

}