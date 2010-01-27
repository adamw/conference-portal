package pl.softwaremill.services

import net.liftweb.mapper._
import net.liftweb.common.Box

import pl.softwaremill.model._

/**
 * @author Adam Warski (adam at warski dot org)
 */
trait PaperService {
  def userPapers(user: User): List[Paper]
  def conferencePapers(conf: Conference): List[Paper]
  def find(id: String): Box[Paper]
}

class PaperServiceImpl extends PaperService {
  def userPapers(user: User): List[Paper] = {
    Paper.findAll(By(Paper.user, user),
      OrderBy(Paper.title, Ascending))
  }

  def conferencePapers(conf: Conference): List[Paper] = {
    Paper.findAll(By(Paper.conference, conf),
      OrderBy(Paper.title, Ascending))
  }

  def find(id: String): Box[Paper] = {
    Paper.find(id)
  }
}