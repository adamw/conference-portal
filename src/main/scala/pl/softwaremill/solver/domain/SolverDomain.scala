package pl.softwaremill.solver.domain

import reflect.BeanProperty

import org.drools.solver.core.score.Score

/**
 * @author Adam Warski (adam at warski dot org)
 */
object SolverDomain { }

class Base extends java.lang.Comparable[Base] {
  @BeanProperty
  var id: Int = _

  def compareTo(other: Base): Int = intWrapper(id).compareTo(other.id)

  override def equals(other: Any) = other match {
    case that: this.type => id == that.id
    case _ => false
  }

  override def hashCode = id
}

class Paper extends Base {
  override def toString = "[P%d]".format(id)
}

class Room extends Base {
  override def toString = "[R%d]".format(id)
}

class Time extends Base {
  override def toString = "[T%d]".format(id)
}

class Slot extends Base {
  @BeanProperty
  var room: Room = _

  @BeanProperty
  var time: Time = _

  override def toString = "[S%d (%s, %s)]".format(id, room, time)
}

class Assigment extends Base {
  @BeanProperty
  var paper: Paper = _

  @BeanProperty
  var slot: Slot = _

  override def clone: Assigment = {
    val cloned = new Assigment
    cloned.paper = paper
    cloned.slot = slot
    cloned
  }

  override def toString = "[A%d (%s -> %s)]".format(id, paper, slot)
}

class Preference extends Base {
  @BeanProperty
  var paper1: Paper = _

  @BeanProperty
  var paper2: Paper = _

  override def toString = "[PR%d (%s & %s)]".format(id, paper1, paper2)
}

class Schedule(papers: List[Paper], rooms: List[Room], times: List[Time], val slots: List[Slot], val assigments: List[Assigment], preferences: List[Preference])
        extends ScalaSolution {
  var score: Score[_] = _

  def setScoreScala(score: Score[_]) = { this.score = score }
  def getScoreScala = score

  def cloneSolution = {
    val clonedAssigments = assigments.map { _.clone }
    val cloned = new Schedule(papers, rooms, times, slots, assigments, preferences)
    cloned.setScoreScala(score)
    cloned
  }

  def getFacts: java.util.Collection[Object] = {
    val ret = new java.util.ArrayList[Object]
    preferences.foreach(ret.add(_))
    assigments.foreach(ret.add(_))
    ret
  }
}
