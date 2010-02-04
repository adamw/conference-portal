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
    case that: Base if (this.getClass() == that.getClass()) => id == that.id
    case _ => false
  }

  override def hashCode = id
}

class Paper extends Base {
  override def toString = "P%d".format(id)
}

class Room extends Base {
  override def toString = "R%d".format(id)
}

class Time extends Base {
  override def toString = "T%d".format(id)
}

class Slot extends Base {
  @BeanProperty
  var room: Room = _

  @BeanProperty
  var time: Time = _

  override def toString = "S%d (%s,%s)".format(id, room, time)
}

class Assigment extends java.lang.Comparable[Assigment] {
  @BeanProperty
  var paper: Paper = _

  @BeanProperty
  var slot: Slot = _

  def getId() = { paper.id }

  override def clone: Assigment = {
    val cloned = new Assigment
    cloned.paper = paper
    cloned.slot = slot
    cloned
  }

  def compareTo(other: Assigment): Int = {
    val res = paper.compareTo(other.paper)
    if (res == 0) slot.compareTo(other.slot) else res
  }

  override def equals(other: Any) = other match {
    case that: Assigment => ((paper == that.paper) && (slot == that.slot))
    case _ => false
  }

  override def hashCode = 41*paper.hashCode + slot.hashCode

  override def toString = "[A %s->%s]".format(paper, slot)
}

class Preference extends Base {
  @BeanProperty
  var paper1: Paper = _

  @BeanProperty
  var paper2: Paper = _

  override def toString = "[PR%d %s & %s]".format(id, paper1, paper2)
}

class Schedule(papers: List[Paper], val slots: List[Slot], val assigments: List[Assigment], preferences: List[Preference])
        extends ScalaSolution {
  var score: Score[_] = _

  def setScoreScala(score: Score[_]) = { this.score = score }
  def getScoreScala = score

  def cloneSolution = {
    val clonedAssigments = assigments.map { _.clone }
    val cloned = new Schedule(papers, slots, clonedAssigments, preferences)
    cloned.setScoreScala(score)
    cloned
  }

  def getFacts: java.util.Collection[Object] = {
    val ret = new java.util.ArrayList[Object]
    preferences.foreach(ret.add(_))
    assigments.foreach(ret.add(_))
    ret
  }

  def violatedPreferences = {
    def preferenceViolated_?(pref: Preference) = {
      def assigmentForPaper(paper: Paper) = assigments.find(_.paper == paper).get

      assigmentForPaper(pref.paper1).slot.time == assigmentForPaper(pref.paper2).slot.time
    }

    preferences.filter(preferenceViolated_?(_))
  }

  override def equals(other: Any) = other match {
    case that: Schedule => assigments == that.assigments
    case _ => false
  }

  override def hashCode = assigments.hashCode
}
