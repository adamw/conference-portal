package pl.softwaremill.solver.move

import org.drools.solver.core.solution.Solution
import org.drools.solver.core.move.factory.CachedMoveFactory
import org.drools.solver.core.move.Move
import org.drools.WorkingMemory

import pl.softwaremill.solver.domain._

/**
 * @author Adam Warski (adam at warski dot org)
 */
object SolverMove {  }

class SlotChangeMove(val ass: Assigment, val slot: Slot) extends Move {
  def doMove(workingMemory: WorkingMemory) = {
    val assHandle = workingMemory.getFactHandle(ass);
    workingMemory.modifyRetract(assHandle);
    ass.slot = slot
    workingMemory.modifyInsert(assHandle, ass);
  }

  def createUndoMove(workingMemory: WorkingMemory) = new SlotChangeMove(ass, ass.slot)

  def isMoveDoable(workingMemory: WorkingMemory) = ass.slot != slot

  override def toString = "[M %s => %s]".format(ass, slot)

  override def hashCode = 41*(41 + ass.hashCode) + slot.hashCode

  override def equals(other: Any): Boolean = other match {
    case that: SlotChangeMove => that.ass == ass && that.slot == slot
    case _ => false
  }
}

class SimpleMoveFactory extends CachedMoveFactory {
  def createCachedMoveList(solution: Solution) = {
    val ret = new java.util.ArrayList[Move]
    val schedule = solution.asInstanceOf[Schedule]
    for (ass <- schedule.assigments; slot <- schedule.slots) {
      ret.add(new SlotChangeMove(ass, slot))
    }

    ret
  }
}