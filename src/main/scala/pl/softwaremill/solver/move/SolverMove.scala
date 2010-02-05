package pl.softwaremill.solver.move

import org.drools.solver.core.solution.Solution
import org.drools.solver.core.move.factory.CachedMoveFactory
import org.drools.solver.core.move.Move
import org.drools.WorkingMemory

import pl.softwaremill.solver.domain._
import org.drools.solver.core.localsearch.decider.accepter.tabu.TabuPropertyEnabled

/**
 * @author Adam Warski (adam at warski dot org)
 */
object SolverMove {  }

class SlotChangeMove(val ass: Assigment, val slot: Slot) extends Move with TabuPropertyEnabled {
  def doMove(workingMemory: WorkingMemory) = {
    val assHandle = workingMemory.getFactHandle(ass);
    workingMemory.modifyRetract(assHandle);
    ass.slot = slot
    workingMemory.modifyInsert(assHandle, ass);
  }

  def createUndoMove(workingMemory: WorkingMemory) = new SlotChangeMove(ass, ass.slot)

  def isMoveDoable(workingMemory: WorkingMemory) = ass.slot != slot

  def getTabuProperties = java.util.Collections.singletonList(ass)

  override def toString = "[Mo %s => %s]".format(ass, slot)

  override def hashCode = 41*(41 + ass.hashCode) + slot.hashCode

  override def equals(other: Any): Boolean = other match {
    case that: SlotChangeMove => that.ass == ass && that.slot == slot
    case _ => false
  }
}

class SlotSwapMove(val ass1: Assigment, val ass2: Assigment) extends Move with TabuPropertyEnabled {
  def doMove(workingMemory: WorkingMemory) = {
    val ass1Handle = workingMemory.getFactHandle(ass1);
    val ass2Handle = workingMemory.getFactHandle(ass2);
    workingMemory.modifyRetract(ass1Handle);
    workingMemory.modifyRetract(ass2Handle);

    val ass2Slot = ass2.slot
    ass2.slot = ass1.slot
    ass1.slot = ass2Slot
    
    workingMemory.modifyInsert(ass1Handle, ass1);
    workingMemory.modifyInsert(ass2Handle, ass2);
  }

  def createUndoMove(workingMemory: WorkingMemory) = new SlotSwapMove(ass2, ass1)

  def isMoveDoable(workingMemory: WorkingMemory) = ass1.slot.time != ass2.slot.time

  def getTabuProperties = java.util.Arrays.asList(ass1, ass2)

  override def toString = "[Sw %s <=> %s]".format(ass1, ass2)

  override def hashCode = 41*(41 + ass1.hashCode) + ass2.hashCode

  override def equals(other: Any): Boolean = other match {
    case that: SlotSwapMove => that.ass1 == ass1 && that.ass2 == ass2
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

class SlotSwapMoveFactory extends CachedMoveFactory {
  def createCachedMoveList(solution: Solution) = {
    val ret = new java.util.ArrayList[Move]
    val schedule = solution.asInstanceOf[Schedule]
    for (ass1 <- schedule.assigments; ass2 <- schedule.assigments; if (ass1.getId < ass2.getId)) {
      ret.add(new SlotSwapMove(ass1, ass2))
    }

    ret
  }
}