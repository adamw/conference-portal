package pl.softwaremill.solver.domain;

import org.drools.solver.core.score.Score;
import org.drools.solver.core.solution.Solution;

/**
 * {@see http://lampsvn.epfl.ch/trac/scala/ticket/1737}
 * @author Adam Warski (adam at warski dot org)
 */
public abstract class ScalaSolution implements Solution {
    public Score getScore() { return getScoreScala(); }
    public abstract Score<?> getScoreScala();

    public void setScore(Score score) { setScoreScala(score); }
    public abstract void setScoreScala(Score<?> score);  
}
