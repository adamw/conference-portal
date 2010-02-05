package pl.softwaremill.solver

import domain.{Paper, Preference}
import org.specs.Specification
import org.specs.runner.JUnit4

/**
 * @author Adam Warski (adam at warski dot org)
 */

object SolverDomainTest extends Specification {
  "The Preference object" should {
    val papers = (1 to 5).map(i => { val p = new Paper; p.id = i; p }).toList

    "generate preferences for an empty paper id list" in {
      Preference.generate(papers, List()) must_== List()
    }

    "generate preferences for a 4-element paper id list" in {
      val prefs = Preference.generate(papers, List(4, 5, 1, 2))
      val ids = prefs.map(pref => (pref.paper1.id, pref.paper2.id))

      ids must_== List((1, 2), (1, 4), (1, 5), (2, 4), (2, 5), (4, 5))
    }
  }
}

class SolverDomainJUnitTest extends JUnit4(SolverDomainTest)