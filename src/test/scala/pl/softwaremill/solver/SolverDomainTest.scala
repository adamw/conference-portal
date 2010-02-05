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

    "collapse preferences" in {
      val prefs = List(Preference(papers(0), papers(1), 2), Preference(papers(3), papers(1), 1), Preference(papers(1), papers(0), 1),
        Preference(papers(3), papers(1), 1), Preference(papers(2), papers(4), 8))
      val collapsed = Preference.collapse(prefs)

      collapsed must haveSize(3)
      collapsed must exist(pref => (pref == Preference(papers(0), papers(1), 3) || (pref == Preference(papers(1), papers(0), 3))))
      collapsed must exist(pref => (pref == Preference(papers(3), papers(1), 2) || (pref == Preference(papers(1), papers(3), 2))))
      collapsed must exist(pref => (pref == Preference(papers(2), papers(4), 8) || (pref == Preference(papers(4), papers(2), 8))))
    }
  }
}

class SolverDomainJUnitTest extends JUnit4(SolverDomainTest)