package pl.softwaremill.lib

import org.specs.Specification

import CollectionTools._

/**
 * @author Adam Warski (adam at warski dot org)
 */
object CollectionToolsTest extends Specification {
  "The aggregate method" should {
    case class Key(content: String)
    case class KeyValue(key: Key, value: Int)
    val projection = (_: KeyValue).key

    "correctly handle empty lists" >> {
      aggregate(Nil: List[KeyValue], projection) must haveSize(0)
    }

    "correctly handle single-item aggregates" >> {
      val result = aggregate(List(KeyValue(Key("a"), 1), KeyValue(Key("b"), 2), KeyValue(Key("c"), 3), KeyValue(Key("d"), 1)), projection)

      result must haveSize(4)
      result must havePair(Key("a"), List(KeyValue(Key("a"), 1)))
      result must havePair(Key("b"), List(KeyValue(Key("b"), 2)))
      result must havePair(Key("c"), List(KeyValue(Key("c"), 3)))
      result must havePair(Key("d"), List(KeyValue(Key("d"), 1)))
    }

    "correctly handle multi-item aggregates" >> {
      val result = aggregate(List(
        KeyValue(Key("a"), 1), KeyValue(Key("b"), 2), KeyValue(Key("a"), 3),
        KeyValue(Key("c"), 4), KeyValue(Key("c"), 4), KeyValue(Key("a"), 5),
        KeyValue(Key("d"), 1)),
        projection)

      result must haveSize(4)
      result must havePair(Key("a"), List(KeyValue(Key("a"), 1), KeyValue(Key("a"), 3), KeyValue(Key("a"), 5)))
      result must havePair(Key("b"), List(KeyValue(Key("b"), 2)))
      result must havePair(Key("c"), List(KeyValue(Key("c"), 4), KeyValue(Key("c"), 4)))
      result must havePair(Key("d"), List(KeyValue(Key("d"), 1)))
    }
  }
}