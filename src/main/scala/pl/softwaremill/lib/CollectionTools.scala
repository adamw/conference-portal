package pl.softwaremill.lib

/**
 * @author Adam Warski (adam at warski dot org)
 */
object CollectionTools {
  def aggregate[K,T](list: List[T], projection: T => K): Map[K,List[T]] = {
    aggregate(list, projection, ((x: T) => x))
  }

  def aggregate[K,V,T](list: List[T], keyProjection: T => K, valueProjection: T => V): Map[K,List[V]] = {
    def aggregate(list: List[T], res: Map[K,List[V]]): Map[K,List[V]] = {
      list match {
        case Nil => res
        case head :: tail => {
          val projectedKey = keyProjection(head)
          val projectedValue = valueProjection(head)
          val toAdd = projectedKey -> (projectedValue :: res.getOrElse(projectedKey, Nil))
          aggregate(tail, res + toAdd)
        }
      }
    }

    aggregate(list.reverse, Map())
  }
}