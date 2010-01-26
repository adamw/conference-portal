package pl.softwaremill.lib

/**
 * @author Adam Warski (adam at warski dot org)
 */
object CollectionTools {
  def aggregate[K,T](list: List[T], projection: Function1[T,K]): Map[K,List[T]] = {
    def aggregate(list: List[T], res: Map[K,List[T]]): Map[K,List[T]] = {
      list match {
        case Nil => res
        case head :: tail => {
          val projected = projection(head)
          val toAdd = projected -> (head :: res.getOrElse(projected, Nil))
          aggregate(tail, res + toAdd)
        }
      }
    }

    aggregate(list.reverse, Map())
  }
}