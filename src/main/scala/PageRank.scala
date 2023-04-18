import scala.annotation.tailrec
import scala.util.Random
import scala.collection.parallel.CollectionConverters.*


@main def mainMapTest(): Unit = {
    val map = Map("Al" -> 1)
    print(map)
    val mapTail = map.tail
    print(mapTail.isEmpty)
}

object PageRank {
    /**
     * @param pages A map of page.id to page for some number of WebPage objects
     * @return      A map of page.id to a weight of 1.0 for those same WebPage objects
     */
    def equal(pages: Map[String, WebPage]): Map[String, Double] = {
        (for (i <- pages.keys) yield pages(i).id -> 1.0).toMap
    }

    /**
     * @param pages A map of page.id to page for some number of WebPage objects
     * @return A map of page.id to a weight that is a simple count of the number of pages linking to that page
     */
    def indegree(pages: Map[String, WebPage]): Map[String, Double] = {
        @tailrec
        def helper(page: String, pages: List[WebPage] , count: Double): Double = {
            pages match {
                case Nil => count
                case _ => helper(page, pages.tail, count + (if pages.head.links.contains(page) then 1 else 0))
            }
        }
        (for (i <- pages.keys) yield pages(i).id -> helper(i, pages.values.toList, 0)).toMap
    }

    def pagerank(pages: Map[String, WebPage]): Map[String, Double] = {
        Map()
    }
}