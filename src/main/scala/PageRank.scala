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
        val corpus: List[WebPage] = pages.values.toList
        val random: Random = new Random()

        // for each random walk we are going to recursively travel 100 times
        def helper(n: Int, webPage: WebPage): WebPage = {
            println(webPage.links.length)
            if (n == 0) return webPage

            val dampFactorValue = random.nextInt(100)

            if dampFactorValue <= 85 && webPage.links.length > 0 then helper(n-1, pages(webPage.links(random.nextInt(webPage.links.length)))) else helper(n-1, corpus(random.nextInt(corpus.length)))
        }

        // 10,000 "walkers", and what webpage they ended up
        val endingWebpages = for (i <- 0 until 10000) yield helper(100, corpus(random.nextInt(corpus.length)))

        // Transform list of ending sites to a count of endings
        val unNormalizedScores: Map[String, Int] = (for (i <- corpus) yield i.id -> endingWebpages.count(_ == i)).toMap

        val normalizedScores: Map[String, Double] = for (i <- unNormalizedScores) yield i._1 -> (i._2 + 1.0)/(10000.0 + corpus.length)

        normalizedScores
    }
}