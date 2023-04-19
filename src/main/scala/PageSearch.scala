import scala.annotation.tailrec
import scala.math.log
import scala.collection.parallel.CollectionConverters.*

object PageSearch {
    /**
     * @param pages  a list of RankedWebPage objects to be searched
     * @param query  a list of search terms to be counted in those pages
     * @return       a list of the number of times any of the terms appeared in each page in the same order as given
     */
    def count(pages: List[RankedWebPage], query: List[String]): List[Double] = {
        val lowerCaseQuery = for (i <- query) yield i.toLowerCase
        for (i <- pages) yield countMatching(i.text.split(' ').toList, 0, lowerCaseQuery)
    }

    def countMatching(list: List[String], count: Double, query: List[String]): Double = {
        list match {
            case Nil => count
            case _ => countMatching(list.tail, count + (if query.contains(list.head.toLowerCase) then 1 else 0), query)
        }
    }

    /**
     * @param pages a list of RankedWebPage objects to be searched
     * @param query a list of search terms to be counted in those pages
     * @return      a list of the term-frequency of the occurrences of those terms in each page in the same order given
     */
    def tf(pages: List[RankedWebPage], query: List[String]): List[Double] = {
        val countWords = count(pages, query)
        (for (i <- 0 until pages.length) yield countWords(i)/pages(i).text.split(' ').toList.length).toList
    }

    /**
     * @param pages a list of RankedWebPage objects to be searched
     * @param query a list of search terms to be counted in those pages
     * @return      a list of the TF-IDF score for each page in the same order given
     */
    def tfidf(pages: List[RankedWebPage], query: List[String]): List[Double] = {
        val numberOfDocuments: Double = pages.length // N
        val corpusContainWordCount: Double = (for (i <- pages; if documentContainsWord(i, query)) yield 1).length + 1 // D + 1
        print(corpusContainWordCount)
        val termFreqList: List[Double] = tf(pages, query)
        val inverseDocumentFrequency: Double = math.log(numberOfDocuments / corpusContainWordCount)
        for (i <- termFreqList) yield i * inverseDocumentFrequency
    }

    def documentContainsWord(page: RankedWebPage, query: List[String]): Boolean = {
        for (i <- page.text.split(' ')) if query.contains(i) then return true
        false
    }

}