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
        for (i <- pages) yield getTfIDF(i, query, pages)
    }

    def getTfIDF(page: RankedWebPage, query: List[String], pages: List[RankedWebPage]): Double = {
        //println(page.url)
        val tfTerm = for(i <- query) yield getIndvCountTf(page, i) // get ind tf
        //print("tf:")
        //println(tfTerm)
        val corpusTerm = for (i <- query) yield indTermInCorpus(pages.filter(_.id != page.id), i) // get how ofter its in other docs
        //print("ct:")
        //println(corpusTerm)
        val idfs = for (i <- 0 until tfTerm.length) yield calcIDF(tfTerm(i), corpusTerm(i), pages.length)
        //print("idf")
        //println(idfs)
        idfs.foldLeft(1.0)(_ * _)
    }

    def calcIDF(tf: Double, df: Double, N: Double): Double = {
        tf * math.log(N/df+1)
    }

    def indTermInCorpus(pages: List[RankedWebPage], temp: String): Double = {
        (for (i <- pages; if i.text.split(' ').contains(temp)) yield 1).length
    }

    def getIndvCountTf(page: RankedWebPage, term: String): Double = {
        val text = page.text.split(' ')
        val textLength: Double = text.length
        text.count(_.toLowerCase() == term.toLowerCase())/textLength
    }
}

@main def testGetInd():Unit ={
    val text = "hello there t t t t t".split(' ')
    println(text.toList)
    val textLength: Double = text.length
    val textCount: Double = text.count(_.toLowerCase() == "t".toLowerCase())/textLength
    print(textCount)
}