package cryptograms

import Constants._
import fileRead._
import collection.mutable.{ HashMap, MultiMap, Set }

object Dave {

  def main(args: Array[String]): Unit = {
    // Load the file that contains the 6,000 quotes and convert them into a list, 
    // one quote per element.
    val quotes: List[String] = getFile(quotesPath).map(x => x drop 2)
    // Load the top 2000 most used Englisth words into a List of string 
    val words = getFile(mostUsedWordsPath).map(_.dropWhile(!_.isLetter).toUpperCase)

    val patternSet = words.map(pattern(_)).distinct

    val wordsMap = {
      val mm = new HashMap[String, Set[String]] with MultiMap[String, String]
      for (p <- patternSet)
        for (w <- words if (pattern(w) == p))
          mm.addBinding(p, w)
      mm
    }

//    wordsMap("ABCA") foreach println
    
    println(sortedWordMessage("ABCDE", List("AB","DEGF", "ACDEG", "ZEQG")))

    //    val temp = patternSet.zip(words).toMap    

    //    val wordsMap: collection.mutable.Map[String, List[String]] = {
    //      
    //    }
    //      

    //      words.foldLeft(Map[String, ListBuffer[String]]())(
    //      (m, c) => m + (pattern(c) -> (
    //          m.getOrElse(pattern(c), ListBuffer[String](""))
    //              
    //              .append(c)))))

    //    quotes.map(x => x.toUpperCase.filter(_.isLetter).distinct) foreach println
    //
    //    // Print out average distinct letters used in each quote.
    //    println("Average number of distinct letters used in each sentence:")
    //    println(quotes.map(x => x.toUpperCase.filter(_.isLetter).distinct.length).sum / quotes.length.toDouble)
    //
    //    val wordsList = quotes.map(x => x.split("\\W+")).flatten.map(_.toUpperCase)
    //
    //    // Print out size of total words used. 
    //    println(s"Word set has ${wordsList.size} of words")
    //    println(s"Word set has ${wordsList.toSet.size} of distinct words")
  }

  def pattern(s: String): String = {
    def patterns(pos: Int, acc: String): String = {
      if (pos == s.length) acc
      else {
        val currentChar = s(pos)
        val replacementChar = ('A' + pos).toChar
        if (s.take(pos) contains currentChar) patterns(pos + 1, acc + acc(s.indexOf(currentChar)))
        else patterns(pos + 1, acc + replacementChar)
      }
    }
    patterns(1, "A")
  }

  def discoverCode(message: String): String = {
    val messageArray = message.toUpperCase.split("\\W+")

    val m = messageArray.maxBy(x => x.length / x.distinct.length.toDouble).toString

    val wList = sortedWordMessage(m, messageArray.toList)
    m
  }

  def sortedWordMessage(m: String, message: List[String]) = {
    def f(s: List[String], acc: List[String]): List[String] = {
      if (s.isEmpty) acc
      else {
        val temp = s.maxBy(x => (x.toList.distinct intersect acc.flatten.distinct).size)
        f(s filter (_ != temp), acc :+ temp)
      }
    }
    f(message.filter(_ != m), List(m))
  }

  def encode(plainText: String, code: String): String = {
    plainText map { x => if (!x.isLetter) x else code(x.toUpper - 'A') }
  }
  def decode(encodedText: String, code: String): String = {
    encodedText map { x => if (!x.isLetter) x else ('A' + code.indexOf(x.toUpper)).toChar }
  }

}