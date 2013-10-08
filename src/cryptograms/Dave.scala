package cryptograms

import Constants._
import fileRead._
import collection.mutable.{ HashMap, MultiMap }

object Dave {
  def main(args: Array[String]): Unit = {
    // Load the file that contains the 6,000 quotes and convert them into a list, 
    // one quote per element.
    val quotes: List[String] = getFile(quotesPath).map(x => x drop 2)
    // Load the top 2000 most used Englisth words into a List of string 
    val words = getFile(mostUsedWordsPath).map(_.dropWhile(!_.isLetter).toUpperCase)

    val patternMap = words.foldLeft(
      new HashMap[String, collection.mutable.Set[String]]() with MultiMap[String, String])(
        (m, c) => m.addBinding(pattern(c), c))

    println(sortWordMessage("ABCDE", Set("AB", "DEGF", "ACDEG", "ZEQG")))
  }
  /**
   * Convert given string into pattern. 
   */
  def pattern(s: String): String = s.tail.foldLeft("A")((res: String, y: Char) => res +
    (if (res.length != s.indexOf(y)) 'A' + s.indexOf(y) else 'A' + res.length).toChar)

  def sortWordMessage(m: String, message: Set[String]) = {
    def f(s: Set[String], acc: List[String]): List[String] = {
      if (s.isEmpty) acc else {
        val temp = s.maxBy(x => (x.toSet intersect acc.flatten.toSet).size)
        f(s - temp, acc :+ temp)
      }
    }
    f(message - m, List(m))
  }

  def discoverCode(message: String): String = {
    val messageSet = message.toUpperCase.split("\\W+").toSet
    val m = messageSet.maxBy(x => x.length / x.distinct.length.toDouble).toString
    val wList = sortWordMessage(m, messageSet)
    m
  }

  def encode(plainText: String, code: String): String = {
    plainText map { x => if (!x.isLetter) x else code(x.toUpper - 'A') }
  }
  def decode(encodedText: String, code: String): String = {
    encodedText map { x => if (!x.isLetter) x else ('A' + code.indexOf(x.toUpper)).toChar }
  }

}