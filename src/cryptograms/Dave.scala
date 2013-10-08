package cryptograms

import Constants._
import fileRead._
import collection.mutable.{ HashMap, MultiMap }

object Dave {
  def main(args: Array[String]): Unit = {
    val testCode = "BCDEFGHIJKLMNOPQRSTUVWXYZA"

    discoverCode(encode(quotes(2), testCode))
  }

  /**
   * Convert given string into pattern.
   */
  def pattern(s: String) = s.tail.foldLeft("A")((res: String, y: Char) => res +
    (if (res.length != s.indexOf(y)) 'A' + s.indexOf(y) else 'A' + res.length).toChar)

  def isLetter(s: String) = s.exists(_.isLetter)

  def getTree(sList: List[String]): Node = {
    def buildTree(sList: List[String], acc: Node): Node =
      if (sList.isEmpty) acc
      else buildTree(sList.init, acc.include(new CipherWordSet(sList.last,
        patternMap.getOrElse(pattern(sList.last), collection.mutable.Set()))))
    buildTree(sList, EmptyNode)
  }

  def sortWordMessage(m: String, message: Set[String]) = {
    def f(s: Set[String], acc: List[String]): List[String] = {
      if (s.isEmpty) acc else {
        val temp = s.maxBy(x => (x.toSet intersect acc.flatten.toSet).size)
        f(s - temp, acc :+ temp)
      }
    }
    f(message - m, List(m))
  }
  def isConflict(x: (String, String), code: String): Boolean = ???
  def searchForCode(mTree: NonEmptyNode): String = {
    def findCode(mTree: NonEmptyNode, acc: String, cnt: HashMap[String, Int]): String = {
      if (mTree.isEmpty) acc
      else {
    	  val 
        if (isConflict(x, acc)) searchForCode()
      }
    }
    val code = "*" * 26
    findCode(mTree, code)
  }

  def discoverCode(message: String): String = {
    // Convert input cipher message into a Set, and remove any white space in between words.
    val messageSet = message.toUpperCase.split("\\W+").filter(isLetter(_)).toSet
    // Find the word where the total length of the word divided by the total 
    // distinct letters in such word is maximum. Break ties arbitrarily.
    val m = messageSet.maxBy(x => x.length / x.distinct.length.toDouble).toString
    // Get the sorted message. 
    val messageSorted = sortWordMessage(m, messageSet)
    // Get message in a tree structure, each Node contains the cipher text, and 
    // set of potential plain word text match.
    val messageTree = getTree(messageSorted)

    println(messageTree.toString)

    println(searchForCode(messageTree))

    m
  }

  def encode(plainText: String, code: String): String = {
    plainText map { x => if (!x.isLetter) x else code(x.toUpper - 'A') }
  }
  def decode(encodedText: String, code: String): String = {
    encodedText map { x => if (!x.isLetter) x else ('A' + code.indexOf(x.toUpper)).toChar }
  }

}