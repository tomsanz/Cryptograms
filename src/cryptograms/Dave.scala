package cryptograms

import Constants._
import fileRead._
import collection.mutable.{ Map, MultiMap }
import scala.annotation.tailrec

object Dave {
  def main(args: Array[String]): Unit = {
    val testCode = "BCDEFGHIJKLMNOPQRSTUVWXYZA"
    val testingQuote = quotes(0)

    val encodedMessage = encode(testingQuote, testCode)
    println(testingQuote)
    println(encodedMessage)
    println("ABCDEFGHIJKLMNOPQRSTUVWXYZ")

    val discoveredCode = discoverCode(encodedMessage)
    val decodedMessage = decode(encodedMessage, discoveredCode())

    println(discoveredCode())
    println(decodedMessage)
  }

  /**
   * Convert given string into pattern.
   * For example given cipher word "DEQGC" will return "ABCDE" as pattern text.
   */
  def pattern(s: String) = s.tail.foldLeft("A")((res: String, y: Char) => res +
    (if (res.length != s.indexOf(y)) 'A' + s.indexOf(y) else 'A' + res.length).toChar)
  /**
   * Check if given string contain any letter.
   */
  def isLetter(s: String) = s.exists(_.isLetter)
  /**
   * Given any pattern text, return the plain English words that match that pattern in a list.
   * Also append an empty string as the last elem in the return List.
   */
  def generateList(patternText: String): List[String] = {
    val (_, res) = patternMap.filter(_._1 == patternText).unzip
    res ::: List("")
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

  def isBestCode(code: Code, mLetter: Int) = code().filter(_ != '*').size == mLetter

  def searchForCode(mTree: Node, maxDistinctLetters: Int): Set[Code] = {
    @tailrec
    def loop(node: Node, result: Set[Code], stack: List[(Node, Code)]): Set[Code] = {
      // Base cases:
      if (node.isEmpty)
        // 1. Found best code available (latest code's length match with input sentence's distinct 
        // letter length.
        if (isBestCode(stack.last._2, maxDistinctLetters)) Set(stack.last._2)
        // 2. Iterated through all cases, and not found case No. 1, then return all Codes found
        // Will analyze for best code outside of this loop.
        else if (stack forall (_._2() == ("*" * 26))) result
        // 3. Found one Code, but have not found best code, nor have iterated all cases, then back-track
        // and continue look for all other cases.
        else {
          loop(stack.last._1.nextNeighborNode, result + stack.last._2, stack.init)
        }
      else if (node.code isConflict stack.last._2)
        loop(node.nextNeighborNode, result, stack)
      else loop(node.nextChildrenNode, result, stack :+ (node, node.code merge stack.last._2))
    }
    loop(mTree, Set(), List((EmptyNode, new Code("*" * 26))))
  }
  /**
   * Generate a tree which connects all relevant plain text words.
   */
  def createTree(message: List[String]): Node = {
    def loop(cipher: List[String],
      plainText: List[String],
      acc: Node, childNode: Node): Node = {
      if (plainText.isEmpty && cipher.size == 1) acc
      else if (plainText.isEmpty) {
        val newCipher = cipher.init
        val newPlainText = generateList(pattern(newCipher.last))
        loop(newCipher, newPlainText.init,
          Node(newCipher.last, newPlainText.last, EmptyNode, acc), acc)
      } else loop(cipher, plainText.init,
        Node(cipher.last, plainText.last, acc, childNode), childNode)
    }
    loop(message, generateList(pattern(message.last)), EmptyNode, EmptyNode)
  }
  /**
   * Find the code used to decipher the given encrypted text.
   */
  def discoverCode(message: String) = {
    // Convert input cipher message into a Set, and remove any white space in between words.
    val messageSet = message.toUpperCase.split("\\W+").filter(isLetter(_)).toSet
    println(messageSet)
    // Find the word where the total length of the word divided by the total 
    // distinct letters in such word is maximum. Break ties arbitrarily.
    val firstWord = messageSet.maxBy(x => x.length / x.distinct.length.toDouble).toString
    // Get the sorted message into a List
    val messageSorted = sortWordMessage(firstWord, messageSet)
    // Get message in a tree structure, each Node contains the cipher text, and 
    // set of potential plain word text match.
    val messageTree = createTree(messageSorted)
    println("Start searching for codes")
    val codeSet = searchForCode(messageTree, messageSet.flatten.size)
    println("Found set of codes")

    /*
     * Find the best code by having the most amount of distinct letters in the code.
     */
    //    codeSet.maxBy((x: Code) => {
    //      //      println(x() + " " + (x().filter(ch => ch != '*')).length)
    //      (x().filter(ch => ch != '*')).length
    //    })
    codeSet.maxBy(x => {
      decodeSpecial(message, x()).split("\\W+").map(
          (elem: String) => dictMap.getOrElse(elem, 0)).sum})
  }
  /**
   * Given plain English text, returns a encoded message using the provided code.
   */
  def encode(plainText: String, code: String): String = {
    plainText map { x => if (!x.isLetter) x else code(x.toUpper - 'A') }
  }
  /**
   * Given encoded text, decode using the code provided and return a plain English text.
   */
  def decode(encodedText: String, code: String): String = {
    encodedText map { x =>
      if (!x.isLetter) x else {
        if (code.indexOf(x.toUpper) != -1)
          ('A' + code.indexOf(x.toUpper)).toChar
        else '*'
      }
    }
  }
  
  def decodeSpecial(encodedText: String, code: String): String = {
    encodedText map { x =>
      if (!x.isLetter) x else {
        if (code.indexOf(x.toUpper) != -1)
          ('A' + code.indexOf(x.toUpper)).toChar
        else 'a'
      }
    }
  }
}