package cryptograms

import Constants._
import fileRead._
import collection.mutable.{ Map, MultiMap }

object Dave {
  def main(args: Array[String]): Unit = {
    val testCode = "BCDEFGHIJKLMNOPQRSTUVWXYZA"
    val testingQuote = quotes(0)

    val encodedMessage = encode(testingQuote, testCode)
    val discoveredCode = discoverCode(encodedMessage)
    //    val decodedMessage = decode(encodedMessage, discoveredCode)
    //
    println(testingQuote)
    println(encodedMessage)
    //    println(decodedMessage)
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

  /*def getTree(sList: List[String]): Node = {
    def buildTree(sList: List[String], acc: Node): Node =
      if (sList.isEmpty) acc
      else buildTree(sList.init, acc.include(new CipherWordSet(sList.last,
        generateList(pattern(sList.last)))))
    buildTree(sList, EmptyNode)
  }*/

  def sortWordMessage(m: String, message: Set[String]) = {
    def f(s: Set[String], acc: List[String]): List[String] = {
      if (s.isEmpty) acc else {
        val temp = s.maxBy(x => (x.toSet intersect acc.flatten.toSet).size)
        f(s - temp, acc :+ temp)
      }
    }
    f(message - m, List(m))
  }
  /**
   * Check if the given pair of code has any of the two conflicts:
   * 1. If any letter got assigned to two different letter code
   * 2. If any two letters got assigned to the same letter.
   */
  def isConflict(newCode: String, oldCode: String): Boolean = {
    newCode.foldLeft(false)((res, y) =>
      res || ((y.isLetter && oldCode.contains(y)) ||
        (y.isLetter && oldCode(newCode.indexOf(y)).isLetter)))
  }

  def getCode(cipher: String, plain: String) = {
    var tempList = "*" * 26
    if (plain.length == 0) tempList
    else {
      for (i <- 0 until cipher.length) {
        val plainAssign = plain(i) - 'A'
        val (first, last) = tempList.splitAt(plainAssign)
        tempList = first + cipher(i) + last.tail
      }
      tempList
    }
  }
  def merge(newCode: String, oldCode: String): String = {
    var result = ""
    for (i <- 0 until 26) {
      if (newCode(i).isLetter)
        result += newCode(i)
      else {
        result += oldCode(i)
      }
    }
    result
  }

  /*def resetCnt(pNode: Node, cnt: Map[String, Int]) = ???

  def searchForCode(mTree: Node, traverseCnt: Map[String, Int]): String = {
    def findCode(mTree: Node, acc: String, cnt: Map[String, Int]): String = {
      if (mTree.isEmpty) { acc
        val nDistinctLetters = cnt.keySet.flatten.size
        if (nDistinctLetters == acc.filter(_ != '*').size) acc
        else {
          // Resetting counter to previous position, and start the loop again.
          val rCnt = resetCnt(mTree.parent, cnt)

          // Restart loop. Need to change acc to previous position.
          findCode(mTree.parent, acc, rCnt)
        }
      } else {
        println(acc)
        val currentCipherWord = mTree.cWord.cipherW
        val currentPos = cnt(currentCipherWord)
        val currentPlainWord = mTree.cWord.plainW(currentPos)
        println("Current cipherword: " + currentCipherWord + ", and current plain word is: " + currentPlainWord)
        val tempCode = getCode(currentCipherWord, currentPlainWord)

        if (isConflict(tempCode, acc)) findCode(mTree, acc, (
          cnt + (currentCipherWord -> (cnt.getOrElse(currentCipherWord, 0) + 1))))
        else findCode(mTree.remove, merge(acc, tempCode), cnt)
      }
    }
    findCode(mTree, "*" * 26, traverseCnt)
  }*/

  def createTree(message: List[String]): Node = {
    def loop(cipher: List[String],
      plainText: List[String],
      acc: Node, childNode: Node): Node = {
      if (plainText.isEmpty && cipher.size == 1) acc
      else if (plainText.isEmpty) {
        val newCipher = cipher.init
        val newPlainText = generateList(pattern(newCipher.last))
        loop(newCipher, newPlainText.init,
          Node(newCipher.last, newPlainText.last, EmptyNode, acc), childNode)
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
    // Find the word where the total length of the word divided by the total 
    // distinct letters in such word is maximum. Break ties arbitrarily.
    val firstWord = messageSet.maxBy(x => x.length / x.distinct.length.toDouble).toString
    // Get the sorted message into a List
    val messageSorted = sortWordMessage(firstWord, messageSet)

    /* print out how many children nodes we would need if we create the full tree */
    //    val tempInt = messageSorted.foldLeft(1L)((x,y) => {
    //      print(generateList(pattern(y)).size + " * ")
    //      x * (generateList(pattern(y)).size)})
    //    println(s"Potential list size is: " + tempInt)
    //    println(messageSorted)
    val messageTree = createTree(messageSorted)
    println(messageTree.toString)
    // Create a counter which will keep track of which Node has been visited.
    /*    val traverseCnt = messageSorted.foldLeft(Map[String, Int]())(
      (m, s) => m + (s -> 0))*/
    // Get message in a tree structure, each Node contains the cipher text, and 
    // set of potential plain word text match.
    //    val messageTree = getTree(messageSorted)

    //    searchForCode(messageTree, traverseCnt)
    message
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
    encodedText map { x => if (!x.isLetter) x else ('A' + code.indexOf(x.toUpper)).toChar }
  }

}