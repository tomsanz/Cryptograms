package cryptograms
import Constants._
import fileRead._
import collection.mutable.{ Map, MultiMap, HashMap, Set }
/**
 * CIT 591 Cryptogram Project.
 * @Authors: Thomas Yin & Lochlain Lewis
 */
object Dave {
  def main(args: Array[String]): Unit = {
    val testCode = "BCDEFGHIJKLMNOPQRSTUVWXYZA"
    val testingQuote = quotes(2301)

    val encodedMessage = encode(testingQuote, testCode)
    println(testingQuote)
    println(encodedMessage)

    val discoveredCode = discoverCode(encodedMessage)
    val decodedMessage = decode(encodedMessage, discoveredCode)

    println(discoveredCode)
    println(decodedMessage)
  }

  /**
   * Convert given string into pattern.
   * For example given cipher word "DEQGC" will return "ABCDE" as pattern text.
   */
  def getPattern(s: String) = s.foldLeft("")((res, ch) => res + (
    if (res.length != s.indexOf(ch)) 'A' + s.indexOf(ch)
    else 'A' + res.distinct.length).toChar)
  /**
   * Check if the given string contains any letter.
   */
  def isLetter(s: String) = s.forall(_.isLetter)
  /**
   * Given any pattern text, return the plain English words that match that pattern in a list.
   * Also append an empty string as the last elem in the return List.
   */
  def generateList(patternText: String) = {
    val (_, res) = patternMap.filter(_._1 == patternText).unzip
    res ::: List("")
  }
  /**
   * Given the head cipher word from a cipher sentence, and the cipher sentence,
   * return a list with the head cipher word as the first element of the list, and the remaining
   * cipher texts sorted from the most amount of distinct letters shared with head elements to least.
   */
  def sortCipherL(headCipher: String, cipherL: collection.immutable.Set[String]) = {
    def loop(s: collection.immutable.Set[String], acc: List[String]): List[String] = {
      if (s.isEmpty) acc
      else {
        // Find the elem that share the most amt of distinct letters with current List.
        val elem = s.maxBy(x => (x.toSet intersect acc.flatten.toSet).size)
        loop(s - elem, acc :+ elem)
      }
    }
    loop(cipherL - headCipher, List(headCipher))
  }
  /**
   * Check if using the given Code can convert all words in the original Cipher sentence
   * into English. Will check against dictionary made from the top 6000 quotes on whether
   * the word is English or Not.
   * @Param Code found
   * @Param original Cipher Text message.
   */
  def isAllLetters(code: Code, message: String) = {
    val decodedMessage = convertCipherTextToSet(message)
    decodedMessage.forall(x => dictMap.getOrElse(x, 0) == 1)
  }
  /**
   * Search given node and find the code that will find the most number of plain text words
   * in the cipher sentence.
   */
  def searchForCode(mTree: Node, message: String) = {
    val startTime = System.nanoTime // Start timer for time out.

    def loop(node: Node, result: Set[Code], stack: List[(Node, Code)]): Set[Code] = {
      /* Base cases: */
      // Terminate if the loop runs over 1 minute.
      if ((System.nanoTime - startTime) > 60000000000L) {
        println("Timed Out! Printing out best code found.");
        result
      } else if (node.isEmpty)
        // 1. Found best code available (latest code's length match with input sentence's distinct 
        // letter length.
        if (isAllLetters(stack.last._2, message)) { println("Best case found!"); Set(stack.last._2) }
        // 2. Iterated through all cases, and not found case No. 1, then return all Codes found
        // Will analyze for best code outside of this loop.
        else if (stack forall (_._2() == ("*" * 26))) result
        // 3. Found one Code, but have not found best code, nor have iterated all cases, then back-track
        // and continue look for all other cases.
        else loop(stack.last._1.nextNeighborNode, result + stack.last._2, stack.init)
      else if (node.code isConflict stack.last._2)
        loop(node.nextNeighborNode, result, stack)
      else loop(node.nextChildrenNode, result, stack :+ (node, node.code merge stack.last._2))
    }
    val codeSet = loop(mTree, Set(), List((EmptyNode, new Code("*" * 26))))

    codeSet.maxBy(x =>
      decode(message, x()).split(textSplitter).map(
        (elem: String) => dictMap.getOrElse(elem, 0)).sum)
  }
  /**
   * Given cipher text, and letter map showing allowed cipher letter to plain letter mapping,
   * return list of plain words that satisfy the letter map. An empty String will also be appended
   * to end of the list.
   */
  def getPlainWords(cipherW: String,
    letterMap: HashMap[Char, Set[Char]] with MultiMap[Char, Char]) = {
    val patternText = getPattern(cipherW)
    def isConsistent(plainW: String): Boolean =
      if (plainW.isEmpty) true
      else (0 until cipherW.length).foldLeft(true)((acc, i) => acc && letterMap(cipherW(i))(plainW(i)))
    generateList(patternText).filter(isConsistent).distinct
  }

  /**
   * Generate a tree which connects all relevant plain text words.
   */
  def createTree(message: List[String],
    wordsMap: HashMap[Char, Set[Char]] with MultiMap[Char, Char]): Node = {

    def loop(cipherL: List[String], plainTextL: List[String],
      acc: Node, childNode: Node): Node = {
      if (plainTextL.isEmpty && cipherL.size == 1) acc
      else if (plainTextL.isEmpty) {
        val newCipherL = cipherL.init
        val newPlainText = getPlainWords(newCipherL.last, wordsMap) // generateList(pattern(newCipher.last))
        loop(newCipherL, newPlainText.init,
          Node(newCipherL.last, newPlainText.last, EmptyNode, acc), acc)
      } else loop(cipherL, plainTextL.init,
        Node(cipherL.last, plainTextL.last, acc, childNode), childNode)
    }
    loop(message, getPlainWords(message.last, wordsMap), EmptyNode, EmptyNode)
  }
  /**
   * Create mapping between cipher letter to plain letter.
   */
  def getLetterSet(wordPair: (String, List[String])) = {
    val tempMap = new HashMap[Char, Set[Char]] with MultiMap[Char, Char]
    for (i <- 0 until wordPair._1.length) {
      for (word <- wordPair._2; if !word.isEmpty) {
        tempMap.addBinding(wordPair._1(i), word(i))
      }
    }
    tempMap
  }

  def mergeMap(map1: HashMap[Char, Set[Char]] with MultiMap[Char, Char],
    map2: HashMap[Char, Set[Char]] with MultiMap[Char, Char]) = {
    val tempMap = new HashMap[Char, Set[Char]] with MultiMap[Char, Char]
    val keys = map1.keySet union map2.keySet
    for (key <- keys) {
      val values = if (map1.getOrElse(key, Set()).isEmpty) map2.getOrElse(key, Set())
      else if (map2.getOrElse(key, Set()).isEmpty) map1.getOrElse(key, Set())
      else map1.getOrElse(key, Set()) intersect map2.getOrElse(key, Set())
      for (v <- values) tempMap.addBinding(key, v)
    }
    tempMap
  }
  def findWordsMap(cipherSentence: List[String]) = {
    // List of cipher text sentence converted to list of pattern text
    val patternList: List[String] = cipherSentence.map(getPattern)
    // Generate list of matching plain words for each pattern text
    val matchingPlainWords = patternList.map(generateList)
    // Group cipher text, cipher text's pattern text and list of plain words matching the pattern text
    // into a three elements-tuple.
    val wordsPattern = cipherSentence zip matchingPlainWords
    // Create mapping between cipher letter to possible plain text letter
    def loop(wordsPattern: List[(String, List[String])],
      acc: HashMap[Char, Set[Char]] with MultiMap[Char, Char]): HashMap[Char, Set[Char]] with MultiMap[Char, Char] = {
      if (wordsPattern.isEmpty) acc
      else loop(wordsPattern.tail, mergeMap(acc, getLetterSet(wordsPattern.head)))
    }
    loop(wordsPattern, new HashMap[Char, Set[Char]] with MultiMap[Char, Char])
  }

  def printAllTree(node: Node): Unit = {
    println(node)
    if (node.nextNeighborNode.isEmpty && node.nextChildrenNode.isEmpty) println("All nodes printed.")
    else if (node.nextNeighborNode.isEmpty) printAllTree(node.nextChildrenNode)
    else if (node.nextChildrenNode.isEmpty) printAllTree(node.nextNeighborNode)
    else printAllTree(node.nextNeighborNode)
  }
  /**
   * Find the code used to decipher the given encrypted text.
   */
  def discoverCode(message: String): String = {
    println("WARNING! Program may run up to 1 minute.")
    // Convert input cipher message into a Set, and remove any white space in between words.
    val messageSet = convertCipherTextToSet(message)
    // Find the word where the total length of the word divided by the total 
    // distinct letters in such word is maximum. Break ties arbitrarily.
    val firstWord = messageSet.maxBy(x => x.length / x.distinct.length.toDouble).toString
    // Get the sorted message into a List
    val messageSorted = sortCipherL(firstWord, messageSet)
    // Get new wordMap based on cipherText patterns.
    val wordsMap = findWordsMap(messageSorted)
    // Get message in a tree structure, each Node contains the cipher text, and 
    // set of potential plain word text match.
    val messageTree = createTree(messageSorted, wordsMap)
    printAllTree(messageTree)
    searchForCode(messageTree, message)()
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
      if (!x.isLetter) x
      else if (code.indexOf(x.toUpper) == -1) '*' // Can't find cipher letter in code
      else ('A' + code.indexOf(x.toUpper)).toChar
    }
  }
  /**
   * Convert given cipher sentence by splitting the string into array of string and
   * filter out any non letter word. Finally, return the the array in a set to remove any
   * duplicate word.
   */
  private def convertCipherTextToSet(message: String): scala.collection.immutable.Set[String] = {
    message.toUpperCase.split(textSplitter).filter(isLetter).toSet
  }
}