package cryptograms
import Constants._
import fileRead._
import scala.util.Random
/**
 * CIT 591 Cryptogram Project.
 * @Authors: Thomas Yin & Lochlain Lewis
 */
object Dave {
  def main(args: Array[String]): Unit = {
    val testCode = "BCDEFGHIJKLMNOPQRSTUVWXYZA"
    //        val testingQuote = quotes(550)
    val r = Random.nextInt(quotes.size)
    println(s"Running quote number: ${r.toString}")
    val testingQuote = quotes(r)

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
  def sortCipherL(headCipher: String, cipherL: Set[String]) = {
    def loop(s: Set[String], acc: List[String]): List[String] = {
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
    //    println(message)
    //    println(decode(message, code()))
    val decodedMessage = getStringFromSentence(decode(message, code()))
    //    println(decodedMessage)
    decodedMessage.forall(x => dictMap.getOrElse(x, 0) == 1)
  }
  /**
   * Search given node and find the code that will find the most number of plain text words
   * in the cipher sentence.
   */
  def searchForCode(mTree: Node, message: String) = {
    val startTime = System.nanoTime // Start timer for time out.
    def loop(node: Node, result: Set[Code], stack: List[(Node, Code)]): Set[Code] = {
      //      println("evaluating node: " + node.toString)
      //      println("mTree is empty? " + node.isEmpty)
      /* Base cases: */
      // Terminate if the loop runs over 1 minute.
      if ((System.nanoTime - startTime) > timeOut) {
        println("Timed Out! Printing out best code found.");
        result
      } else if (node.isEmpty)
        // 1. Found best code available (latest code's length match with input sentence's distinct 
        // letter length.
        if (isAllLetters(stack.last._2, message)) { println("Best case found!"); Set(stack.last._2) }
        // 2. Iterated through all cases, and not found case No. 1, then return all Codes found
        // Will analyze for best code outside of this loop.
        else if (stack forall (_._2() == ("*" * 26))) {
          println("All options evaluated, no best search found."); result
        } // 3. Found one Code, but have not found best code, nor have iterated all cases, then back-track
        // and continue look for all other cases.
        else loop(stack.last._1.nextNeighborNode, result + stack.last._2, stack.init)
      else if (node.code isConflict stack.last._2)
        loop(node.nextNeighborNode, result, stack)
      else loop(node.nextChildrenNode, result, stack :+ (node, node.code merge stack.last._2))
    }
    val codeSet = loop(mTree, Set(), List((EmptyNode, newCode)))
    //    println("Printing codeSet:")
    //    codeSet foreach println
    codeSet.maxBy(x =>
      decode(message, x()).split(textSplitter).map(
        (elem: String) => dictMap.getOrElse(elem, 0)).sum)
  }
  /**
   * Given cipher text, and letter map showing allowed cipher letter to plain letter mapping,
   * return list of plain words that satisfy the letter map. An empty String will also be appended
   * to end of the list.
   */
  def getPlainWords(cipherW: String, letterMap: LetterMap) = {
    val patternText = getPattern(cipherW)
    def isConsistent(plainW: String): Boolean =
      if (plainW.isEmpty) true
      else (0 until cipherW.length).foldLeft(true)(
        (acc, i) => acc && letterMap(cipherW(i))(plainW(i)))
    generateList(patternText).filter(isConsistent).distinct
  }

  /**
   * Generate a tree which connects all relevant plain text words.
   */
  def createTree(message: List[String], wordsMap: LetterMap): Node = {
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
  def getCipherMap(wordPair: CipherToPlain) = {
    val resultLetterMap = newLetterMap
    val cipherW = wordPair._1
    val plainWordList = wordPair._2
    for (i <- 0 until cipherW.length) yield {
      for (word <- plainWordList; if !word.isEmpty) {
        resultLetterMap.addBinding(cipherW(i), word(i))
      }
    }
    //    println("Current Cipher Text and its letter mappings: " + cipherW)
    //    resultLetterMap foreach println
    //    println
    //    println("Current plain word mappings are: ")
    //    println(plainWordList)
    resultLetterMap
  }
  /**
   * Merge two cipher letter to plain letter maps, taking only plain letter
   * shared in common between the two maps.
   */
  def mergeMap(map1: LetterMap, map2: LetterMap) = {
    val resultMap = newLetterMap
    val keys = map1.keySet union map2.keySet
    for (key <- keys) {
      val values =
        if (map1.getOrElse(key, emptySet).isEmpty) map2(key)
        else if (map2.getOrElse(key, emptySet).isEmpty) map1(key)
        else map1(key) intersect map2(key)
      for (v <- values) resultMap.addBinding(key, v)
    }
    //    println("Current result map contains:")
    //    resultMap foreach println
    //    println
    resultMap
  }
  /**
   * Given wordsPattern (tuple with cipher word, and possible plain word),
   * filter out any plain word that does not meet the given HashMap.
   */
  def getFilteredPair(wordsPattern: CipherToPlain, acc: LetterMap) = {
    val (cipherW, plainWordList) = wordsPattern
    val filteredPlainWordList = plainWordList.filter((plainW: String) =>
      plainW.forall((ch: Char) => {
        val pos = plainW.indexOf(ch)
        val cipherCh = cipherW(pos)
        acc(cipherCh)(ch)
      }))
    (cipherW, filteredPlainWordList)
  }
  /**
   * Given a list of cipher string, return the possible mapping between cipher letter to plain
   * letter, based on given dictionary.
   */
  def findWordsMap(cipherSentence: List[String]) = {
    // List of cipher text sentence converted to list of pattern text
    // Generate list of matching plain words for each pattern text
    val matchingPlainWords = cipherSentence.map(getPattern).map(generateList)
    // Group cipher text and list of plain words matching the pattern text
    // into a two elements-tuple.
    val wordsPattern = cipherSentence zip matchingPlainWords
    // Create mapping between cipher letter to possible plain text letter
    def loop(wordsPattern: List[CipherToPlain], acc: LetterMap): LetterMap = {
      if (wordsPattern.isEmpty) acc
      else {
        val mergedM = mergeMap(acc, getCipherMap(wordsPattern.head)) // initial merged map
        val filteredPair = getFilteredPair(wordsPattern.head, mergedM)
        loop(wordsPattern.tail, mergeMap(acc, getCipherMap(filteredPair)))
      }
    }
    loop(wordsPattern, newLetterMap)
  }
  /**
   *  Print everything in the node.
   */
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
    // Remove any non-Letter words. 
    val messageSet = getStringFromSentence(message).filter(isLetter)
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
    //    printAllTree(messageTree)
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
   * Convert given cipher sentence by splitting the string into array of string.
   * Finally, return the the array in a set to remove any duplicate word.
   */
  private def getStringFromSentence(message: String): Set[String] = {
    message.toUpperCase.split(textSplitter).toSet
  }
}