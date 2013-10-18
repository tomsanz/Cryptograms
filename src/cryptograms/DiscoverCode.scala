package cryptograms
import Common._
import Constants._
/**
 * Discover Code method.
 */
object DiscoverCode {
  def apply(message: String) = {
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
    fillBlank(searchForCode(messageTree, message))
  }

  def fillBlank(code: Code): String = {
    val currentCode = code()
    currentCode.foldLeft("")((acc: String, x: Char) => acc +
      (if (x.isLetter) x
      else aToZ.filter((y: Char) => !acc.contains(y)).head))
  }

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
    val decodedMessage = getStringFromSentence(Dave.decode(message, code()))
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
    val codeFound =
      codeSet.maxBy(x =>
        Dave.decode(message, x()).split(textSplitter).map(
          (elem: String) => dictMap.getOrElse(elem, 0)).sum)

    println(codeFound())
    codeFound
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
        (acc, i) => acc && letterMap.getOrElse(cipherW(i), Set[Char]())(plainW(i)))
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
   * Convert given cipher sentence by splitting the string into array of string.
   * Finally, return the the array in a set to remove any duplicate word.
   */
  private def getStringFromSentence(message: String): Set[String] = {
    message.toUpperCase.split(textSplitter).toSet
  }

}