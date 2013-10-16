package cryptograms
import fileRead._
import collection.mutable.{ HashMap, MultiMap, Set }
import Dave._
/**
 * This object contains all the values/parameters used by the Dave Object.
 */
object Constants {
  /* File path for various word databases. */
  val quotesPath = "data/fortunes.txt" // 6000 Quotes.
  val mostUsedWordsPath = "data/mostUsedWords.txt" // top 2000 most used words.
  val words = "data/words.txt" // 350,000 words from Internet.
  val wikiWordsPath = "data/wikiWords.txt" // Top 100,000 words from Wikipedia.
  val googleWordsPath = "data/google-10000-english.txt" // Top 10,000 googled English words.

  // Used to split sentence into words array, will not separate word with apostrophe.
  val textSplitter = "[^*A-Za-z]+"

  // Time out - Currently set to 1 minute (60 x 10^9 nano seconds)
  val timeOut = 60000000000L

  // Define type name for char to char multiMap
  type LetterMap = HashMap[Char, Set[Char]] with MultiMap[Char, Char]
  type CipherToPlain = (String, List[String])
  val newLetterMap = new HashMap[Char, Set[Char]] with MultiMap[Char, Char]
  def newCode = new Code("*" * 26)
  
  // Empty Set
  val emptySet = Set()

  // Load the file that contains the 6,000 quotes and convert them into a list, 
  // one quote per element.
  val quotes: List[String] = getFile(quotesPath).map(x => x drop 2)

  // Create dictionary from the 6000 quotes.
  val dictionary = quotes.map(_.split("\\W+")).flatten.toSet.map((x: String) => x.toUpperCase)
  val dictMap = dictionary.zipAll(Set(), "", 1).toMap

  //  All words from the 6,000 quotes.
  val wordsList = dictionary.filter(isLetter).toList

  /*  Optional words list. */
  //   Load the top 2000 most used Englisth words into a List of string 
  //  val wordsList = getFile(mostUsedWordsPath).map(_.dropWhile(!_.isLetter).toUpperCase)

  // google 10,000 words
  //  val wordsList = getFile(googleWordsPath).map(_.dropWhile(!_.isLetter).toUpperCase)

  // wiki 100,000 words
  //      val wordsList = getFile(wikiWordsPath).filter(x => x(0) != '#').filter(y =>
  //        y.foldLeft(true)((acc, ch) => acc && ((ch.toUpper.toInt >= 65 && ch.toUpper.toInt <= 90) ||
  //          ch.toInt == 39))).map((word: String) => word.toUpperCase)

  // 350,000 words.
  //  val wordsList = getFile(words).map(_.toUpperCase)

  val patternList = wordsList.map(getPattern)

  // Organize the top 2000 words into sub-category. 
  val patternMap: List[(String, String)] = patternList zip wordsList

}