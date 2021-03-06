package cryptograms
/**
 * CIT 591 Cryptogram Project.
 * @Authors: Thomas Yin & Lochlain Lewis
 */
import fileRead._
import collection.mutable.{ HashMap, MultiMap, Set }
import Dave._
import Common._
/**
 * This object contains all the values/parameters used by the Dave Object.
 */
object Constants {
  /* File path for various word databases. */
  val quotesPath = "data/fortunes.txt" // 9193 Quotes.
  /*  val mostUsedWordsPath = "data/mostUsedWords.txt" // top 2000 most used words.
  val words = "data/words.txt" // 350,000 words from Internet.
  val wikiWordsPath = "data/wiki100k.txt" // Top 100,000 words from Wikipedia.*/
  val googleWordsPath = "data/google-10000-english.txt" // Top 10,000 googled English words. 
  //    val tinyWordsPath = "data/ed-tiny.txt"

  // Used to split sentence into words array, will not separate word with apostrophe.
  val textSplitter = "[^*A-Za-z']+"

  // Time out - Currently set to 1 minute (60 x 10^9 nano seconds)
  val timeOut = 60000000000L

  val aToZ = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  // Define type alias
  type LetterMap = HashMap[Char, Set[Char]] with MultiMap[Char, Char]
  type CipherToPlain = (String, List[String])
  val newLetterMap = new HashMap[Char, Set[Char]] with MultiMap[Char, Char]
  def newCode = new Code("*" * 26)
  val emptySet = Set()

  // Load the file that contains the 6,000 quotes and convert them into a list, 
  // one quote per element.
  val quotes: List[String] = getFile(quotesPath).map(_ drop 2)

  // Create dictionary using the 6000 quotes.
  val dictionary = quotes.map(_.split("\\W+")).flatten.toSet.map((x: String) => x.toUpperCase)
  val dictMap = dictionary.zipAll(Set(), "", 1).toMap

  val wordsList = getFile(googleWordsPath).map(_.dropWhile(!_.isLetter).toUpperCase)
  val patternList = wordsList.map(getPattern)

  // Organize the words into sub-category. 
  val patternMap: List[(String, String)] = patternList zip wordsList
}