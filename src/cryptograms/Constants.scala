package cryptograms
import fileRead._
import collection.mutable.{ HashMap, MultiMap }
import Dave._
object Constants {
  //File path
  val quotesPath = "data/fortunes.txt"
  val mostUsedWordsPath = "data/mostUsedWords.txt"
  val words = "data/words.txt"
  val wikiWordsPath = "data/wikiWords.txt"
  // Used to split sentence into words array, will not word with apostrophe.
  val textSplitter = "[^\\w*']+"

  // Letter appearance frequency out of 10,000. 
  val letterBestChance = Map('e' -> 1257, 't' -> 908,
    'a' -> 800, 'o' -> 7591, 'i' -> 692, 'n' -> 690, 's' -> 634,
    'h' -> 623, 'r' -> 595, 'd' -> 431, 'l' -> 405, 'u' -> 284,
    'c' -> 257, 'm' -> 256, 'f' -> 235, 'w' -> 222, 'g' -> 198,
    'y' -> 190, 'p' -> 179, 'b' -> 153, 'v' -> 98, 'k' -> 73,
    'x' -> 17, 'j' -> 14, 'q' -> 11, 'z' -> 7)

  // Load the file that contains the 6,000 quotes and convert them into a list, 
  // one quote per element.
  val quotes: List[String] = getFile(quotesPath).map(x => x drop 2)

  // Create dictionary from the 6000 quotes.
  val dictionary = quotes.map(_.split("\\W+")).flatten.toSet.map((x: String) => x.toUpperCase)
  val dictMap = dictionary.zipAll(Set(), "", 1).toMap

  val wordsList = dictionary.filter(isLetter(_)).toList
  //   Load the top 2000 most used Englisth words into a List of string 
  //    val wordsList = getFile(mostUsedWordsPath).map(_.dropWhile(!_.isLetter).toUpperCase)

  // wiki 100,000 words
  //  val wordsList = getFile(wikiWordsPath).filter(x => x(0) != '#').filter(y =>
  //    y.foldLeft(true)((acc, ch) => acc && ((ch.toUpper.toInt >= 65 && ch.toUpper.toInt <= 90) ||
  //      ch.toInt == 39))).map((word: String) => word.toUpperCase)

  // 350,000 words.
  //    val wordsList = getFile(words).map(_.toUpperCase)

  val patternList = wordsList.map(pattern)

  // Organize the top 2000 words into sub-category. 
  val patternMap: List[(String, String)] = patternList zip wordsList

}