package cryptograms

import fileRead._
import collection.mutable.{ HashMap, MultiMap }
import Dave._

object Constants {
  //File path
  val quotesPath = "data/fortunes.txt"
  val mostUsedWordsPath = "data/mostUsedWords.txt"

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
  // Load the top 2000 most used Englisth words into a List of string 
  val words = getFile(mostUsedWordsPath).map(_.dropWhile(!_.isLetter).toUpperCase)
  // Organize the top 2000 words into sub-category. 
  val patternMap = words.foldLeft(
    new HashMap[String, collection.mutable.Set[String]]() with MultiMap[String, String])(
      (m, c) => m.addBinding(pattern(c), c))

}