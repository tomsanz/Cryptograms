package cryptograms

import scala.io.Source

object fileRead {

  def getFile = {
    val file = Source.fromFile("data/fortunes.txt")
    val quotes = file.getLines.toList    
    file.close()
    quotes
  }
  
  def getWords = {
    val file = Source.fromFile("data/mostUsedWords.txt")
    val words = file.getLines.toList
    file.close()
    words
  }
}