package cryptograms

import scala.io.Source

object fileRead {

  def getFile(path: String) = {
    val file = Source.fromFile(path)
    val quotes = file.getLines.toList
    file.close()
    quotes
  }
}