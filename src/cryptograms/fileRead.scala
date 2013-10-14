package cryptograms

import scala.io.Source

object fileRead {

  def getFile(path: String) = {
    val file = Source.fromFile(path, "UTF-8")
    val quotes = file.getLines.toList
    file.close()
    quotes
  }
}