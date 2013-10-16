package cryptograms
/**
 * CIT 591 Cryptogram Project.
 * @Authors: Thomas Yin & Lochlain Lewis
 */

import scala.io.Source

object fileRead {
  def getFile(path: String) = {
    val file = Source.fromFile(path)
    val quotes = file.getLines.toList
    file.close()
    quotes
  }
}