package cryptograms

object Dave {

  def main(args: Array[String]): Unit = {

    //    val testCode = "BCDEFGHIJKLMNOPQRSTUVWXYZA"
//    println(Constants.letterBestChance.keys)
    //
    //    var test = true
    //
    //    while (test) {
    //      println("Enter something, and i'll translate it into gibberish.")
    //      val i = readLine()
    //      val j = encode(i, testCode)
    //      println("Encoded message is: " + j)
    //      println("Decoded message is: " + decode(j, testCode))
    //    }
    val quotes = fileRead.getFile.map(x => x drop 2)
    quotes.map(x => x.toUpperCase.filter(_.isLetter).distinct) foreach println
    println(quotes.map(x => x.toUpperCase.filter(_.isLetter).distinct.length).sum/quotes.length.toDouble)
  }

  def encode(plainText: String, code: String): String = {
    plainText map { x => if (!x.isLetter) x else code(x.toUpper - 'A') }
  }
  def decode(encodedText: String, code: String): String = {
    encodedText map { x => if (!x.isLetter) x else ('A' + code.indexOf(x.toUpper)).toChar }
  }

}