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
    
    val quotes: List[String] = fileRead.getFile.map(x => x drop 2)
    
    //quotes.map(x => x.toUpperCase.filter(_.isLetter).distinct) foreach println

    // print out average distinct letters used in each quote.
    println("Average number of distinct letters used in each sentence:")
    println(quotes.map(x => x.toUpperCase.filter(_.isLetter).distinct.length).sum / quotes.length.toDouble)

    val wordsList = quotes.map(x => x.split("\\W+")).flatten.map(_.toUpperCase)
    
    // print out size of total words used. 
    println(s"Word set has ${wordsList.size} of words")
    println(s"Word set has ${wordsList.toSet.size} of distinct words")
  }

  def encode(plainText: String, code: String): String = {
    plainText map { x => if (!x.isLetter) x else code(x.toUpper - 'A') }
  }
  def decode(encodedText: String, code: String): String = {
    encodedText map { x => if (!x.isLetter) x else ('A' + code.indexOf(x.toUpper)).toChar }
  }

}