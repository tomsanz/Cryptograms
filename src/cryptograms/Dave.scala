package cryptograms
/**
 * CIT 591 Cryptogram Project.
 * @Authors: Thomas Yin & Lochlain Lewis
 */
object Dave {
  def main(args: Array[String]): Unit = {
  }

  /**
   * Find the code used to decipher the given encrypted text.
   */
  def discoverCode(message: String): String = DiscoverCode(message)
  /**
   * Given plain English text, returns a encoded message using the provided code.
   */
  def encode(plainText: String, code: String): String = {
    plainText map { x => if (!x.isLetter) x else code(x.toUpper - 'A') }
  }
  /**
   * Given encoded text, decode using the code provided and return a plain English text.
   */
  def decode(encodedText: String, code: String): String = {
    encodedText map { x =>
      if (!x.isLetter) x
      else if (code.indexOf(x.toUpper) == -1) '*' // Can't find cipher letter in code
      else ('A' + code.indexOf(x.toUpper)).toChar
    }
  }

}