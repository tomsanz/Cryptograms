package cryptograms

case class Node(
  val cipherW: String, val plainW: String,
  val nextNeighborNode: Node, val nextChildrenNode: Node) {
  override def toString = "My cipher text is: " + cipherW + ", and my plain text is: " + plainW + "."
  def this(cipherW: String, plainW: String) = this(cipherW, plainW, EmptyNode, EmptyNode)
  def isEmpty = false
  /**
   * Return a code with cipherWord and plainWord.
   */
  def code: Code = {
    var tempList = "*" * 26
    if (plainW.length == 0) new Code(tempList)
    else {
      for (i <- 0 until cipherW.length) {
        val plainAssign = (plainW(i) - 'A') min 26
        val (first, last) = tempList.splitAt(plainAssign)
        tempList =
          if (plainAssign > 25) first + cipherW(i)
          else first + cipherW(i) + last.tail
      }
      new Code(tempList)
    }
  }
}

object EmptyNode extends Node("Empty Cipher Word", "Empty Plain Word") {
  override def toString = ""
  override def isEmpty = true
}

class Code(val code: String) {
  /**
   * Check if the given pair of code has any of the two conflicts:
   * 1. If any letter got assigned to two different letter code
   * 2. If any two letters got assigned to the same letter.
   */
  def isConflict(that: Code) = {
    that().foldLeft(false)((res, ch) =>
      // if that ch is a letter AND this code contains ch, AND this ch's position is different than that ch's position   
      res || ((ch.isLetter && this().contains(ch) && this().indexOf(ch) != that().indexOf(ch)) ||
        // if that ch is a letter, AND at that's position in this, there is a letter, AND the letters are different
        (ch.isLetter && this()(that().indexOf(ch)).isLetter && ch != this()(that().indexOf(ch)))))
  }
  def apply() = code

  def merge(that: Code): Code = {
    var result = ""
    for (i <- 0 until 26) {
      if (that()(i).isLetter)
        result += that()(i)
      else {
        result += this()(i)
      }
    }
    new Code(result)
  }
}

class CipherWordSet(val cipherW: String, val plainW: List[String]) {
  override def toString = "cipherWord is " + cipherW + ", and plain Word is: " + plainW
  def zippedSet = Set(cipherW).zipAll(plainW, cipherW, "")
}
