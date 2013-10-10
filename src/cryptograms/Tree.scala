package cryptograms

case class Node(
  val cipherW: String,
  val plainW: String,
  neighorNode: Node,
  childrenNode: Node) {
  def nextNeighborNode = neighorNode
  def nextChildrenNode = childrenNode
  override def toString = "My cipher text is: " + cipherW + ", and my plain text is: " + plainW + ".\n" +
    this.neighorNode.toString + this.childrenNode.toString
  def this(cipherW: String, plainW: String) = this(cipherW, plainW, EmptyNode, EmptyNode)
}

object EmptyNode extends Node("Empty Cipher Word", "Empty Plain Word") {
  override def toString = ""
}

class CipherWordSet(val cipherW: String, val plainW: List[String]) {
  override def toString = "cipherWord is " + cipherW + ", and plain Word is: " + plainW
  def zippedSet = Set(cipherW).zipAll(plainW, cipherW, "")
}
