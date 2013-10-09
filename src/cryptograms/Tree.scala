package cryptograms

case class Node(
  val cipherW: String,
  val plainW: String,
  neighorNode: Node,
  childrenNode: Node) {
  def nextNeighborNode = neighorNode
  def nextChildrenNode = childrenNode
  override def toString = "My cipher text is: " + cipherW + ", and my plain text is: " + plainW + ".s"    
  def this(cipherW: String, plainW: String) = this(cipherW, plainW, EmptyNode, EmptyNode)
}

object EmptyNode extends Node("Empty Cipher Word", "Empty Plain Word")

class CipherWordSet(val cipherW: String, val plainW: List[String]) {
  override def toString = "cipherWord is " + cipherW + ", and plain Word is: " + plainW
  def zippedSet = Set(cipherW).zipAll(plainW, cipherW, "")
}

//abstract class Node {
//  def include(c: CipherWordSet): Node
//  def isEmpty: Boolean
//  def remove: Node
//  def cWord: CipherWordSet
////  def parent: Node
//}
//
//object EmptyNode extends Node {
//  def include(c: CipherWordSet) = new NonEmptyNode(c, EmptyNode)
//  def isEmpty = true
//  def remove: Node = EmptyNode
//  def cWord = new CipherWordSet("", List(""))
////  def parent = EmptyNode
//}
//
//class NonEmptyNode(cipherWordSet: CipherWordSet, nextNode: Node) extends Node {
//  def isEmpty = false
//  def include(cWord: CipherWordSet): Node = new NonEmptyNode(cWord, this)
//  override def toString = cipherWordSet.toString + "\n" + nextNode.toString
//  def remove = nextNode
//  def cWord = cipherWordSet
////  def parent = EmptyNode
//}

