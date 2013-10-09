package cryptograms

class CipherWordSet(val cipherW: String, val plainW: List[String]) {
  override def toString = "cipherWord is " + cipherW + ", and plain Word is: " + plainW
  def zippedSet = Set(cipherW).zipAll(plainW, cipherW, "")
}

abstract class Node {
  def include(c: CipherWordSet): Node
  def isEmpty: Boolean
  def remove: Node
  def cWord: CipherWordSet
//  def parent: Node
}

object EmptyNode extends Node {
  def include(c: CipherWordSet) = new NonEmptyNode(c, EmptyNode)
  def isEmpty = true
  def remove: Node = EmptyNode
  def cWord = new CipherWordSet("", List(""))
//  def parent = EmptyNode
}

class NonEmptyNode(cipherWordSet: CipherWordSet, nextNode: Node) extends Node {
  def isEmpty = false
  def include(cWord: CipherWordSet): Node = new NonEmptyNode(cWord, this)
  override def toString = cipherWordSet.toString + "\n" + nextNode.toString
  def remove = nextNode
  def cWord = cipherWordSet
//  def parent = EmptyNode
}

