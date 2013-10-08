package cryptograms

class CipherWord(val cipherW: String, val plainW: String, val score: Int) {
  override def toString = "cipherWord is " + cipherW + ", and plain Word is: " + plainW
}

abstract class Node {
  def include(c: CipherWord): Node
  def isEmpty: Boolean
}

class EmptyNode extends Node {
  def include(c: CipherWord) = new NonEmptyNode(c, new EmptyNode)
  def isEmpty = true
}

class Root extends Node {
  def isEmpty = false
  def include(c: CipherWord): Node = ???
}

class NonEmptyNode(c: CipherWord, keys: Node*) extends Node {
  def isEmpty = false
  def include(c: CipherWord): Node = ???

}

