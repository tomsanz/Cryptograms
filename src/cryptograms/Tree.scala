package cryptograms

abstract class Node

class EmptyNode extends Node {

}

class NonEmptyNode(cipherW: String, plainW: String, score: Int) extends Node

