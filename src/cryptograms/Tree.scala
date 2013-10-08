package cryptograms

class CipherWord(val cipherW: String, val plainW: String, val score: Int)

abstract class Node

class EmptyNode extends Node {}

class Root extends Node {}

class NonEmptyNode() extends Node

