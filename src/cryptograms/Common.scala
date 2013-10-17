package cryptograms
/**
 * Common helper methods used by all files.
 */
object Common {
  
  /**
   * Convert given string into pattern.
   * For example given cipher word "DEQGC" will return "ABCDE" as pattern text.
   */
  def getPattern(s: String) = s.foldLeft("")((res, ch) => res + (
    if (res.length != s.indexOf(ch)) 'A' + s.indexOf(ch)
    else 'A' + res.distinct.length).toChar)
    
  /**
   * Check if the given string contains any letter.
   */
  def isLetter(s: String) = s.forall(_.isLetter)
  
  
  /**
   *  Print everything in the node.
   */
  def printAllTree(node: Node): Unit = {
    println(node)
    if (node.nextNeighborNode.isEmpty && node.nextChildrenNode.isEmpty) println("All nodes printed.")
    else if (node.nextNeighborNode.isEmpty) printAllTree(node.nextChildrenNode)
    else if (node.nextChildrenNode.isEmpty) printAllTree(node.nextNeighborNode)
    else printAllTree(node.nextNeighborNode)
  }
}