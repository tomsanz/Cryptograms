package test
import cryptograms._
import cryptograms.Common._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import cryptograms.Code
import collection.mutable.{ Map, MultiMap, HashMap, Set }

@RunWith(classOf[JUnitRunner])
class SetSuite extends FunSuite {
  val testCode = new Code("BCDEFGHIJKLMNOPQRSTUVWXYZA")
  val emptyCode = new Code("*" * 26)
  val headCode = new Code("C" + "*" * 25)
  val code1 = new Code("***I*******U**B*****S*F***")
  val code2 = new Code("*A*******G****************")
  val code3 = new Code("A**D*****G****************")
  val code4 = new Code("*B**L*****G***************")
  val map1, map2, map3, map4 = new HashMap[Char, Set[Char]] with MultiMap[Char, Char]

  map1 += (('A', Set('D', 'E', 'F')), ('B', Set('E', 'F', 'C', 'D')),
    ('D', Set('T', 'G', 'H')), ('A', Set('H', 'I', 'G')), ('X', Set('G', 'A')))
  map2 += (('A', Set('G', 'H', 'F')), ('B', Set('Z', 'D', 'F', 'D')))
  map3 += (('A', Set('F')), ('B', Set('F', 'D')))
  map4 += (('A', Set('D', 'E', 'F')), ('B', Set('E', 'F', 'C', 'D')))

  test("test Encoding") {
    assert(Dave.encode("Hello", testCode()) === "IFMMP")
  }

  test("test Decoding") {
    assert(Dave.decode("IFMMP", testCode.code) === "HELLO")
    assert(Dave.decode("IFMMP", testCode.code) != "Hello")
  }

  test("test pattern matching") {
    assert(getPattern("this") == "ABCD")
    assert(getPattern("that") == "ABCA")
    assert(getPattern("ttat") == "AABA")
    assert(getPattern("HELP") == "ABCD")
    assert(getPattern("A") == "A")
    assert(getPattern("G") == "A")
    assert(getPattern("GG") == "AA")
    assert(getPattern("THAT") == "ABCA")
    assert(getPattern("THAHTA") == "ABCBAC")
  }

  test("test isConflict") {
    assert(headCode isConflict testCode)
    assert(code2 isConflict code3)
    assert(code3 isConflict code4)
    assert(code2 isConflict code4)

    assert(!(testCode isConflict testCode))
    assert(!(code1 isConflict code2))
    assert(!(emptyCode isConflict emptyCode))
  }

  test("mergeMap") {
    assert(DiscoverCode.mergeMap(map4, map2) == map3)
    assert(DiscoverCode.mergeMap(map4, map2) != map2)
  }

  test("getPlainWords") {
    assert(DiscoverCode.getPlainWords("DAXD", map1) == List("THAT", "HIGH", ""))
  }

}