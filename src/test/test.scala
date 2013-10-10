package test
import cryptograms.Dave
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class SetSuite extends FunSuite {
  val testCode = "BCDEFGHIJKLMNOPQRSTUVWXYZA"
  val emptyCode = "*" * 26
  val headCode = 'C' +: emptyCode.tail

  test("test Encoding") {
    assert(Dave.encode("Hello", testCode) === "IFMMP")
  }

  test("test Decoding") {
    assert(Dave.decode("IFMMP", testCode) === "HELLO")
    assert(Dave.decode("IFMMP", testCode) != "Hello")
  }

  test("test pattern matching") {
    assert(Dave.pattern("this") == "ABCD")
    assert(Dave.pattern("that") == "ABCA")
    assert(Dave.pattern("ttat") == "AACA")
  }

  test("test isConflict") {
    assert(!Dave.isConflict(emptyCode, emptyCode))
    assert(Dave.isConflict(headCode, testCode))
    assert(!Dave.isConflict("***I*******U**B*****S*F***", "*A*******G****************"))
  }

}