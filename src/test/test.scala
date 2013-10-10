package test
import cryptograms.Dave
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import cryptograms.Code

@RunWith(classOf[JUnitRunner])
class SetSuite extends FunSuite {
  val testCode = new Code("BCDEFGHIJKLMNOPQRSTUVWXYZA")
  val emptyCode = new Code("*" * 26)
  val headCode = new Code("C" + "*" * 25)
  val code1 = new Code("***I*******U**B*****S*F***")
  val code2 = new Code("*A*******G****************")

  test("test Encoding") {
    assert(Dave.encode("Hello", testCode()) === "IFMMP")
  }

  test("test Decoding") {
    assert(Dave.decode("IFMMP", testCode.code) === "HELLO")
    assert(Dave.decode("IFMMP", testCode.code) != "Hello")
  }

  test("test pattern matching") {
    assert(Dave.pattern("this") == "ABCD")
    assert(Dave.pattern("that") == "ABCA")
    assert(Dave.pattern("ttat") == "AACA")
  }

  test("test isConflict") {
    assert(!(emptyCode isConflict emptyCode))
    assert(headCode isConflict testCode)
    assert(!(testCode isConflict testCode))
    assert(!(code1 isConflict code2))
  }

}