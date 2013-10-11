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
  val code3 = new Code("A**D*****G****************")
  val code4 = new Code("*B**L*****G***************")

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
    assert(headCode isConflict testCode)
    assert(code2 isConflict code3)
    assert(code3 isConflict code4)
    assert(code2 isConflict code4)
    
    assert(!(testCode isConflict testCode))
    assert(!(code1 isConflict code2))
    assert(!(emptyCode isConflict emptyCode))
  }

}