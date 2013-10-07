package test
import cryptograms.Dave
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class SetSuite extends FunSuite {
  val testCode = "BCDEFGHIJKLMNOPQRSTUVWXYZA"

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
  }
  
  test("discoverCode") {
    assert(Dave.discoverCode("Hello World!") == "HELLO")
  }

}