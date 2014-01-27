import org.scalatest._

class TestFibonacci extends FunSpec with ShouldMatchers {

  describe("A Fibonacci sequence") {
    it("should have zero as its first number") {
      fibonacci(0) should be(0)
    }
    it("should have one as its second number") {
      fibonacci(1) should be(1)
    }
    it("should have 1 as its third number") {
      fibonacci(2) should be(1)
    }
    it("should have 2 as its fourth number") {
      fibonacci(3) should be(2)
    }
    it("should have 34 as its tenth number") {
      fibonacci(9) should be(34)
    }
    it("should throw an exception on negative input") {
      intercept[IndexOutOfBoundsException] {
        fibonacci(-1)
      }
    }
  }

  def fibonacci(index: Int) : Int = index match {
    case x if x < 0 => throw new IndexOutOfBoundsException
    case 0 => 0
    case 1 => 1
    case x => fibonacci(x - 1) + fibonacci(x - 2)
  }

  describe("A Fibonacci encoder") {
    it("should transform 0 into '0'") {
      fibonacciEncode(0) should be("0")
    }
    it("should transform 1 into '1'") {
      fibonacciEncode(1) should be("1")
    }
  }

  def fibonacciEncode(number: Int) : String = {
    s"$number"
  }
}
